namespace OpcViewer.Base.Picking

open System
open Aardvark.Base
open Aardvark.Base.Coder 
open Aardvark.Geometry  
open Aardvark.SceneGraph.Opc
open MBrace.FsPickler    
open MBrace.FsPickler.Combinators  
open Aardvark.VRVis.Opc.KdTrees



module KdTrees = 
    
  let makeInCoreKd a = 
    {
      kdTree = new ConcreteKdIntersectionTree()
      boundingBox = a
    }

  let makeLazyTree a b c d e f =
    {
      kdTree          = None
      affine          = a
      boundingBox     = b
      kdtreePath      = c
      objectSetPath   = d    
      coordinatesPath = e
      texturePath     = f
    }

  let incorePickler : Pickler<InCoreKdTree> =
    Pickler.product makeInCoreKd
    ^. Pickler.field (fun s -> s.boundingBox)     Pickler.auto<Box3d>

  let lazyPickler : Pickler<LazyKdTree> =
      Pickler.product makeLazyTree
      ^+ Pickler.field (fun (s:LazyKdTree) -> s.affine)       Pickler.auto<Trafo3d>
      ^+ Pickler.field (fun (s:LazyKdTree) -> s.boundingBox)  Pickler.auto<Box3d>
      ^+ Pickler.field (fun s -> s.kdtreePath)                Pickler.string
      ^+ Pickler.field (fun s -> s.objectSetPath)             Pickler.string
      ^+ Pickler.field (fun s -> s.coordinatesPath)           Pickler.string
      ^. Pickler.field (fun s -> s.texturePath)               Pickler.string

  let level0KdTreePickler : Pickler<Level0KdTree> =
      Pickler.sum (fun x k1 k2->
          match x with
              | InCoreKdTree k -> k1 k
              | LazyKdTree k -> k2 k)
      ^+ Pickler.case InCoreKdTree incorePickler
      ^. Pickler.case LazyKdTree lazyPickler

  let save path (b : BinarySerializer) (d : 'a) =    
    let arr =  b.Pickle d
    System.IO.File.WriteAllBytes(path, arr);
    d
  
  let loadAs<'a> path (b : BinarySerializer) : 'a =
    let arr = System.IO.File.ReadAllBytes(path)
    b.UnPickle arr
  
  let loadKdtree p =
    //Log.startTimed "loading tree"
    use b = new BinaryReadingCoder(System.IO.File.OpenRead(p))
    let mutable treeOida = Unchecked.defaultof<KdIntersectionTree>
    b.CodeT(&treeOida)
    //Log.stop()        
    ConcreteKdIntersectionTree(treeOida, Trafo3d.Identity)


  let loadKdTrees' (h : PatchHierarchy) (trafo:Trafo3d) (load : bool) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
    //ObjectBuilder

    let masterKdPath = 
      mode |> ViewerModality.matchy
        (h.kdTreeAggZero_FileAbsPath) 
        (h.kdTreeAggZero2d_FileAbsPath)

    let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")
    
    if System.IO.File.Exists(cacheFile) then
      Log.line "Found lazy kdtree cache"
      if load then
        let trees = loadAs<list<Box3d*Level0KdTree>> cacheFile b
  //      let trees = trees |> List.filter(fun (_,(LazyKdTree k)) -> k.kdtreePath = blar)
        trees |> HMap.ofList
      else
        HMap.empty
    else
      
      Log.line "did not find lazy kdtree cache"
      let patchInfos =
        h.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun x -> x.info)

      let kd0Paths = 
        patchInfos 
          |> List.map(fun x -> h.kdTree_FileAbsPath x.Name 0 mode)

      let kd0PathsExist = 
          kd0Paths |> List.forall(System.IO.File.Exists)
    
      match (System.IO.File.Exists(masterKdPath), kd0PathsExist) with
          | (true, false) -> 
            Log.line "Found master kdtree - loading incore"
            let tree = loadKdtree masterKdPath                     
            let kd = {
               kdTree = tree;
               boundingBox = tree.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
            }
            HMap.add kd.boundingBox (InCoreKdTree kd) HMap.empty
          | (true, true) ->   
            Log.line "Found master kdtree and patch trees"
            Log.startTimed "building lazy kdtree cache"
                    
            let num = kd0Paths |> List.ofSeq |> List.length        
            
            let bla = 
              kd0Paths
                |> List.zip patchInfos
                |> List.mapi(
                  fun i (info, kdPath) ->                                
                    let t = loadKdtree kdPath
                    let pos =
                      match mode with
                      | XYZ -> info.Positions
                      | SvBR -> info.Positions2d.Value
                    
                    let dir = h.opcPaths.Patches_DirAbsPath +/ info.Name
                        
                    let lazyTree : LazyKdTree = {
                        kdTree          = None
                        objectSetPath   = dir +/ pos
                        coordinatesPath = dir +/ (List.head info.Coordinates)
                        texturePath     = Patch.extractTexturePath (OpcPaths h.opcPaths.Opc_DirAbsPath) info 0 
                        kdtreePath      = kdPath
                        affine          = 
                          mode 
                            |> ViewerModality.matchy info.Local2Global info.Local2Global2d
                        boundingBox   = t.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
                    }
                    Report.Progress(float i / float num)
            
                    (lazyTree.boundingBox, (LazyKdTree lazyTree))
                )                            
             
            Log.stop()
             
            bla |> save cacheFile b |> ignore
               
            if load then
              bla |> HMap.ofList
            else
              HMap.empty
          | _ ->
            Log.warn "Could not find level 0 kdtrees"
            HMap.empty
    
  let loadKdTrees (h : PatchHierarchy) (trafo:Trafo3d) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
    loadKdTrees' (h) (trafo) (true) mode b