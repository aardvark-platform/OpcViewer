namespace Aardvark.VRVis.Opc
// copy of https://raw.githubusercontent.com/aardvark-platform/OpcViewer/master/src/OPCViewer.Base/KdTrees.fs
// should be consolidated
open System
open System.IO
open Aardvark.Geometry
open Aardvark.Base
open Aardvark.Base.Coder
open Aardvark.SceneGraph.Opc
open MBrace.FsPickler
open MBrace.FsPickler.Combinators
open FSharp.Data.Adaptive
open System.Collections.Generic

module KdTrees =

    type LazyKdTree =
        { kdTree: option<ConcreteKdIntersectionTree>
          affine: Trafo3d
          boundingBox: Box3d
          kdtreePath: string
          objectSetPath: string
          coordinatesPath: string
          texturePath: string }

    type InCoreKdTree =
        { kdTree: ConcreteKdIntersectionTree
          boundingBox: Box3d }

    type Level0KdTree =
        | LazyKdTree of LazyKdTree
        | InCoreKdTree of InCoreKdTree

    let relativePath (path: string) (remaining: int) =
        path.Split([| "/"; "\\" |], StringSplitOptions.None)
        |> List.ofArray
        |> List.rev
        |> List.take remaining
        |> List.rev
        |> Path.combine

    let relativePath' (path: string) = relativePath path 3

    let tryRepairCaseInsitivityInCaches (pathToFileInPatchDir : string) =
        let components = pathToFileInPatchDir.Split([| "/"; "\\" |], StringSplitOptions.None) 
        "Dinosaur_Quarry_2/OPC_000_000/patches/00-Patch-00007~0001/someFile.aara"
        let prefix = components[0..components.Length - 4]
        let suffix = components[components.Length - 2 ..]
        let patches = components[components.Length - 3]
        prefix,patches,suffix

    let expandKdTreePaths basePath kd =
        kd
        |> HashMap.map (fun _ k ->
            match k with
            | Level0KdTree.LazyKdTree lkt ->
                let kdTreeSub = lkt.kdtreePath |> relativePath'
                let triangleSub = lkt.objectSetPath |> relativePath'

                let kdPath = Path.Combine(basePath, kdTreeSub)
                let objectSetPath = Path.Combine(basePath, triangleSub)

                Log.line "[KdTrees] path: %s" kdPath
                if not (File.Exists(kdPath)) then
                    Log.warn "KdPath does not exist"

                LazyKdTree
                    { lkt with
                        kdtreePath = Path.Combine(basePath, kdTreeSub)
                        objectSetPath = Path.Combine(basePath, triangleSub) }
            | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik)

    let makeInCoreKd a =
        { kdTree = new ConcreteKdIntersectionTree()
          boundingBox = a }

    let makeLazyTree a b c d e f =
        { kdTree = None
          affine = a
          boundingBox = b
          kdtreePath = c
          objectSetPath = d
          coordinatesPath = e
          texturePath = f }

    // PICKLER
    let incorePickler: Pickler<InCoreKdTree> =
        Pickler.product makeInCoreKd
        ^. Pickler.field (fun s -> s.boundingBox) Pickler.auto<Box3d>

    let lazyPickler: Pickler<LazyKdTree> =
        Pickler.product makeLazyTree
        ^+ Pickler.field (fun (s: LazyKdTree) -> s.affine) Pickler.auto<Trafo3d>
        ^+ Pickler.field (fun (s: LazyKdTree) -> s.boundingBox) Pickler.auto<Box3d>
        ^+ Pickler.field (fun s -> s.kdtreePath) Pickler.string
        ^+ Pickler.field (fun s -> s.objectSetPath) Pickler.string
        ^+ Pickler.field (fun s -> s.coordinatesPath) Pickler.string
         ^. Pickler.field (fun s -> s.texturePath) Pickler.string

    let level0KdTreePickler: Pickler<Level0KdTree> =
        Pickler.sum (fun x k1 k2 ->
            match x with
            | InCoreKdTree k -> k1 k
            | LazyKdTree k -> k2 k)
        ^+ Pickler.case InCoreKdTree incorePickler
           ^. Pickler.case LazyKdTree lazyPickler

    // SAVE LOAD
    let save path (b: BinarySerializer) (d: 'a) =
        let arr = b.Pickle d
        System.IO.File.WriteAllBytes(path, arr)
        d

    let loadAs<'a> path (b: BinarySerializer) : 'a =
        let arr = System.IO.File.ReadAllBytes(path)
        b.UnPickle arr

    let loadKdtree path =
        //Log.startTimed "loading tree"
        use b = new BinaryReadingCoder(System.IO.File.OpenRead(path))
        let mutable kdTree = Unchecked.defaultof<KdIntersectionTree>
        b.CodeT(&kdTree)
        //Log.stop()
        ConcreteKdIntersectionTree(kdTree, Trafo3d.Identity)

    let saveKdTree (kdTree : KdIntersectionTree) path =
        Log.startTimed "saving kd tree"
        use b = new BinaryWritingCoder(System.IO.File.OpenWrite(path))
        b.CodeT(ref kdTree)
        Log.stop()


    let loadKdTrees'
        (h: PatchHierarchy)
        (trafo: Trafo3d)
        (load: bool)
        (mode: ViewerModality)
        (b: BinarySerializer)
        (forceRebuild : bool)
        (ignoreMasterKdTree : bool)
        (loadTriangles : Trafo3d -> string -> TriangleSet)
        (surpressFileConstruction : bool)
        : HashMap<Box3d, Level0KdTree> =

        let masterKdPath =
            mode
            |> ViewerModality.matchy (h.kdTreeAggZero_FileAbsPath) (h.kdTreeAggZero2d_FileAbsPath)

        let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")

        let loadAndCreateCache () =
            let patchInfos =
                h.tree
                |> QTree.getLeaves
                |> Seq.toArray
                |> Array.map (fun x -> x.info)

            let kd0Paths =
                patchInfos
                |> Array.map (fun x -> x, h.kdTree_FileAbsPath x.Name 0 mode)

            let missingKd0Paths = kd0Paths |> Array.filter (not << System.IO.File.Exists << snd)
            Log.line "missing kd0 paths: %d/%d" missingKd0Paths.Length kd0Paths.Length

            if not ignoreMasterKdTree && (File.Exists masterKdPath && not (Array.isEmpty missingKd0Paths)) && not forceRebuild then

                Log.warn "Found master kdtree - loading incore. THIS NEEDS A LOT OF MEMORY. CONSIDER CREATING PER-PATCH KD TREES. see: "
                let tree = loadKdtree masterKdPath

                let kd =
                    { kdTree = tree
                      boundingBox = tree.KdIntersectionTree.BoundingBox3d.Transformed(trafo) }

                HashMap.single kd.boundingBox (InCoreKdTree kd) 
            else
                Log.line "Found master kdtree and patch trees"
                Log.startTimed "building lazy kdtree cache"

                let num = kd0Paths |> List.ofSeq |> List.length


                let kdTrees =
                    kd0Paths
                    |> Array.indexed
                    |> Array.map (fun (i, (info,kdPath)) ->
                        
                        let dir = h.opcPaths.Patches_DirAbsPath +/ info.Name
                        let pos =
                            match mode with
                            | XYZ -> info.Positions
                            | SvBR -> info.Positions2d.Value

                        let objectSetPath = dir +/ pos
                        let trafo = mode |> ViewerModality.matchy info.Local2Global info.Local2Global2d

                        let createConcreteTree () : ConcreteKdIntersectionTree = 
                            let triangleSet = loadTriangles trafo objectSetPath

                            Log.startTimed $"Building KdTree for {info.Name}"
                            let flags =
                                 KdIntersectionTree.BuildFlags.Picking 
                                 ||| KdIntersectionTree.BuildFlags.FastBuild 
                                 ||| KdIntersectionTree.BuildFlags.EmptySpaceOptimization
                                 
                            let kdTree = KdIntersectionTree(triangleSet, flags )
                            Log.stop()
                            Log.startTimed "saving KdTree to: %s" info.Name
                            saveKdTree kdTree kdPath
                            Log.stop()
                            let fi = FileInfo(kdPath)
                            Log.line $"{info.Name} has size: {Mem(fi.Length)}."
                            ConcreteKdIntersectionTree(kdTree, Trafo3d.Identity)

                        let t = 
                            if File.Exists kdPath && not forceRebuild then 
                                try 
                                    loadKdtree kdPath |> Some
                                with e -> 
                                    Log.warn "[KdTrees] could not load kdtree: %A" e
                                    if surpressFileConstruction then None
                                    else createConcreteTree() |> Some
                            elif not surpressFileConstruction then
                                createConcreteTree() |> Some
                            else 
                                None


                        match t with
                        | Some t -> 
                            let lazyTree: LazyKdTree =
                                { kdTree = None
                                  objectSetPath = objectSetPath
                                  coordinatesPath = dir +/ (List.head info.Coordinates)
                                  texturePath = Patch.extractTexturePath (OpcPaths h.opcPaths.Opc_DirAbsPath) info 0
                                  kdtreePath = kdPath
                                  affine =
                                    mode
                                    |> ViewerModality.matchy info.Local2Global info.Local2Global2d
                                  boundingBox = t.KdIntersectionTree.BoundingBox3d 
                                }

                            Report.Progress(float i / float num)

                            (lazyTree.boundingBox, (LazyKdTree lazyTree)) |> Some
                        | _ -> 
                            None
                    )

                
                Log.stop ()

                if kdTrees |> Array.exists Option.isNone then 
                    HashMap.empty
                else
                    let trees = kdTrees |> Array.map Option.get |> Array.toList // safe because check above
                    trees |> save cacheFile b |> ignore

                    if load then
                        trees |> HashMap.ofList
                    else
                        HashMap.empty


        if File.Exists cacheFile then
            Log.line "Found lazy KdTree cache"

            if load then

                if forceRebuild then
                    loadAndCreateCache()
                else
                    try
                        let trees = loadAs<list<Box3d * Level0KdTree>> cacheFile b
                        trees |> HashMap.ofList
                    with
                    | e ->
                        Log.warn "could not load lazy KdTree cache. (%A) rebuilding..." e
                        loadAndCreateCache ()
            else
                HashMap.empty
        else
            loadAndCreateCache ()

    let loadKdTrees
        (h: PatchHierarchy) (trafo: Trafo3d) (mode: ViewerModality)
        (b: BinarySerializer) (forceRebuild : bool) (ignoreMasterKdTree : bool)
        (loadTriangles : Trafo3d -> string -> TriangleSet) (surpressFileConstruction : bool) : HashMap<Box3d, Level0KdTree> =

        loadKdTrees' h trafo true mode b forceRebuild ignoreMasterKdTree loadTriangles surpressFileConstruction
