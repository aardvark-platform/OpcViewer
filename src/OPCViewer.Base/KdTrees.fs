namespace Aardvark.VRVis.Opc

open System
open System.IO
open Aardvark.Geometry
open Aardvark.Base
open Aardvark.Base.Coder
open Aardvark.SceneGraph.Opc
open MBrace.FsPickler
open MBrace.FsPickler.Combinators

module KdTrees =

    type LazyKdTree = {
        name              : string
        opcPath           : string
        kdTree            : option<ConcreteKdIntersectionTree>
        affine            : Trafo3d
        boundingBox       : Box3d
        boundingBox2d     : Box3d
        kdtreePath        : string
        objectSetPath     : string
        coordinatesPath   : string
        texturePath       : string
        positions2dPath   : string
        positions2dAffine : Trafo3d
    }

    type InCoreKdTree = {
        kdTree      : ConcreteKdIntersectionTree
        boundingBox : Box3d
    }

    type Level0KdTree =
        | LazyKdTree   of LazyKdTree
        | InCoreKdTree of InCoreKdTree

    let relativePath (path : string) (remaining : int) =
        path.Split(Path.DirectorySeparatorChar)
        |> List.ofArray
        |> List.rev
        |> List.take remaining
        |> List.rev
        |> Path.combine

    let relativePath' (path : string) =
        relativePath path 3

    let expandKdTreePaths basePath kd =
        kd
        |> HMap.map(fun _ k ->
            match k with
            | Level0KdTree.LazyKdTree lkt ->
                let kdTreeSub   = lkt.kdtreePath    |> relativePath'
                let triangleSub = lkt.objectSetPath |> relativePath'

                LazyKdTree {
                    lkt with
                        kdtreePath    = Path.Combine(basePath, kdTreeSub)
                        objectSetPath = Path.Combine(basePath, triangleSub)
                }
            | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik
        )

    let makeInCoreKd a =
        {
            kdTree = new ConcreteKdIntersectionTree()
            boundingBox = a
        }

    let makeLazyTree a b c d e f g h i j k =
        {
            name              = a
            opcPath           = b
            kdTree            = None
            affine            = c
            boundingBox       = d
            boundingBox2d     = e
            kdtreePath        = f
            objectSetPath     = g
            coordinatesPath   = h
            texturePath       = i
            positions2dPath   = j
            positions2dAffine = k
        }

    // PICKLER
    let incorePickler : Pickler<InCoreKdTree> =
        Pickler.product makeInCoreKd
        ^. Pickler.field (fun s -> s.boundingBox)     Pickler.auto<Box3d>

    let lazyPickler : Pickler<LazyKdTree> =
        Pickler.product makeLazyTree
        ^+ Pickler.field (fun s -> s.name)                Pickler.string
        ^+ Pickler.field (fun s -> s.opcPath)             Pickler.string
        ^+ Pickler.field (fun s -> s.affine)              Pickler.auto<Trafo3d>
        ^+ Pickler.field (fun s -> s.boundingBox)         Pickler.auto<Box3d>
        ^+ Pickler.field (fun s -> s.boundingBox2d)       Pickler.auto<Box3d>
        ^+ Pickler.field (fun s -> s.kdtreePath)          Pickler.string
        ^+ Pickler.field (fun s -> s.objectSetPath)       Pickler.string
        ^+ Pickler.field (fun s -> s.coordinatesPath)     Pickler.string
        ^+ Pickler.field (fun s -> s.texturePath)         Pickler.string
        ^+ Pickler.field (fun s -> s.positions2dPath)     Pickler.string
        ^. Pickler.field (fun s -> s.positions2dAffine)   Pickler.auto<Trafo3d>

    let level0KdTreePickler : Pickler<Level0KdTree> =
        Pickler.sum (fun x k1 k2->
            match x with
                | InCoreKdTree k -> k1 k
                | LazyKdTree k -> k2 k)
        ^+ Pickler.case InCoreKdTree incorePickler
        ^. Pickler.case LazyKdTree lazyPickler

    // SAVE LOAD
    let save path (b : BinarySerializer) (d : 'a) =
        let arr =  b.Pickle d
        File.WriteAllBytes(path, arr);
        d

    let loadAs<'a> path (b : BinarySerializer) : 'a =
        let arr = File.ReadAllBytes(path)
        b.UnPickle arr

    let loadKdtree path =
        //Log.startTimed "loading tree"
        use b = new BinaryReadingCoder(File.OpenRead(path))
        let mutable treeOida = Unchecked.defaultof<KdIntersectionTree>
        b.CodeT(&treeOida)
        //Log.stop()
        ConcreteKdIntersectionTree(treeOida, Trafo3d.Identity)

    // Checks if the cached directories are valid
    // Throws an ArgumentException on failure
    let validatePaths (tree : Level0KdTree) =
        let validate f path =
            if not (f path) then raise (ArgumentException path)

        let validateDir = validate Directory.Exists
        let validateFile = validate File.Exists

        match tree with
        | LazyKdTree t ->
            let dirs = [ t.opcPath ]
            let files = [ t.coordinatesPath; t.kdtreePath; t.objectSetPath; t.texturePath ]
            dirs |> List.iter validateDir
            files |> List.iter validateFile
        | _ -> ()

    let loadKdTrees' (h : PatchHierarchy) (trafo:Trafo3d) (load : bool) (mode : ViewerModality) (b : BinarySerializer) : hmap<Box3d, Level0KdTree> =
        //ObjectBuilder

        let masterKdPath =
            mode |> ViewerModality.matchy (h.kdTreeAggZero_FileAbsPath) (h.kdTreeAggZero2d_FileAbsPath)

        let cacheFile = Path.ChangeExtension(masterKdPath, ".cache")

        let cached =
            if load then
                try
                    let trees : List<Box3d * Level0KdTree> = loadAs cacheFile b
                    trees |> List.iter (fun (_, t) -> validatePaths t)
                    trees |> HMap.ofList |> Some
                with
                | :? FileNotFoundException ->
                    Log.line "did not find lazy kdtree cache"
                    None
                | :? ArgumentException as ex ->
                    Log.line "referenced path in lazy kdtree cache invalid: %s" ex.Message
                    None
                | _ ->
                    Log.line "could not load lazy kdtree cache"
                    None
            else
                Some HMap.empty

        match cached with
        | Some trees ->
            Log.line "loaded lazy kdtree cache"
            trees

        | None ->
            let patchInfos =
                h.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun x -> x.info)

            let kd0Paths =
                patchInfos
                |> List.map(fun x -> h.kdTree_FileAbsPath x.Name 0 mode)

            let kd0PathsExist =
                kd0Paths |> List.forall(File.Exists)

            match (File.Exists(masterKdPath), kd0PathsExist) with
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
                            name                = info.Name
                            opcPath             = h.opcPaths.Opc_DirAbsPath
                            kdTree              = None
                            objectSetPath       = dir +/ pos
                            coordinatesPath     = dir +/ (List.head info.Coordinates)
                            texturePath         = Patch.extractTexturePath (OpcPaths h.opcPaths.Opc_DirAbsPath) info 0
                            kdtreePath          = kdPath
                            affine              = mode |> ViewerModality.matchy info.Local2Global info.Local2Global2d
                            boundingBox         = t.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
                            boundingBox2d       = info.GlobalBoundingBox2d
                            positions2dAffine   = info.Local2Global2d
                            positions2dPath     = dir +/ info.Positions2d.Value
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