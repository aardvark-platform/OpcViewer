namespace CrackDetection

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open OpcViewer.Base
open OpcViewer.Base.Picking

#nowarn "0686"

[<AutoOpen>]
module private Utilities =

    module Matrix =
        let private lerp =
            Func<float, V3d, V3d, V3d>(
                fun t a b -> VecFun.Lerp(t, a, b)
            )

        let size (m : Matrix<'a>) =
            m.Dim |> V2i

        let ofVolume (v : Volume<'a>) =
            v.SubXYMatrix 0L

        let subMatrix (origin : V2i) (size : V2i) (m : Matrix<'a>) =
            m.SubMatrix(origin, size)

        let load (path : string) =
            path |> fromFile |> ofVolume

        let toPixImage (matrix : Matrix<C3b>) =
            TensorExtensions.ToPixImage<byte> matrix

        let map (map : 'a -> 'b) (input : Matrix<'a>) : Matrix<'b> =        
            let array = input.Array.ToArrayOfT<'a>() |> Array.map map
            Matrix(array, input.Size)

        let get (position : V2d) (matrix : Matrix<V3d>) =
            matrix.Sample4Clamped(position, lerp, lerp)

        let sample (uv : V2d) (matrix : Matrix<V3d>) =
            let size = V2d (matrix.Dim.XY - V2l.II)
            let texel = V2d (1.0 - uv.X, uv.Y) * size

            matrix |> get texel

    module KdTree =
        let loadSvBRMap (kd : LazyKdTree) =
            kd.positions2dPath |> Matrix.load<V3f> |> Matrix.map V3d

    module Array =

        let cluster (f : 'a -> 'a -> bool) (s : 'a []) =
            let single x =
                [|[| x |]|]

            let add (clusters : 'a [][]) (value : 'a) =
                let ret = clusters |> Array.tryFindIndex (Array.forall (f value))
                match ret with
                | Some i ->
                    clusters.[i] <- [| value |] |> Array.append clusters.[i]
                    clusters
                | None ->
                    value |> single |> Array.append clusters

            s |> Array.fold add Array.empty

        let toArray2d (arr : 'a [][]) =
            let w = arr.Length
            let h = arr |> Array.fold (fun l x -> x.Length |> max l) 0

            Array2D.init w h (fun x y ->
                if y < h then
                    Some arr.[x].[y]
                else
                    None
            )

    module Array2D =

        let toSeq (arr : 'a [,]) =
            seq {
                for x in 0 .. (Array2D.length1 arr) - 1 do
                    for y in 0 .. (Array2D.length2 arr) - 1 do
                        yield arr.[x, y]
            }

        let row (i : int) (arr : 'a [,]) =
            seq { for x in 0 .. (Array2D.length1 arr) - 1 do arr.[x, i] }

        let col (i : int) (arr : 'a [,]) =
            seq { for y in 0 .. (Array2D.length2 arr) - 1 do arr.[i, y] }

        let rows (arr : 'a [,]) =
            seq { for i in 0 .. (Array2D.length2 arr) - 1 do arr |> row i }

        let cols (arr : 'a [,]) =
            seq { for i in 0 .. (Array2D.length1 arr) - 1 do arr |> col i }

        let get (p : V2i) (arr : 'a [,]) =
            arr.[p.X, p.Y]

        let size (arr : 'a [,]) =
            V2i (Array2D.length1 arr, Array2D.length2 arr)

        let iteri' (f : V2i -> 'a -> unit) =
            Array2D.iteri (fun x y elem -> elem |> f (V2i (x, y)))

        let subarray (min : V2i) (max : V2i) (arr : 'a [,]) =
            let size = max - min + V2i.One
            let rs = Array2D.zeroCreate size.X size.Y
            Array2D.blit arr min.X min.Y rs 0 0 size.X size.Y
            rs

    module Patch =

        type private Box2d with
            static member ofBox3d (b : Box3d) =
                Box2d (b.Min.XY, b.Max.XY)

            member x.Intersects (b : Box3d) =
                b |> Box2d.ofBox3d |> x.Intersects

        let intersects (b : Box2d) (p : Patch) =
            b.Intersects (p.info.GlobalBoundingBox2d)

        let alignsY (x : Patch) (y : Patch) =
            let a = x.info.GlobalBoundingBox2d
            let b = y.info.GlobalBoundingBox2d
            Fun.ApproximateEquals (a.Center.Y, b.Center.Y, (max a.Size.Y b.Size.Y) * 0.5)

        let loadMap (rootPath : string) (fileName : string) (patch : Patch) =
            let path = rootPath +/ patch.info.Name +/ fileName
            path |> fromFile<'a> |> Matrix.ofVolume

        let loadEdgeMap (rootPath : string) (patch : Patch) =
            patch
            |> loadMap<float> rootPath "EdgeMap.aara"

        let loadPositionsMap (rootPath : string) (patch : Patch) =
            patch
            |> loadMap<V3f> rootPath patch.info.Positions
            |> Matrix.map (V3d >> patch.info.Local2Global.Forward.TransformPos)

    module QTree =

        type private Patch with
            member x.Intersects (b : Box2d) =
                x |> Patch.intersects b

        /// Finds leaf patches that intersect with
        /// the given bounding box
        let intersect (bb : Box2d) (tree : QTree<Patch>) =
            let rec check (t : QTree<Patch>) =
                match t with
                | QTree.Node (p, c) when p.Intersects bb ->
                    c |> Array.map check |> List.concat
                | QTree.Leaf p when p.Intersects bb ->
                    [ p ]
                | _ -> []

            check tree

    type PatchInfo =
        {
            size         : V2i
            name         : string
            edgeMap      : Lazy<Matrix<float>>
            positionsMap : Lazy<Matrix<V3d>>
        }

    type PatchMap =
        {
            cellSize    : V2i
            rootPath    : string
            lookup      : Map<string, V2i>
            patches     : PatchInfo option [,]
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PatchInfo =

        let size (p : PatchInfo) = p.size

        let create (rootPath : string) (patch : Patch) =

            let edgeMap = lazy (patch |> Patch.loadEdgeMap rootPath)
            let positionMap = lazy (patch |> Patch.loadPositionsMap rootPath)

            {
                // TODO: Don't read the whole map to get the dimension
                size            = edgeMap.Value |> Matrix.size
                name            = patch.info.Name
                edgeMap         = edgeMap
                positionsMap    = positionMap
            }

        /// Transforms UV coordinates to global XYZ coordinates
        let patch2global (patch : PatchInfo) (uv : V2d) =
            patch.positionsMap.Value |> Matrix.sample uv

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PatchMap =

        let private fromArray (rootPath : string) (patches : PatchInfo option [,]) =

            let lookup =
                patches
                |> Array2D.mapi (fun x y v ->
                    v |> Option.map (fun p -> p.name, V2i(x, y))
                )
                |> Array2D.toSeq
                |> Seq.choose id
                |> Map.ofSeq

            let cellSize =
                patches
                |> Array2D.toSeq
                |> Seq.choose id
                |> Seq.fold (fun (s : V2i) (p : PatchInfo) ->
                    V2i (max s.X p.size.X, max s.Y p.size.Y)
                ) V2i.Zero

            {
                rootPath = rootPath
                patches = patches
                cellSize = cellSize
                lookup = lookup
            }

        /// Creates a patch map from the given quadtree of patches.
        let create (rootPath : string) (tree : QTree<Patch>) =

            // Degenerated bounding boxes of ground patches span the whole B axis
            // Unusable so we remove them
            let isInvalid (p : Patch) =
                let bb = p.info.GlobalBoundingBox2d
                Fun.ApproximateEquals (abs bb.Min.Y, abs bb.Max.Y, 0.001)

            let leaves =
                tree
                |> QTree.getLeaves
                |> Seq.filter (isInvalid >> not)

            let patches =
                leaves
                |> Array.ofSeq
                |> Array.cluster Patch.alignsY
                |> Array.sortBy (fun row -> row.[0].info.GlobalBoundingBox2d.Center.Y)
                |> Array.map (Array.sortByDescending (fun p -> p.info.GlobalBoundingBox2d.Center.X))
                |> Array.toArray2d
                |> Array2D.map (Option.map (PatchInfo.create rootPath))

            patches |> fromArray rootPath

        /// Creates a submap using the given patch names.
        let submap (patches : #seq<string>) (map : PatchMap) =

            let min (a : V2i) (b : V2i) =
                V2i (min a.X b.X, min a.Y b.Y)

            let max (a : V2i) (b : V2i) =
                V2i (max a.X b.X, max a.Y b.Y)

            let coords =
                patches |> Seq.choose (fun s -> map.lookup |> Map.tryFind s)

            let min = coords |> Seq.reduce min
            let max = coords |> Seq.reduce max

            map.patches
            |> Array2D.subarray min max
            |> fromArray map.rootPath

        /// Loads the edge maps of the patches and concats them into a single one.
        let loadEdgeMap (map : PatchMap) =

            let setCell (p : PatchInfo) (cell : Matrix<float>) =
                let data = p.edgeMap.Value

                cell.SetByCoord (fun (coord : V2l) ->
                    if V2l.AllSmaller(coord, data.Dim) then
                        data.[coord]
                    else
                        0.0
                ) |> ignore

            let cells = Array2D.size map.patches
            let edgeMap = Matrix<float>(map.cellSize * cells)

            map.patches
            |> Array2D.iteri' (fun cell elem ->
                match elem with
                | Some p ->
                    let origin = cell * map.cellSize
                    edgeMap |> Matrix.subMatrix origin map.cellSize |> setCell p
                | None ->
                    ()
            )

            edgeMap

        /// Transforms local texture coordinates to global map coordinates.
        let patch2map (map : PatchMap) (patch : string) (uv : V2d) =
            let patchCoord = map.lookup.[patch]

            let size =
                map.patches
                |> Array2D.get patchCoord
                |> Option.map PatchInfo.size
                |> Option.defaultValue map.cellSize
                |> V2d

            let offset =
                patchCoord * map.cellSize

            let local =
                V2d (uv.X, 1.0 - uv.Y) * (size - V2d.One)

            offset + V2i (round local.X, round local.Y)

        /// Transforms global map coordinates to local texture coordinates of
        /// the corresponding patch.
        let map2patch (map : PatchMap) (coord : V2i) =
            let patchCoord = coord / map.cellSize

            let patchInfo =
                map.patches
                |> Array2D.get patchCoord

            patchInfo |> Option.map (fun info ->
                let size = V2d info.size
                let offset = patchCoord * map.cellSize
                let local = V2d (coord - offset) / (size - V2d.One)

                info, V2d (local.X, 1.0 - local.Y)
            )


module CrackDetectionApp =

    // Saves the results as an image
    let private saveResultImage (edgeMap : Matrix<float>) (controlPoints : #seq<V2i>) (crackPoints : #seq<V2f>) =
        let resultImage =
            edgeMap
            |> Matrix.map (byte >> C3b)
            |> Matrix.toPixImage

        controlPoints
        |> Seq.iter(fun x ->
            resultImage.ChannelArray.[0].SetCross(x, 2, 255uy)
        )
        
        crackPoints
        |> Seq.map V2d
        |> Seq.pairwise
        |> Seq.iter (fun (a, b) ->
            resultImage.ChannelArray.[2].SetLine(a, b, 255uy)
        )

        resultImage.SaveAsImage (@".\cracks.png")

    // Finds a crack given some input points
    // edgeMapPath is the location of the edge map
    // posPath is the location of the attribute map containing local coordinates
    // trafo is the transformation to compute world space positions from local coordinates
    let private findCrack (points : HitInfo plist) =

        let opc = (points |> PList.first).opcInfo
        let tree = opc.patchHierarchy.tree
        let rootPath = opc.patchHierarchy.opcPaths.Patches_DirAbsPath

        // Find distinct kd trees and load SvBR maps
        let svbrMaps =
            points
            |> Seq.map (fun lp -> lp.kdTree)
            |> Seq.distinctBy (fun kd -> kd.name)
            |> Seq.map (fun kd -> kd.name, KdTree.loadSvBRMap kd)
            |> Map.ofSeq

        // Compute points in global SvB space
        let toGlobal2d (hit : HitInfo) =
            let map = svbrMaps.[hit.kdTree.name]
            let trafo = hit.kdTree.positions2dAffine.Forward
            map |> Matrix.sample hit.texCoords |> Mat.transformPos trafo |> Vec.xy

        let points2d =
            points |> Seq.map toGlobal2d

        // Find patches that are intersected
        let boundingBox2d =
            points2d.GetBoundingBox2d()

        let leafs =
            tree |> QTree.intersect boundingBox2d |> List.map (fun p -> p.info.Name)

        // Create a patch map
        let map =
            tree
            |> PatchMap.create rootPath
            |> PatchMap.submap leafs

        // Load edge map
        let edgeMap =
            PatchMap.loadEdgeMap map

        let size = edgeMap.Dim.XY

        let coeff =
            edgeMap |> Matrix.map float32

        // Control points
        let controlPoints =
            points |> Seq.map (fun p -> p.texCoords |> PatchMap.patch2map map p.kdTree.name)

        let controlPointsArr = 
            controlPoints 
            |> Seq.map V2f
            |> Seq.toArray

        // Find the crack
        let (w, h) = (uint32 size.X, uint32 size.Y)
        let crackPoints = Wrapper.findCrack w h coeff.Data controlPointsArr

        // Save results as image
        saveResultImage edgeMap controlPoints crackPoints

        // Return points
        crackPoints
        |> Array.choose (fun p ->
            V2i p
            |> PatchMap.map2patch map
            |> Option.map (fun (patch, uv) -> uv |> PatchInfo.patch2global patch)
        )
        |> PList.ofArray

    let initModel =
        {
            inputPoints  = PList.empty
            outputPoints = PList.empty
        }

    /// Initializes the native library
    let initialize () =
        let logDir = @".\crackDetection" 
        let configDir = @".\crackDetection"

        Wrapper.initialize configDir logDir

    /// Frees resources of the native library
    let free =
        Wrapper.free

    let update (msg : CrackDetectionAction) (model : CrackDetectionModel) =
        match msg with
        | AddCrackPoint hit ->
            { model with inputPoints = model.inputPoints |> PList.prepend hit }

        | FinishCrack ->
            let op = model.inputPoints |> findCrack
            { model with outputPoints = op }

    let viewBrush near far (model : MCrackDetectionModel) =
        let points = alist {
            for p in model.inputPoints do
                let! pos = p.position
                yield pos
        }
        
        let color = ~~C4b.DarkMagenta

        let pointsSg = 
            points     
            |> SgUtilities.drawPointList color ~~10.0 ~~0.1 near far
        
        pointsSg            
        |> Sg.noEvents