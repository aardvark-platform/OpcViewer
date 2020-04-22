namespace CrackDetection

open Aardvark.Base
open OpcViewer.Base
open OpcViewer.Base.Picking
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc.KdTrees

type PatchId =
    | PatchId of opcPath : string * name : string

type PatchInfo =
    {
        id           : PatchId
        size         : V2i
        boundingBox  : Box2d
        edgeMap      : Lazy<Matrix<float>>
        positionsMap : Lazy<Matrix<V3d>>
    }

type PatchMap =
    {
        size        : V2i
        lookup      : Map<PatchId, V2i>
        patches     : PatchInfo option [,]
        offsets     : V2i [,]
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchId =

    let create (opcPath : string) (name : string) =
        PatchId (opcPath, name)

    let ofKdTree (tree : LazyKdTree) =
        PatchId (tree.opcPath, tree.name)

    let ofHit (hit : HitInfo) =
        hit.kdTree |> ofKdTree

    let opc = function
        PatchId (x, _) -> x

    let name = function
        PatchId (_, x) -> x


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchInfo =

    let alignsY (x : PatchInfo) (y : PatchInfo) =
        let a = x.boundingBox
        let b = y.boundingBox
        Fun.ApproximateEquals (a.Center.Y, b.Center.Y, (max a.Size.Y b.Size.Y) * 0.5)

    let size (p : PatchInfo) = p.size

    let create (opcPath : string) (rootPath : string) (patch : Patch) =

        let edgeMap = lazy (patch |> Patch.loadEdgeMap rootPath)
        let positionMap = lazy (patch |> Patch.loadPositionsMap rootPath)

        {
            // TODO: Don't read the whole map to get the dimension
            id              = PatchId (opcPath, patch.info.Name)
            size            = edgeMap.Value |> Matrix.size
            boundingBox     = Box2d.ofBox3d patch.info.GlobalBoundingBox2d
            edgeMap         = edgeMap
            positionsMap    = positionMap
        }

    /// Transforms UV coordinates to global XYZ coordinates
    let patch2global (patch : PatchInfo) (uv : V2i) =
        patch.positionsMap.Value |> Matrix.get uv


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchMap =

    let private fromArray (patches : PatchInfo option [,]) =

        let lookup =
            patches
            |> Array2D.mapi (fun x y v ->
                v |> Option.map (fun p -> p.id, V2i(x, y))
            )
            |> Array2D.toSeq
            |> Seq.choose id
            |> Map.ofSeq

        let patchSize (p : PatchInfo option) =
            p |> Option.map PatchInfo.size |> Option.defaultValue V2i.Zero

        let patchWidth p = (patchSize p).X

        let patchHeight p = (patchSize p).Y

        let width =
            patches |> Array2D.rows |> Seq.map (Seq.sumBy patchWidth) |> Seq.max

        let height =
            patches |> Array2D.cols |> Seq.map (Seq.sumBy patchHeight) |> Seq.max

        let offsets =
            patches |> Array2D.partialSums patchSize

        {
            size = V2i (width, height)
            lookup = lookup
            offsets = offsets
            patches = patches
        }

    /// Creates a patch map from the given quadtree of patches.
    let create (patches : #seq<PatchInfo>) =

        // Degenerated bounding boxes of ground patches span the whole B axis
        // Unusable so we remove them
        let isInvalid (p : PatchInfo) =
            let bb = p.boundingBox
            Fun.ApproximateEquals (abs bb.Min.Y, abs bb.Max.Y, 0.001)

        patches
        |> Seq.filter (isInvalid >> not)
        |> Array.ofSeq
        |> Array.cluster PatchInfo.alignsY
        |> Array.sortByDescending (fun row -> row.[0].boundingBox.Center.Y)
        |> Array.map (Array.sortBy (fun p -> p.boundingBox.Center.X))
        |> Array.toArray2d
        |> fromArray

    /// Loads the edge maps of the patches and concats them into a single one.
    let loadEdgeMap (map : PatchMap) =

        let setCell (p : PatchInfo) (cell : Matrix<float>) =
            let data = p.edgeMap.Value
            cell.SetByCoord (fun (coord : V2l) -> data.[coord]) |> ignore

        let edgeMap = Matrix<float> map.size

        map.patches
        |> Array2D.iteri' (fun cell elem ->
            match elem with
            | Some p ->
                let origin = map.offsets |> Array2D.get cell
                edgeMap |> Matrix.subMatrix origin p.size |> setCell p
            | None ->
                ()
        )

        edgeMap

    /// Transforms hit coordinates to global map coordinates.
    let hit2map (map : PatchMap) (hit : HitInfo) =
        map.lookup
        |> Map.tryFind (PatchId.ofHit hit)
        |> Option.map (fun patchCoord ->
            let info = map.patches |> Array2D.get patchCoord |> Option.get
            let offset = map.offsets |> Array2D.get patchCoord
            let local = hit.triangle |> TriangleHit.toV2i info.size

            offset + local
        )

    /// Transforms global map coordinates to local coordinates of
    /// the corresponding patch.
    let map2patch (map : PatchMap) (coord : V2i) =

        let isWithin (patchCoord : V2i) (patch : PatchInfo option) =
            patch |> Option.map (fun info ->
                let offset = map.offsets |> Array2D.get patchCoord
                let bb = Box2i (offset, offset + info.size)
                bb.Contains coord
            ) |> Option.defaultValue false

        map.patches
        |> Array2D.tryFindIndexi isWithin
        |> Option.map (fun patchCoord ->
            let info = map.patches |> Array2D.get patchCoord |> Option.get
            let offset = map.offsets |> Array2D.get patchCoord

            info, coord - offset
        )