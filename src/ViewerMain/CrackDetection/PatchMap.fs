namespace CrackDetection

open Aardvark.Base
open OpcViewer.Base
open OpcViewer.Base.Picking
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc.KdTrees

// Don't know if we'll use textures or attribute maps in the future.
// So support both for the time being.
type MapType =
    | Texture
    | AttributeMap

type PatchId =
    | PatchId of opcPath : string * name : string

type EdgeMapInfo =
    {
        size : V2i
        data : Lazy<float Matrix>
    }

type PatchInfo =
    {
        id              : PatchId
        boundingBox     : Box2d
        edgeMap         : Map<MapType, EdgeMapInfo>
        positionsMap    : Lazy<V3d Matrix>
        textureCoords   : Lazy<V2d Matrix>
    }

type PatchMap =
    {
        typ         : MapType
        size        : V2i
        lookup      : Map<PatchId, V2i>
        patches     : PatchInfo option [,]
        offsets     : V2i [,]
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchId =

    let create (opcPath : string) (name : string) =
        PatchId (opcPath, name)

    let ofHit (hit : HitInfo) =
        let opcPath = hit.opcInfo.patchHierarchy.opcPaths.Opc_DirAbsPath
        PatchId (opcPath, hit.kdTree.name)

    let opc = function
        PatchId (x, _) -> x

    let name = function
        PatchId (_, x) -> x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EdgeMapInfo =

    let create (data : Lazy<float Matrix>) =
        // TODO: Don't read the whole map to get the dimension
        {
            size = data.Value |> Matrix.size
            data = data
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchInfo =

    let alignsY (x : PatchInfo) (y : PatchInfo) =
        let a = x.boundingBox
        let b = y.boundingBox
        Fun.ApproximateEquals (a.Center.Y, b.Center.Y, (max a.Size.Y b.Size.Y) * 0.5)

    // Gets the size of the edge map
    let size (typ : MapType) (p : PatchInfo) =
        p.edgeMap.[typ].size

    // Gets the data of the edge map
    let data (typ : MapType) (p : PatchInfo) =
        Lazy.get p.edgeMap.[typ].data

    let create (opcPath : string) (rootPath : string) (patch : Patch) =

        let edgeMap =
            [
                AttributeMap,   lazy (patch |> Patch.loadEdgeMap rootPath)
                Texture,        lazy (patch |> Patch.loadEdgeTexture opcPath)
            ]
            |> List.map (fun (typ, data) -> typ, EdgeMapInfo.create data)
            |> Map.ofList

        let positionMap = lazy (
            patch |> Patch.loadPositionsMap rootPath
        )

        let textureCoords = lazy (
            patch |> Patch.loadTextureCoords rootPath
        )

        {
            id              = PatchId (opcPath, patch.info.Name)
            boundingBox     = Box2d.ofBox3d patch.info.GlobalBoundingBox2d
            edgeMap         = edgeMap
            positionsMap    = positionMap
            textureCoords   = textureCoords
        }

    let getLocalCoords (typ : MapType) (hit : TriangleHit) (patch : PatchInfo) =
        let indexSize = patch |> size AttributeMap
        let coords = hit |> TriangleHit.toV2i indexSize

        match typ with
        | Texture ->
            let size = patch |> size Texture
            let uv = patch.textureCoords.Value |> Matrix.get coords
            let local = uv * V2d (size - V2i.II)
            V2i (local.Round ())
        | AttributeMap ->
            coords

    let patch2global (typ : MapType) (local : V2i) (patch : PatchInfo) =
        match typ with
        | Texture ->
            let size = patch |> size Texture
            let uv = V2d local / V2d (size - V2i.II)

            // TODO: This is quite inaccurate. Unfortunately, there
            // is no easy way to get from texture space to world space
            let (index, _) =
                patch.textureCoords.Value.Data
                |> Array.indexed
                |> Array.fold (fun (currentIndex, currentDist) (i, x) ->
                    let d = V2d.Distance(x, uv)
                    if d < currentDist then (i, d) else (currentIndex, currentDist)
                ) (0, System.Double.MaxValue)

            patch.positionsMap.Value.Data.[index]
        | AttributeMap ->
            patch.positionsMap.Value |> Matrix.get local


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchMap =

    let private fromArray (typ : MapType) (patches : PatchInfo option [,]) =

        let lookup =
            patches
            |> Array2D.mapi (fun x y v ->
                v |> Option.map (fun p -> p.id, V2i(x, y))
            )
            |> Array2D.toSeq
            |> Seq.choose id
            |> Map.ofSeq

        let patchSize (p : PatchInfo option) =
            p |> Option.map (PatchInfo.size typ) |> Option.defaultValue V2i.Zero

        let patchWidth p = (patchSize p).X

        let patchHeight p = (patchSize p).Y

        let width =
            patches |> Array2D.rows |> Seq.map (Seq.sumBy patchWidth) |> Seq.max

        let height =
            patches |> Array2D.cols |> Seq.map (Seq.sumBy patchHeight) |> Seq.max

        let offsets =
            patches |> Array2D.partialSums patchSize

        {
            typ = typ
            size = V2i (width, height)
            lookup = lookup
            offsets = offsets
            patches = patches
        }

    /// Creates a patch map from the given quadtree of patches.
    let create (typ : MapType) (patches : #seq<PatchInfo>) =

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
        |> fromArray typ

    /// Loads the edge maps of the patches and concats them into a single one.
    let loadEdgeMap (map : PatchMap) =

        let setCell (p : PatchInfo) (cell : Matrix<float>) =
            let data = p |> PatchInfo.data map.typ
            cell.SetByCoord (fun (coord : V2l) -> data.[coord]) |> ignore

        let edgeMap = Matrix<float> map.size

        map.patches
        |> Array2D.iteri' (fun cell elem ->
            match elem with
            | Some p ->
                let origin = map.offsets |> Array2D.get cell
                let size = p |> PatchInfo.size map.typ
                edgeMap |> Matrix.subMatrix origin size |> setCell p
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
            let local = info |> PatchInfo.getLocalCoords map.typ hit.triangle

            offset + local
        )

    /// Transforms global map coordinates to local coordinates of
    /// the corresponding patch.
    let map2patch (map : PatchMap) (coord : V2i) =

        let isWithin (patchCoord : V2i) (patch : PatchInfo option) =
            patch |> Option.map (fun info ->
                let offset = map.offsets |> Array2D.get patchCoord
                let size = info |> PatchInfo.size map.typ
                let bb = Box2i (offset, offset + size)
                bb.Contains coord
            ) |> Option.defaultValue false

        map.patches
        |> Array2D.tryFindIndexi isWithin
        |> Option.map (fun patchCoord ->
            let info = map.patches |> Array2D.get patchCoord |> Option.get
            let offset = map.offsets |> Array2D.get patchCoord

            info, coord - offset
        )

    /// Transforms global map coordiantes to XYZ world space.
    let map2global (map : PatchMap) (coord : V2i) =
        coord
        |> map2patch map
        |> Option.map (fun (patch, local) -> patch |> PatchInfo.patch2global map.typ local)