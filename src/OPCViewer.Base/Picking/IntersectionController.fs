namespace OpcViewer.Base.Picking

open System
open System.IO
open System.Drawing

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Application
open Aardvark.UI
open Aardvark.Geometry
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc
open Aardvark.VRVis.Opc.KdTrees
open OpcViewer.Base

module IntersectionController =
    open Aardvark.Base

    let loadTrianglesFromFileWithIndices (aaraFile: string) (matrix: M44d) =
        let positions = aaraFile |> fromFile<V3f>

        let data = positions.Data |> Array.map (fun x -> x.ToV3d() |> matrix.TransformPos)

        let invalidIndices = getInvalidIndices data
        let index = IndexHelper.computeIndexArray (positions.Size.XY.ToV2i()) invalidIndices

        let triangleIndices = index |> Seq.chunkBySize 3

        let triangles =
            triangleIndices
            |> Seq.choose (fun x ->
                if x.Length = 3 then
                    Some
                        [| data.[x.[0]]
                           data.[x.[1]]
                           data.[x.[2]] |]
                else
                    None)
            |> Seq.map (fun x -> Triangle3d(x))
            |> Seq.toArray

        (triangles, (Seq.toArray triangleIndices))

    let private getInvalidIndices2d (positions: V2d []) =
        positions
        |> List.ofArray
        |> List.mapi (fun i x ->
            if x.AnyNaN then Some i else None)
        |> List.choose id

    let loadUVTrianglesFromFileWithIndices (kdTree: LazyKdTree) =
        let coordinates = kdTree.coordinatesPath |> fromFile<V2f>

        let data = coordinates.Data |> Array.map (fun x -> x.ToV2d()) //|> kdTree.affine.Forward.TransformPos)

        let invalidIndices = getInvalidIndices2d data
        let index = IndexHelper.computeIndexArray (coordinates.Size.XY.ToV2i()) invalidIndices

        let uvIndices = index |> Seq.chunkBySize 3

        let uvCoords =
            uvIndices
            |> Seq.choose (fun x ->
                if x.Length = 3 then
                    Some
                        [| data.[x.[0]]
                           data.[x.[1]]
                           data.[x.[2]] |]
                else
                    None)
            |> Seq.map (fun x -> Triangle2d(x))
            |> Seq.toArray

        (uvCoords, (Seq.toArray uvIndices))



    let loadTrianglesWithIndices (kd: LazyKdTree) =
        loadTrianglesFromFileWithIndices kd.objectSetPath kd.affine.Forward


    let triangleIsNan (t: Triangle3d) = t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN

    let loadTriangles (kd: LazyKdTree) =
        let indexing = (fun size invalidIndices -> IndexHelper.computeIndexArray size invalidIndices)
        loadTrianglesFromFile' kd.objectSetPath indexing kd.affine.Forward

    let loadTriangleSet (kd: LazyKdTree) =
        kd
        |> loadTriangles
        |> TriangleSet

    let intersectSingleForIndex ray (kdTree: ConcreteKdIntersectionTree) = // (bb:Box3d) = //(hitObject : 'a)
        let kdi = kdTree.KdIntersectionTree
        let mutable hit = ObjectRayHit.MaxRange
        let objFilter _ _ = true
        try
            if kdi.Intersect(ray, Func<_, _, _>(objFilter), null, 0.0, Double.MaxValue, &hit)
            then Some(hit.RayHit.T, hit.SetObject.Index) //, kdTree, bb)
            else None
        with e ->
            Log.error "null ref exception in kdtree intersection %A" e
            None

    let calculateBarycentricCoordinates (triangle: Triangle3d) (pos: V3d) =
        let v0 = triangle.P1 - triangle.P0
        let v1 = triangle.P2 - triangle.P0
        let v2 = pos - triangle.P0

        let d00 = V3d.Dot(v0, v0)
        let d01 = V3d.Dot(v0, v1)
        let d11 = V3d.Dot(v1, v1)
        let d20 = V3d.Dot(v2, v0)
        let d21 = V3d.Dot(v2, v1)

        let denom = d00 * d11 - d01 * d01

        let v = (d11 * d20 - d01 * d21) / denom
        let w = (d00 * d21 - d01 * d20) / denom
        let u = 1.0 - v - w

        V3d (v, w, u)

    let insideTriangle (triangle: Triangle2d) (p: V2d) =
        //  if p lies inside the triangle, then a1 + a2 + a3 must be equal to a.
        let a = triangle.Area //Triangle2d(triangle.P0.XY, triangle.P1.XY, triangle.P2.XY).Area
        let a1 = Triangle2d(p, triangle.P1, triangle.P2).Area
        let a2 = Triangle2d(triangle.P0, p, triangle.P2).Area
        let a3 = Triangle2d(triangle.P0, triangle.P1, p).Area

        a = (a1 + a2 + a3)

    let getV3d (uvt: Triangle2d) (xyzT: Triangle3d) (p: V2d) =
        let t00 = uvt.P0.X - uvt.P2.X
        let t01 = uvt.P1.X - uvt.P2.X
        let t10 = uvt.P0.Y - uvt.P2.Y
        let t11 = uvt.P1.Y - uvt.P2.Y

        let denom = t00 * t11 - t01 * t10

        let iT00 = t11 / denom
        let iT01 = -t01 / denom
        let iT10 = -t10 / denom
        let iT11 = t00 / denom

        let lamb0 = iT00 * (p.X - uvt.P2.X) + iT01 * (p.Y - uvt.P2.Y)
        let lamb1 = iT10 * (p.X - uvt.P2.X) + iT11 * (p.Y - uvt.P2.Y)
        let lamb2 = 1.0 - lamb0 - lamb1

        //let x = xyzT.P0.X * lamb0 + xyzT.P1.X * lamb1 + xyzT.P2.X * lamb2
        //let y = xyzT.P0.Y * lamb0 + xyzT.P1.Y * lamb1 + xyzT.P2.Y * lamb2
        //let z = xyzT.P0.Z * lamb0 + xyzT.P1.Z * lamb1 + xyzT.P2.Z * lamb2

        let newV3d = xyzT.P0 * lamb0 + xyzT.P1 * lamb1 + xyzT.P2 * lamb2
        newV3d

    let calc3dPointFromUV (kdTree: LazyKdTree) (position: V2d) =
        //let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
        let coords, coordsIndices = loadUVTrianglesFromFileWithIndices kdTree
        let positions = kdTree.objectSetPath |> fromFile<V3f>
        let data = positions.Data |> Array.map (fun x -> x.ToV3d() |> kdTree.affine.Forward.TransformPos)

        let mutable index = -1

        let triangle2d =
            coords
            |> List.ofArray
            |> List.choose (fun t2d ->
                index <- index + 1
                if (insideTriangle t2d position) then Some(t2d, index) else None)
            |> List.tryHead

        let test =
            match triangle2d with
            | Some t ->
                let t2d, i = t
                let cI = coordsIndices.[i]
                let t3d = Triangle3d(data.[cI.[0]], data.[cI.[1]], data.[cI.[2]])
                let point = getV3d t2d t3d position
                point
            | None -> V3d.NaN

        test //|> kdTree.affine.Forward.TransformPos

    let calcTriangleHit (kdTree: LazyKdTree) (index: int) (position: V3d) =
        let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
        let triangle = triangles.[index]
        let barycentric = calculateBarycentricCoordinates triangle position

        if Box3d.Unit.Contains barycentric then
            Some { indices = V3i triangleIndices.[index]
                   barycentricCoords = barycentric }
        else
            Log.error "[Intersection] invalid barycentric coordinates: %A" barycentric
            None

    let calcTextureCoordinates (kdTree : LazyKdTree) (hit : TriangleHit) =
        let coords = kdTree.coordinatesPath |> fromFile<V2f>

        let p0 = coords.Data.[hit.indices.X] * (float32 hit.barycentricCoords.X)
        let p1 = coords.Data.[hit.indices.Y] * (float32 hit.barycentricCoords.Y)
        let p2 = coords.Data.[hit.indices.Z] * (float32 hit.barycentricCoords.Z)

        V2d (p0 + p1 + p2)

    let findCoordinates (kdTree: LazyKdTree) (index: int) (position: V3d) =
        let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
        let triangle = triangles.[index]

        let baryCentricCoords = calculateBarycentricCoordinates triangle position

        let coordinates = kdTree.coordinatesPath |> fromFile<V2f>

        let coordinateIndices = triangleIndices.[index]

        let p0 = coordinates.Data.[coordinateIndices.[0]] * (float32 baryCentricCoords.X)
        let p1 = coordinates.Data.[coordinateIndices.[1]] * (float32 baryCentricCoords.Y)
        let p2 = coordinates.Data.[coordinateIndices.[2]] * (float32 baryCentricCoords.Z)

        V2d(p0 + p1 + p2)

    let findCoordinatesSvB (kdTree: LazyKdTree) (index: int) (position: V3d) =

        let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices

        let dir = (Path.GetDirectoryName kdTree.coordinatesPath)
        let path = dir + "\Positions2d.aara"
        Log.line "Positions2d path: %s" path

        let positions2d = path |> fromFile<V3f>
        let svbrData =
            positions2d.Data |> Array.map (fun x -> x.ToV3d() |> kdTree.positions2dAffine.Forward.TransformPos) // |> Array.map( fun x -> x.XY)
        //let posData = data.[index]
        let invalidIndices = getInvalidIndices svbrData //getInvalidIndices2d data //
        let size = positions2d.Size.XY.ToV2i()
        let indicesSvB = IndexHelper.computeIndexArray size invalidIndices

        let svbr0 = svbrData.[triangleIndices.[index].[0]]
        let svbr1 = svbrData.[triangleIndices.[index].[1]]
        let svbr2 = svbrData.[triangleIndices.[index].[2]]

        //let uvIndices =
        //  indicesSvB
        //    |> Seq.chunkBySize 3
        //    |> Seq.toArray


        let coordinates = kdTree.coordinatesPath |> fromFile<V2f>
        //let p0 = coordinates.Data.[uvIndices.[index].[0]] * (float32 position.X)
        //let p1 = coordinates.Data.[uvIndices.[index].[1]] * (float32 position.Y)
        //let p2 = coordinates.Data.[uvIndices.[index].[2]] * (float32 position.Z)

        let testpos = V2f(position.X, position.Y)
        let exactUV = svbr0 + svbr1 + svbr2 //coordinates.Data.[indicesSvB.[index]] //* testpos // p0+p1+p2 //

        let image = PixImage.Create(kdTree.texturePath).ToPixImage<byte> (Col.Format.RGB)

        let changePos =
            V2i(((float32 image.Size.X) * (float32) exactUV.X), ((float32 image.Size.Y) * (float32) exactUV.Y))

        //let test = image.GetMatrix<C3b>().GetValue((int64)changePos.X, (int64)changePos.Y)

        image.GetMatrix<C3b>().SetCross(changePos, 5, C3b.Red) |> ignore

        image.SaveAsImage(@".\testcoord2d.png")

        exactUV

    let getEdgeLayerValue (kdTree: LazyKdTree) (index: int) (position: V3d) = //(selAttribute:string)
        let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
        let triangle = triangles.[index]

        let baryCentricCoords = calculateBarycentricCoordinates triangle position

        Log.line "barycentricCoords: u: %f, v: %f, w: %f" baryCentricCoords.X baryCentricCoords.Y baryCentricCoords.Z

        let dir = (Path.GetDirectoryName kdTree.coordinatesPath)
        let path = dir + "\EdgeMap.aara"
        Log.line "EdgeMap path: %s" path
        let values = path |> fromFile<float>

        let coordinateIndices = triangleIndices.[index]

        let p0 = values.Data.[coordinateIndices.[0]] * baryCentricCoords.X
        let p1 = values.Data.[coordinateIndices.[1]] * baryCentricCoords.Y
        let p2 = values.Data.[coordinateIndices.[2]] * baryCentricCoords.Z

        let exactAttrVal = p0 + p1 + p2

        Log.line "attr values: p0: %f + p1: %f + p2: %f = val: %f" p0 p1 p2 exactAttrVal

        exactAttrVal


//let data = values.Data |> Array.map (fun x -> x)
//Log.line "EdgeMap value: %f at index: %i" data.[index] index
//data.[index]



module Intersect =

    let mutable private cache = HMap.empty

    let single ray (kdTree: ConcreteKdIntersectionTree) =
        let kdi = kdTree.KdIntersectionTree
        let mutable hit = ObjectRayHit.MaxRange
        let objFilter _ _ = true
        try
            if kdi.Intersect(ray, Func<_, _, _>(objFilter), null, 0.0, Double.MaxValue, &hit)
            then Some(hit.RayHit.T)
            else None
        with _ ->
            Log.error "null ref exception in kdtree intersection"
            None

    let private getInvalidIndices3f (positions: V3f []) =
        positions
        |> List.ofArray
        |> List.mapi (fun i x ->
            if x.AnyNaN then Some i else None)
        |> List.choose id


    let private triangleIsNan (t: Triangle3d) = t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN

    let private getTriangleSet (indices: int []) (vertices: V3d []) =
        indices
        |> Seq.map (fun x -> vertices.[x])
        |> Seq.chunkBySize 3
        |> Seq.map (fun x -> Triangle3d(x))
        |> Seq.filter (fun x -> (triangleIsNan x |> not))
        |> Seq.toArray
        |> TriangleSet

    let private loadTriangles (kd: LazyKdTree) =

        let positions = kd.objectSetPath |> Aara.fromFile<V3f>

        let invalidIndices = getInvalidIndices3f positions.Data |> List.toArray
        let size = positions.Size.XY.ToV2i()
        let indices = IndexHelper.computeIndexArray size invalidIndices

        positions.Data
        |> Array.map (fun x -> x.ToV3d() |> kd.affine.Forward.TransformPos)
        |> getTriangleSet indices

    let private loadObjectSet (cache: hmap<string, ConcreteKdIntersectionTree>) (lvl0Tree: Level0KdTree) =
        match lvl0Tree with
        | InCoreKdTree kd -> kd.kdTree, cache
        | LazyKdTree kd ->
            let kdTree, cache =
                match kd.kdTree with
                | Some k -> k, cache
                | None ->
                    let key = (kd.boundingBox.ToString())
                    let tree = cache |> HMap.tryFind key
                    match tree with
                    | Some t -> t, cache
                    | None ->
                        Log.line "cache miss %A- loading kdtree" kd.boundingBox

                        let mutable tree = KdTrees.loadKdtree kd.kdtreePath
                        let triangles = kd |> loadTriangles

                        tree.KdIntersectionTree.ObjectSet <- triangles
                        tree, (HMap.add key tree cache)
            kdTree, cache

    let private intersectKdTrees
        bb
        (cache: hmap<string, ConcreteKdIntersectionTree>)
        (ray: FastRay3d)
        (kdTreeMap: hmap<Box3d, Level0KdTree>)
        =
        let kdtree, c =
            kdTreeMap
            |> HMap.find bb
            |> loadObjectSet cache

        let hit = single ray kdtree
        hit, c

    let intersectKdTreesWithIndex
        bb
        (cache: hmap<string, ConcreteKdIntersectionTree>)
        (ray: FastRay3d)
        (kdTreeMap: hmap<Box3d, Level0KdTree>)
        =
        let kdtree, c =
            kdTreeMap
            |> HMap.find bb
            |> loadObjectSet cache

        let hit = IntersectionController.intersectSingleForIndex ray kdtree //bb
        hit, c

    let private hitBoxes (kd: hmap<Box3d, Level0KdTree>) (r: FastRay3d) (trafo: Trafo3d) =
        kd
        |> HMap.toList
        |> List.map fst
        |> List.filter (fun x ->
            let mutable t = 0.0
            let r' = r.Ray.Transformed(trafo.Backward) //combine pre and current transform
            x.Intersects(r', &t))

    let intersectWithOpc (kdTree0: option<hmap<Box3d, Level0KdTree>>) ray =
        kdTree0
        |> Option.bind (fun kd ->
            let boxes = hitBoxes kd ray Trafo3d.Identity

            let allhits =
                boxes
                |> List.choose (fun bb ->
                    let treeHit, c = kd |> intersectKdTrees bb cache ray
                    cache <- c
                    treeHit)
                |> List.filter (fun t -> t.IsNaN() |> not)
                |> List.sortBy (fun t -> t)

            let closest = allhits |> List.tryHead
            closest)


    let intersectWithOpcIndex (kdTree0: option<hmap<Box3d, Level0KdTree>>) ray =
        kdTree0
        |> Option.bind (fun kd ->
            let boxes = hitBoxes kd ray Trafo3d.Identity

            let closest =
                boxes
                |> List.choose (fun bb ->
                    let treeHit, c = kd |> intersectKdTreesWithIndex bb cache ray
                    cache <- c
                    match treeHit with
                    | Some (t, index) -> Some(t, index, bb)
                    | None -> None)
                |> List.sortBy (fun (t, _, _) -> t)
                |> List.tryHead

            match closest with
            | Some(t, index, bb) ->
                let lvl0KdTree = kd |> HMap.find bb

                let position = ray.Ray.GetPointOnRay(t)

                let triangle =
                    match lvl0KdTree with
                    | InCoreKdTree _ -> None
                    | LazyKdTree kd -> IntersectionController.calcTriangleHit kd index position

                Some(position, triangle, lvl0KdTree)
            | None -> None)

    let perform (m: PickingModel) (hit: SceneHit) (boxId: Box3d) = //  (hitFun: V3d->V3d) =
        let fray = hit.globalRay.Ray
        Log.line "try intersecting %A" boxId

        match m.pickingInfos |> HMap.tryFind boxId with
        | Some kk ->
            let closest = intersectWithOpc (Some kk.kdTree) fray
            match closest with
            | Some t ->
                let hitpoint = fray.Ray.GetPointOnRay t
                Log.line "hit surface at %A" hitpoint

                //===============================
                //let projOrigin = (hitFun hitpoint)// + V3d.OOI // shift axis to tunnel center

                //let intersectFunc (p:V3d) : Option<V3d> =
                //  let ray =
                //    // ViewerModality.XYZ - standard axis centered projection
                //    let dir = (p - projOrigin).Normalized
                //    FastRay3d(Ray3d(projOrigin, dir))

                //  match intersectWithOpc (Some kk.kdTree) ray with
                //  | Some t -> Some (ray.Ray.GetPointOnRay t)
                //  | None -> None
                // =====================================

                { m with
                      intersectionPoints = m.intersectionPoints |> PList.prepend hitpoint
                      hitPointsInfo = HMap.add hitpoint boxId m.hitPointsInfo }

            | None ->
                Log.error "[Intersection] didn't hit"
                m
        | None ->
            Log.error "[Intersection] box not found in picking infos"
            m

    let performIndex (m: PickingModel) (hit: SceneHit) (boxId: Box3d) =
        let fray = hit.globalRay.Ray
        Log.line "try intersecting %A" boxId

        match m.pickingInfos |> HMap.tryFind boxId with
        | Some kk ->
            let closest = intersectWithOpcIndex (Some kk.kdTree) fray
            match closest with
            | Some (point, triangle, kd) ->

                Log.line "hit surface at %A" point

                let hit =
                    match kd, triangle with
                    | LazyKdTree k, Some t ->
                        Some
                            { position = point
                              triangle = t
                              opcInfo = kk
                              kdTree = k }
                    | _ -> None

                { m with
                      intersectionPoints = m.intersectionPoints |> PList.prepend point
                      hitPointsInfo = HMap.add point boxId m.hitPointsInfo
                      lastHit = hit }

            | None ->
                Log.error "[Intersection] didn't hit"
                { m with lastHit = None }
        | None ->
            Log.error "[Intersection] box not found in picking infos"
            { m with lastHit = None }
