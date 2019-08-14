namespace OpcViewer.Base.Picking

open System
open System.Drawing

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Application
open Aardvark.UI  
open Aardvark.Geometry
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc
open Aardvark.VRVis.Opc.KdTrees 
open OpcViewer.Base.Picking

//module legacy =
  //let computeIndexArray(size:V2i) (invalidPoints:seq<int>) : int[] =

  //  let indexArray = Array.zeroCreate ((size.X - 1) * (size.Y - 1) * 6)

  //  let mutable k = 0;

  //  match invalidPoints.IsEmptyOrNull() with
  //  | true -> 
  //    for y in 0 .. (size.Y-2) do
  //      for x in 0..(size.X-2) do
  //        indexArray.[k] <- y * size.X + x
  //        indexArray.[k + 1] <- (y + 1) * size.X + x
  //        indexArray.[k + 2] <- y * size.X + x + 1

  //        indexArray.[k + 3] <- y * size.X + x + 1
  //        indexArray.[k + 4] <- (y + 1) * size.X + x
  //        indexArray.[k + 5] <- (y + 1) * size.X + x + 1

  //        k <- k + 6;
  //  | false ->
  //    let invalidDict = invalidPoints |> HashSet.ofSeq
  //    let mutable counter = 0

  //    for y in 0 .. (size.Y-2) do
  //      for x in 0..(size.X-2) do
  //        let a1 = y * size.X + x
  //        let b1 = (y + 1) * size.X + x
  //        let c1 = y * size.X + x + 1

  //        let a2 = y * size.X + x + 1
  //        let b2 = (y + 1) * size.X + x
  //        let c2 = (y + 1) * size.X + x + 1

  //        let indices = [a1; b1; c1; a2; b2; c2]

  //        let invalidFace = indices |> List.filter(fun x -> invalidDict.Contains(x)) |> List.length > 0

  //        if invalidFace then
  //            counter <- counter + 1
  //        else 
  //          indexArray.[k] <- a1
  //          indexArray.[k + 1] <- b1
  //          indexArray.[k + 2] <- c1

  //          indexArray.[k + 3] <- a2
  //          indexArray.[k + 4] <- b2
  //          indexArray.[k + 5] <- c2

  //        k <- k + 6

  //    if (counter > 0) then
  //        Report.Line(5, "Invalid faces found: {0}", counter)

  //  indexArray

module IntersectionController =   

  let loadTrianglesFromFileWithIndices (aaraFile : string) (matrix : M44d) =
    let positions = aaraFile |> fromFile<V3f>
    
    let data = 
      positions.Data |> Array.map (fun x ->  x.ToV3d() |> matrix.TransformPos)
 
    let invalidIndices = getInvalidIndices data
    let index = LegacyCode.Class1.ComputeIndexArray(positions.Size.XY.ToV2i(), invalidIndices)
        
    let triangleIndices = 
      index
        |> Seq.chunkBySize 3

    let triangles =         
      triangleIndices
        |> Seq.choose(fun x -> 
          if x.Length = 3 then Some [|data.[x.[0]]; data.[x.[1]]; data.[x.[2]]|]
          else None)
        |> Seq.map (fun x -> Triangle3d(x))
        |> Seq.toArray      
    
    (triangles, (Seq.toArray triangleIndices))
  
  let loadTrianglesWithIndices (kd : LazyKdTree) =
    loadTrianglesFromFileWithIndices kd.objectSetPath kd.affine.Forward
  
  let triangleIsNan (t:Triangle3d) =
      t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN
   
  let loadTriangles (kd : LazyKdTree) = 
    let indexing = (fun size invalidIndices -> LegacyCode.Class1.ComputeIndexArray(size, invalidIndices))
    loadTrianglesFromFile' kd.objectSetPath indexing kd.affine.Forward
    
  let loadTriangleSet (kd : LazyKdTree) =
    kd |> loadTriangles |> TriangleSet
              
  let intersectSingleForIndex ray (hitObject : 'a) (kdTree:ConcreteKdIntersectionTree) = 
      let kdi = kdTree.KdIntersectionTree 
      let mutable hit = ObjectRayHit.MaxRange
      let objFilter _ _ = true              
      try           
        if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then  
            Some (hit.RayHit.T, hit.SetObject.Index)
        else            
            None
      with 
        | e -> 
          Log.error "null ref exception in kdtree intersection %A" e
          None 

  let calculateBarycentricCoordinates (triangle : Triangle3d) (pos : V3d) = 
    let v0 = triangle.P1 - triangle.P0
    let v1 = triangle.P2 - triangle.P0
    let v2 = pos         - triangle.P0

    let d00 = V3d.Dot(v0, v0)
    let d01 = V3d.Dot(v0, v1)
    let d11 = V3d.Dot(v1, v1)
    let d20 = V3d.Dot(v2, v0)
    let d21 = V3d.Dot(v2, v1)
    
    let denom = d00 * d11 - d01 * d01;

    let v = (d11 * d20 - d01 * d21) / denom
    let w = (d00 * d21 - d01 * d20) / denom
    let u = 1.0 - v - w
    
    V3d(v,w,u)
    
  let findCoordinates (kdTree : LazyKdTree) (index : int) (position : V3d) =
    let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
    let triangle = triangles.[index]
    
    let baryCentricCoords = calculateBarycentricCoordinates triangle position

    Log.line "barycentricCoords: u: %f, v: %f, w: %f" baryCentricCoords.X baryCentricCoords.Y baryCentricCoords.Z 
    
    let coordinates = kdTree.coordinatesPath |> fromFile<V2f>

    let coordinateIndices = triangleIndices.[index]

    let p0 = coordinates.Data.[coordinateIndices.[0]] * (float32 baryCentricCoords.X)
    let p1 = coordinates.Data.[coordinateIndices.[1]] * (float32 baryCentricCoords.Y)
    let p2 = coordinates.Data.[coordinateIndices.[2]] * (float32 baryCentricCoords.Z)
    
    let exactUV = p0+p1+p2
    
    let image = PixImage.Create(kdTree.texturePath).ToPixImage<byte>(Col.Format.RGB)
    
    let changePos = V2i (((float32 image.Size.X) * exactUV.X),((float32 image.Size.Y) * exactUV.Y))

    image.GetMatrix<C3b>().SetCross(changePos, 5, C3b.Red) |> ignore

    image.SaveAsImage (@".\testcoordSelect.png")

    exactUV

module Intersect =

  let mutable private cache = HMap.empty

  let single ray (kdTree:ConcreteKdIntersectionTree) =
    let kdi = kdTree.KdIntersectionTree 
    let mutable hit = ObjectRayHit.MaxRange
    let objFilter _ _ = true              
    try           
      if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then              
        Some (hit.RayHit.T)
      else            
        None
    with 
      | _ -> 
        Log.error "null ref exception in kdtree intersection" 
        None 

  let private getInvalidIndices3f (positions : V3f[]) =
    positions 
      |> List.ofArray 
      |> List.mapi (fun i x -> if x.AnyNaN then Some i else None) 
      |> List.choose id
  
  let private triangleIsNan (t:Triangle3d) = 
    t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN

  let private getTriangleSet (indices : int[]) (vertices:V3d[]) = 
    indices 
      |> Seq.map(fun x -> vertices.[x])
      |> Seq.chunkBySize 3
      |> Seq.map(fun x -> Triangle3d(x))
      |> Seq.filter(fun x -> (triangleIsNan x |> not)) |> Seq.toArray
      |> TriangleSet

  let private loadTriangles (kd : LazyKdTree) =
    
    let positions = kd.objectSetPath |> Aara.fromFile<V3f>
            
    let invalidIndices = getInvalidIndices3f positions.Data |> List.toArray
    let size = positions.Size.XY.ToV2i()
    let indices = LegacyCode.Class1.ComputeIndexArray(size,invalidIndices)
                   
    positions.Data 
      |> Array.map (fun x ->  x.ToV3d() |> kd.affine.Forward.TransformPos) 
      |> getTriangleSet indices

  let private loadObjectSet (cache : hmap<string, ConcreteKdIntersectionTree>) (lvl0Tree : Level0KdTree) =             
    match lvl0Tree with
      | InCoreKdTree kd -> 
        kd.kdTree, cache
      | LazyKdTree kd ->             
        let kdTree, cache =
          match kd.kdTree with
          | Some k -> k, cache
          | None -> 
            let key = (kd.boundingBox.ToString())
            let tree = cache |> HMap.tryFind key
            match tree with
            | Some t ->                 
              t, cache
            | None ->                                     
              Log.line "cache miss %A- loading kdtree" kd.boundingBox
          
              let mutable tree = KdTrees.loadKdtree kd.kdtreePath      
              let triangles = kd |> loadTriangles
              
              tree.KdIntersectionTree.ObjectSet <- triangles                                                            
              tree, (HMap.add key tree cache)
        kdTree, cache

  let private intersectKdTrees bb (cache : hmap<string, ConcreteKdIntersectionTree>) (ray : FastRay3d) (kdTreeMap: hmap<Box3d, Level0KdTree>) = 
      let kdtree, c = kdTreeMap |> HMap.find bb |> loadObjectSet cache
      let hit = single ray kdtree
      hit,c

  let private hitBoxes (kd : hmap<Box3d, Level0KdTree>) (r : FastRay3d) (trafo : Trafo3d) =
    kd
      |> HMap.toList 
      |> List.map fst
      |> List.filter(
        fun x -> 
          let mutable t = 0.0
          let r' = r.Ray.Transformed(trafo.Backward) //combine pre and current transform
          x.Intersects(r', &t)
      )

  let intersectWithOpc (kdTree0 : option<hmap<Box3d, Level0KdTree>>) ray =
    kdTree0 
      |> Option.bind(fun kd ->
          let boxes = hitBoxes kd ray Trafo3d.Identity
          
          let allhits = 
            boxes 
              |> List.choose(
                  fun bb -> 
                    let treeHit,c = kd |> intersectKdTrees bb cache ray
                    cache <- c
                    treeHit)
              |> List.filter(fun t -> t.IsNaN() |> not)
              |> List.sortBy(fun t-> t)
            
          let closest = 
              allhits |> List.tryHead            
          closest
      )
  
  let perform (m : PickingModel) (hit : SceneHit) (boxId : Box3d) = //(axisfunc:V3d->V3d) = 
    let fray = hit.globalRay.Ray
    Log.line "try intersecting %A" boxId    
    
    match m.pickingInfos |> HMap.tryFind boxId with
    | Some kk ->
      let closest = intersectWithOpc (Some kk.kdTree) fray      
      match closest with
        | Some t -> 
          let hitpoint = fray.Ray.GetPointOnRay t
          Log.line "hit surface at %A" hitpoint 
          
          //let axisPoint = (axisfunc hitpoint) + V3d.OOI // shift axis to tunnel center
          
          //let intersectFunc (p:V3d) : Option<V3d> = 
          //  let ray = 
          //    // ViewerModality.XYZ - standard axis centered projection
          //    let dir = (p - axisPoint).Normalized          
          //    FastRay3d(Ray3d(axisPoint, dir))              
          
          //  match intersectWithOpc (Some kk.kdTree) ray with
          //  | Some t -> Some (ray.Ray.GetPointOnRay t)
          //  | None -> None

          { m with
             intersectionPoints = m.intersectionPoints |> PList.prepend hitpoint
             hitPointsInfo = HMap.add hitpoint boxId m.hitPointsInfo
          }   

        | None ->       
          Log.error "[Intersection] didn't hit"
          m
    | None -> 
      Log.error "[Intersection] box not found in picking infos"
      m