namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open OpcViewer.Base

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit) -> //, axisPoint) -> 
      Intersect.perform model sceneHit box // axisPoint
    | RemoveLastPoint ->
      let points, infos = 
        match model.intersectionPoints.AsList with
          | [] -> [], HMap.empty
          | first :: rest -> 
            rest, model.hitPointsInfo.Remove first
      { model with intersectionPoints = points |> PList.ofList; hitPointsInfo = infos }
    | ClearPoints -> 
      { model with intersectionPoints = PList.empty; hitPointsInfo = HMap.empty}
    //| AddTestBrushes pointsOnAxisFunc ->
    //  if model.intersectionPoints.Count > 0 then
    //    let rand = RandomSystem()

    //    let colors =
    //      [|
    //        for _ in 1 .. 20 do
    //          yield rand.UniformC3f().ToC4b()
    //      |]

    //    let testBrushes = 
    //      Seq.initInfinite (fun _ ->
    //        let mutable dir = rand.UniformV3dDirection()
    //        if dir.Z < 0.0 then dir.Z <- -dir.Z
    //        let p0 = (model.intersectionPoints.[rand.UniformInt(model.intersectionPoints.Count)])

    //        match pointsOnAxisFunc (PList.single p0) with
    //        | Some center ->
    //          let center = center.midPoint

    //          let dir = p0 - center |> Vec.normalize
    //          let t = Trafo3d.FromNormalFrame(p0, dir)

    //          let o = rand.UniformV2dDirection() * rand.UniformDouble()
    //          let p1 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()
    //          let p2 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()

    //          let p0 = t.Forward.TransformPos(V3d(o, 0.0))
    //          let p1 = t.Forward.TransformPos(V3d(p1, 0.0))
    //          let p2 = t.Forward.TransformPos(V3d(p2, 0.0))

    //          let pts = PList.ofList [ p0; p1; p2; p0]
    //          match pointsOnAxisFunc (pts |> PList.skip 1) with
    //          | Some aps ->
    //            Some { 
    //              // 1m shift for scene with axis outside of tunnel....REMOVE
    //              pointsOnAxis = Some aps
    //              points = pts
    //              segments = PList.empty
    //              color = colors.[rand.UniformInt colors.Length]
    //            }
    //          | None ->
    //            None
    //        | _ ->
    //          None)
    //      |> Seq.choose id
    //      |> Seq.take 200
    //      |> PList.ofSeq

    //    let newGrouped =
    //      testBrushes |> Seq.fold (fun groupedBrushes newBrush -> 
    //        groupedBrushes|> HMap.alter newBrush.color (fun x -> 
    //          match x with 
    //          | Some y -> Some (y |> PList.append newBrush)
    //          | None -> Some (PList.single newBrush))
    //       ) model.groupedBrushes

    //    { model with brush = model.brush |> PList.concat2 testBrushes; intersectionPoints = PList.empty; segments = PList.empty; groupedBrushes = newGrouped }
    //  else 
    //    model

    //| AddBrush pointsOnAxisFunc ->
    //  if model.intersectionPoints.Count > 2 then
        
    //    let subdivision = false

    //    let p, pa =
    //      if subdivision then
    //        // SEGMENT TEST-CASE - add starting point to end (last edge is NOT subdivided - only for testing)
    //        let points = 
    //          model.segments 
    //          |> PList.map(fun x -> x.points |> PList.ofList) 
    //          |> PList.concat

    //        let p = points |> PList.prepend (model.intersectionPoints |> PList.last)
    //        let pa = points |> pointsOnAxisFunc
    //        (p,pa) 
    //      else
    //        // REGULAR CASE - add starting point to end
    //        let p = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last)
    //        let pa = pointsOnAxisFunc model.intersectionPoints
    //        (p,pa)
        
    //    let p, pa = 
    //      // Fix winding order (if axis is available!)
    //      match pa with
    //      | None -> (p,pa)
    //      | Some paa -> 
    //        // for higher Precision shift by AxisPoint
    //        let axisPoint = paa.pointsOnAxis |> PList.skip 1 |> PList.first
    //        let p0 = p |> PList.first                  |> fun x -> x - axisPoint
    //        let p1 = p |> PList.skip 1 |> PList.first  |> fun x -> x - axisPoint
    //        let p2 = p |> PList.skip 2 |> PList.first  |> fun x -> x - axisPoint

    //        let dir1 = p1.Normalized  // already shifted by axisPoint
    //        let x1 = (p0-p1).Normalized
    //        let x2 = (p2-p1).Normalized
    //        let dir2 = (x1.Cross(x2)).Normalized

    //        if dir1.Dot(dir2) |> sign < 0 then
    //          let pRev = p |> PList.toList |> List.rev |> PList.ofList
    //          let aRev = { paa with pointsOnAxis = paa.pointsOnAxis |> PList.toList |> List.rev |> PList.ofList }
    //          printfn "\n\n\nFixed winding order \n\n\n"
    //          (pRev, Some aRev)
    //        else 
    //          (p,pa)

    //    let newBrush = 
    //      { 
    //        // 1m shift for scene with axis outside of tunnel....REMOVE
    //        pointsOnAxis = pa |> Option.map(fun x -> {x with pointsOnAxis = x.pointsOnAxis |> PList.map(fun v -> v + V3d.OOI); midPoint = x.midPoint + V3d.OOI})
    //        points = p
    //        segments = model.segments
    //        color = 
    //          match model.brush.Count % 2 with
    //          | 0 -> C4b.DarkGreen
    //          | 1 -> C4b.DarkCyan
    //          | 2 -> C4b.DarkBlue
    //          | 3 -> C4b.DarkMagenta
    //          | 4 -> C4b.DarkRed
    //          | _ -> C4b.DarkYellow
    //      }
        
    //    let newGrouped =
    //        model.groupedBrushes |> HMap.alter newBrush.color (fun x -> 
    //          match x with 
    //          | Some y -> Some (y |> PList.prepend newBrush)
    //          | None -> Some (PList.single newBrush))
           
    //    { model with brush = model.brush |> PList.prepend newBrush; intersectionPoints = PList.empty; segments = PList.empty; groupedBrushes = newGrouped }
    //  else
    //    model

  let view (model : MPickingModel) =
    
    // TODO...maybe add last click point as animation?
    failwith ""