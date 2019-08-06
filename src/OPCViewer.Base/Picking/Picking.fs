namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open OpcViewer.Base

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit, axisPoint) -> 
      Intersect.perform model sceneHit box axisPoint
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

    //let lineWidth = 2.0
    //let depthOffset = 0.01

    //let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
    //let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

    //let debugClippingVolumeSg polygonSG = 
    //  let debugVolume = 
    //    polygonSG
    //      |> Sg.onOff model.debugShadowVolume
    //      |> Sg.effect [
    //        toEffect DefaultSurfaces.stableTrafo
    //        Shader.DebugColor.Effect
    //        ]

    //  let debugShadowVolume =
    //    debugVolume
    //      |> Sg.uniform "UseDebugColor" (Mod.constant(false))
    //      |> Sg.depthTest (Mod.constant DepthTestMode.Less)
    //      |> Sg.cullMode (Mod.constant (CullMode.Front))
    //      |> Sg.blendMode (Mod.constant(BlendMode.Blend))
        
    //  let debugShadowVolumeLines =
    //    debugVolume
    //      |> Sg.uniform "UseDebugColor" (Mod.constant(true))
    //      |> Sg.fillMode (Mod.constant (FillMode.Line))

    //  [ debugShadowVolume; debugShadowVolumeLines] |> Sg.ofList

    //let brushOutlineSg (b:MBrush) = 
    //  let detailOutline = 
    //    b.segments 
    //      |> AList.map(fun x -> x.points |> AList.ofList) 
    //      |> AList.concat
    //      |> Sg.drawOutlineDetail (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showDetailOutline

    //  let outline = 
    //    b.points
    //      |> Sg.drawOutlineSimple (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showOutline

    //  [outline; detailOutline] |> Sg.ofList

    //let debugAxisSg (b:MBrush) = 
      
    //  let pointSG size t = 
    //    Sg.sphere 3 b.color (Mod.constant size)
    //    |> Sg.noEvents
    //    |> Sg.trafo t
    //    |> Sg.uniform "WorldPos" (t |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
    //    |> Sg.uniform "Size" (Mod.constant(size))
    //    |> Sg.effect [
    //      toEffect <| DefaultSurfaces.stableTrafo
    //      toEffect <| DefaultSurfaces.vertexColor
    //    ]
      
    //  b.pointsOnAxis 
    //    |> Mod.map(fun x ->
    //      x |> Option.map(fun a -> 
    //        let points = 
    //          a.pointsOnAxis 
    //            |> AList.map(fun x -> 
    //              let t = (Mod.constant(Trafo3d.Translation x))
    //              pointSG 0.1 t) 

    //        let midPoint =
    //          let t = a.midPoint |> Mod.map(fun x -> Trafo3d.Translation x)
    //          pointSG 0.2 t

    //        points 
    //          |> AList.append (AList.single midPoint)
    //          |> ASet.ofAList 
    //          |> Sg.set)
    //        |> Option.defaultValue Sg.empty) 
    //    |> Sg.dynamic
    //    |> Sg.onOff model.debugShadowVolume

    //let drawSeqBrushes = 
    //  model.brush 
    //  |> AList.map(fun b ->
        
    //    let clippingPolygon = 
    //      model.volumeGeneration 
    //        |> Mod.map (fun x -> 
    //          match x with
    //          | None -> Sg.empty
    //          | Some vg -> 
    //            match vg with
    //            | Plane -> Sg.drawFinishedBrushPlane b model.alpha model.extrusionOffset
    //            | _ -> Sg.drawFinishedBrushAxis b model.alpha model.extrusionOffset vg
    //          ) |> Sg.dynamic

    //    let coloredPolygon =
    //      clippingPolygon 
    //        |> Sg.effect [
    //          toEffect DefaultSurfaces.stableTrafo
    //          toEffect DefaultSurfaces.vertexColor
    //          ]
    //        |> StencilAreaMasking.stencilAreaSG maskPass areaPass
  
    //    maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
    //    areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
           
    //    [
    //      coloredPolygon
    //      debugClippingVolumeSg clippingPolygon
    //      brushOutlineSg b
    //      //debugAxisSg b
    //    ] |> Sg.ofList) 
    //  |> AList.toASet 
    //  |> Sg.set

    //let drawGroupedBrushes =
    //  model.groupedBrushes 
    //  |> AMap.map(fun groupColor x -> 
    //    let clippingPolygon = 
    //      x |> AList.map (fun b ->
    //        model.volumeGeneration 
    //          |> Mod.map (fun x -> 
    //            match x with
    //            | None -> Sg.empty
    //            | Some vg -> 
    //              match vg with
    //              | Plane -> Sg.drawFinishedBrushPlane b model.alpha model.extrusionOffset
    //              | _ -> Sg.drawFinishedBrushAxis b model.alpha model.extrusionOffset vg)
    //          |> Sg.dynamic)
    //        |> AList.toASet 
    //        |> Sg.set
        
    //    let coloredPolygon =
          
    //      let groupColorAlpha = model.alpha |> Mod.map(fun a -> groupColor.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f())
          
    //      clippingPolygon
    //        |> Sg.effect [
    //          toEffect DefaultSurfaces.stableTrafo
    //          toEffect DefaultSurfaces.vertexColor
    //          ]
    //        |> StencilAreaMasking.stencilAreaGroupedSG maskPass areaPass groupColorAlpha

    //    let brushOutlines = 
    //      x |> AList.map( fun b -> brushOutlineSg b) |> AList.toASet |> Sg.set

    //    maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
    //    areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

    //    [
    //     coloredPolygon
    //     debugClippingVolumeSg clippingPolygon
    //     brushOutlines
    //    ] |> Sg.ofList)
    //|> AMap.toASet
    //|> ASet.map snd
    //|> Sg.set

    //let sketchOutlineDetail = 
    //  model.segments 
    //    |> AList.map(fun x -> x.points |> AList.ofList) 
    //    |> AList.concat
    //    |> Sg.drawOutlineDetail (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showDetailOutline

    //let sketchOutline = 
    //  model.intersectionPoints 
    //    |> Sg.drawOutlineSimple (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) (Mod.constant(true))

    //let finishedBrushes = 
    //  model.useGrouping |> Mod.map(fun x ->
    //    match x with
    //    | true -> drawGroupedBrushes
    //    | false -> drawSeqBrushes ) |> Sg.dynamic

    //[ 
    //  sketchOutline
    //  sketchOutlineDetail 
    //  finishedBrushes
    //] |> Sg.ofList