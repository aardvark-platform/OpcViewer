namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open System.Collections.Generic

open Uncodium

open OpcViewer.Base

module StencilAreaMasking =

  let writeZFail =
      let compare = new StencilFunction(StencilCompareFunction.Always, 0, 0xffu)
      let front   = new StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.DecrementWrap, StencilOperationFunction.Keep)
      let back    = new StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.IncrementWrap, StencilOperationFunction.Keep)
      StencilMode(front, compare, back, compare)

  //let writeZPass =
  //    let compare = new StencilFunction(StencilCompareFunction.Always, 0, 0xffu)
  //    let front   = new StencilOperation(StencilOperationFunction.IncrementWrap, StencilOperationFunction.Keep, StencilOperationFunction.Keep)
  //    let back    = new StencilOperation(StencilOperationFunction.DecrementWrap, StencilOperationFunction.Keep, StencilOperationFunction.Keep)
  //    StencilMode(front, compare, back, compare)

  let readMaskAndReset = 
      let compare = new StencilFunction(StencilCompareFunction.NotEqual, 0, 0xffu)
      let operation = new StencilOperation(StencilOperationFunction.Zero, StencilOperationFunction.Zero, StencilOperationFunction.Zero)
      StencilMode(operation, compare)

  //let maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
  //let areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

  let maskSG maskPass sg = 
    sg
      |> Sg.pass maskPass
      |> Sg.stencilMode (Mod.constant (writeZFail))
      |> Sg.cullMode (Mod.constant (CullMode.None))
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])

  let fillSG areaPass sg =
    sg
      |> Sg.pass areaPass
      |> Sg.stencilMode (Mod.constant (readMaskAndReset))
      //|> Sg.cullMode (Mod.constant CullMode.CounterClockwise)  // for zpass -> backface-culling
      //|> Sg.depthTest (Mod.constant DepthTestMode.Less)        // for zpass -> active depth-test
      |> Sg.cullMode (Mod.constant CullMode.None)
      |> Sg.depthTest (Mod.constant DepthTestMode.None)
      |> Sg.blendMode (Mod.constant BlendMode.Blend)
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Stencil])

  let stencilArea pass1 pass2 sg1 sg2 =
    [
      maskSG pass1 sg1   // one pass by using EXT_stencil_two_side :)
      fillSG pass2 sg2
    ] |> Sg.ofList

  let stencilAreaSG pass1 pass2 sg =
    stencilArea pass1 pass2 sg sg

  let stencilAreaGroupedSG pass1 pass2 (color:IMod<V4f>) sg =

    let fullscreenSg =     
      Aardvark.SceneGraph.SgPrimitives.Sg.fullScreenQuad
      |> Sg.noEvents
      |> Sg.vertexBufferValue DefaultSemantic.Colors color
      |> Sg.effect [ toEffect DefaultSurfaces.vertexColor]

    stencilArea pass1 pass2 sg fullscreenSg

module Sg =
  

  let drawOutline (colorLine : IMod<C4b>) (colorPoint : IMod<C4b>) (pointSize : IMod<float>) (offset:IMod<float>) (width : IMod<float>) (visible: IMod<bool>) (points:alist<V3d>) =           
    // Increase Precision
    let shift = 
      points
        |> AList.toMod 
        |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)

    let pointsF =
      points 
      |> AList.toMod 
      |> Mod.map(fun x ->
        let shift = (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero
        x |> PList.toSeq |> Seq.map(fun y -> V3f (y-shift)) |> Seq.toArray)

    let indexArray = 
      pointsF |> Mod.map(fun x -> ( Array.init (max 0 (x.Length * 2 - 1)) (fun a -> (a + 1)/ 2)))

    let lines = 
      Sg.draw IndexedGeometryMode.LineList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF 
      |> Sg.index indexArray
      |> Sg.uniform "Color" colorLine
      |> Sg.uniform "LineWidth" width
//      |> Sg.uniform "depthOffset" offset
      |> Sg.effect [
        toEffect DefaultSurfaces.stableTrafo
        toEffect DefaultSurfaces.thickLine
        toEffect DefaultSurfaces.sgColor
        ]

    let points = 
      Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF 
      |> Sg.uniform "Color" colorPoint
      |> Sg.uniform "PointSize" pointSize
      |> Sg.uniform "depthOffset" offset
      |> Sg.effect [
        toEffect DefaultSurfaces.stableTrafo
        Shader.PointSprite.Effect
        toEffect DefaultSurfaces.sgColor
        ]

    let sg = [ lines; points] |> Sg.ofSeq |> Sg.translate' shift

    visible |> Mod.map(fun x -> 
      match x with
      | true -> sg
      | false -> Sg.empty) |> Sg.dynamic

  let drawOutlineSimple (offset:IMod<float>) (width : IMod<float>) (visible: IMod<bool>) (points:alist<V3d>) = drawOutline (Mod.constant(C4b.Yellow)) (Mod.constant(C4b.Red)) (Mod.constant (10.0)) offset width visible points
  let drawOutlineDetail (offset:IMod<float>) (width : IMod<float>) (visible: IMod<bool>) (points:alist<V3d>) = drawOutline (Mod.constant(C4b.DarkYellow)) (Mod.constant(C4b.DarkRed)) (Mod.constant (5.0)) offset width visible points

  let planeFit (points:seq<V3d>) : Plane3d =
    let length = points |> Seq.length |> float

    let c = 
        let sum = points |> Seq.reduce (fun x y -> V3d.Add(x,y))
        sum / length

    let pDiffAvg = points |> Seq.map(fun x -> x - c)
        
    //let mutable matrix = M33d.Zero
    //pDiffAvg |> Seq.iter(fun x -> matrix.AddOuterProduct(&x))
    let mutable matrix = M33d.Zero
    pDiffAvg |> Seq.iter(fun x -> 
      let mutable y = x //.ToV3f()
      (&matrix).AddOuterProduct(&y))

    matrix <- matrix / length
         
    let mutable q = M33d.Zero
    let mutable w = V3d.Zero
    let passed = Eigensystems.Dsyevh3(&matrix, &q, &w)
        
    let n = 
        if w.X < w.Y then
            if w.X < w.Z then q.C0
            else q.C2
        else if w.Y < w.Z then q.C1
        else q.C2

    Plane3d(n, c)

  let planeFitPolygon (points:plist<V3d>) offset = 
    let plane = planeFit points
    let extrudeNormal = plane.Normal
    let projPointsOnPlane = points |> PList.map(plane.Project) |> PList.toList

    // Top and Bottom triangle-fan startPoint
    let startPoint = projPointsOnPlane |> List.head
    let startPos = startPoint + extrudeNormal * offset
    let startNeg = startPoint - extrudeNormal * offset
         
    if projPointsOnPlane |> List.length < 3 then
      []
    else 
      projPointsOnPlane
      |> List.pairwise
      |> List.mapi (fun i (a,b) -> 
        // Shift points 
        let aPos = a + extrudeNormal * offset
        let bPos = b + extrudeNormal * offset
        let aNeg = a - extrudeNormal * offset
        let bNeg = b - extrudeNormal * offset
               
        // Generate Triangles for watertight polygon
        [
          if i <> 0 then // first edge has to be skipped for top and bottom triangle generation
              yield Triangle3d(startPos, bPos, aPos) // top
              yield Triangle3d(startNeg, aNeg, bNeg) // bottom
             
          yield Triangle3d(aPos, bNeg, aNeg) // side1
          yield Triangle3d(aPos, bPos, bNeg) // side2
        ]
      ) |> List.concat

  let drawFinishedBrushPlane (brush:MBrush) (alpha:IMod<float>) (offset:IMod<float>) =

    let colorAlpha = Mod.map2(fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) brush.color alpha

    let generatePolygonTriangles (offset : float) (points:alist<V3d>) =
      let shiftAndPosAndCol =
        points 
        |> AList.toMod
        |> Mod.bind(fun x -> 
          // increase Precision
          let shift = x |> PList.tryAt 0 |> Option.defaultValue V3d.Zero
          let shiftedPoints = x |> PList.toSeq |> Seq.map(fun (y:V3d) -> (y-shift)) |> PList.ofSeq

          let triangles = planeFitPolygon shiftedPoints offset
          let pos = triangles |> Seq.collect (fun t -> [| V3f t.P0; V3f t.P1; V3f t.P2 |]) |> Seq.toArray
          colorAlpha |> Mod.map (fun cc -> shift, pos, cc))

      Sg.draw IndexedGeometryMode.TriangleList
        |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.map (fun (_,p,_) -> p) shiftAndPosAndCol)
        |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.map (fun (_,_,c) -> c) shiftAndPosAndCol)
        |> Sg.translate' (Mod.map (fun (s,_,_) -> s) shiftAndPosAndCol)

    let sg = offset |> Mod.map(fun o -> generatePolygonTriangles o brush.points) 
  
    sg |> Mod.toASet |> Sg.set

  let drawFinishedBrushAxis (brush:MBrush) (alpha:IMod<float>) (offset:IMod<float>) (volumeGeneration:VolumeGeneration) :ISg<'a> =
    
    let colorAlpha = Mod.map2(fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) brush.color alpha
    
    let sg = 
      brush.pointsOnAxis |> Mod.bind(fun axisInfo -> 
      match axisInfo with
      | None -> Mod.constant(Sg.empty)
      | Some (api:MAxisPointInfo) ->
        offset |> Mod.map(fun o -> 
          
          // increase Precision
          let shiftVec p = 
            p |> AList.toMod 
              |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)

          let shiftPoints p shiftVec =
            let asdf = p |> AList.toMod
            Mod.map2(fun x (s:V3d) -> 
              x |> PList.toSeq 
                |> Seq.map(fun (y:V3d) -> V3f(y-s)) 
                |> Seq.toArray) asdf shiftVec

          let shift = shiftVec brush.points 
          let pointsF = (shiftPoints brush.points shift) |> Mod.map(fun x -> x |> Array.skip 1) // undo closing polygon (duplicates not needed)

          let vertices = 
            match volumeGeneration with
            | AxisMidPoint -> 
              let axisMidPointF = Mod.map2(fun (m:V3d) (s:V3d) -> V3f(m-s)) api.midPoint shift
              
              Mod.map2(fun ps (midPoint:V3f) -> 
                let dir = ps |> Array.map(fun (p:V3f) -> ((midPoint-p).Normalized))
                let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))
                
                let inner = Array.create ps.Length midPoint // use center point....
                //let inner = Array.map2(+) ps offsetP  // shift from lasso points
                let outer = Array.map2(-) ps offsetP
                inner |> Array.append outer
              ) pointsF axisMidPointF
            | AxisPoints -> 
              let axisPsF = shiftPoints api.pointsOnAxis shift
              
              Mod.map2(fun ps aps ->
                let dir = Array.map2(fun (p:V3f) (a:V3f) -> ((a-p).Normalized)) ps aps 
                let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))
                
                let inner = Array.map2 (-) aps dir // use axisPoints and shift 1m to points
                //let inner = Array.map2(+) ps offsetP  // shift from lasso points
                let outer = Array.map2(-) ps offsetP
                inner |> Array.append outer) pointsF axisPsF
            | AxisPointsMidRing -> 
              let axisPsF = shiftPoints api.pointsOnAxis shift
              
              Mod.map2(fun ps aps ->
                let dir = Array.map2(fun (p:V3f) (a:V3f) -> ((a-p).Normalized)) ps aps 
                let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))

                let inner = Array.map2 (-) aps dir // use axisPoints and shift 1m to points
                //let inner = Array.map2(+) ps offsetP
                let outer = Array.map2(-) ps offsetP
                ps |> Array.append inner |> Array.append outer ) pointsF axisPsF
            | Plane -> failwith "cannot reach this"

          let indexArray = 
            match volumeGeneration with 
            | Plane -> failwith ""
            | AxisPoints  // same as MidPoint
            | AxisMidPoint -> 
              pointsF |> Mod.map(fun x -> 
              
                let l = x.Length
                let indices = List<int>()
                // TOP
                for i in 0 .. (l - 3) do 
                  indices.Add(0)      
                  indices.Add(i + 1)  
                  indices.Add(i + 2)  
              
                // BOTTOM
                for i in 0 .. ( l - 3 ) do
                  indices.Add(l)
                  indices.Add(2*l - i - 1)
                  indices.Add(2*l - i - 2)
              
                // SIDE
                for i in 0 .. l - 1 do
                  indices.Add(i)          
                  indices.Add(l + i)      
                  indices.Add((i + 1) % l)

                  indices.Add(l + i)            
                  indices.Add(l + ((i + 1) % l))
                  indices.Add((i + 1) % l)      

                indices.ToArray())
            | AxisPointsMidRing -> 
              pointsF |> Mod.map(fun x -> 
              
                let l = x.Length
                let indices = List<int>()
                // TOP
                for i in 0 .. (l - 3) do 
                  indices.Add(0)      
                  indices.Add(i + 1)  
                  indices.Add(i + 2)  
              
                // BOTTOM
                for i in 0 .. ( l - 3 ) do
                  indices.Add(l)
                  indices.Add(2*l - i - 1)
                  indices.Add(2*l - i - 2)
              
                // SIDE
                for i in 0 .. l - 1 do
                  indices.Add(i)         // 0  
                  indices.Add(2*l + i) // 6
                  indices.Add((i + 1) % l) // 1
                  
                  indices.Add(2*l + i) // 6
                  indices.Add(l + i)     // 3 
                  indices.Add(2*l + ((i + 1) % l)) // 7

                  indices.Add(l + i)           //3 
                  indices.Add(l + ((i + 1) % l)) // 4
                  indices.Add(2*l + ((i + 1) % l)) // 7
                  
                  indices.Add(2*l + i) // 6
                  indices.Add(2*l + ((i + 1) % l)) // 7
                  indices.Add((i + 1) % l)  // 1

                indices.ToArray())

          let triangles = 
            Sg.draw IndexedGeometryMode.TriangleList
              |> Sg.vertexAttribute DefaultSemantic.Positions vertices 
              |> Sg.vertexBufferValue DefaultSemantic.Colors colorAlpha
              |> Sg.index indexArray
              |> Sg.translate' shift
              
          triangles)
        ) 
  
    sg |> Mod.toASet |> Sg.set

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
    | AddTestBrushes pointsOnAxisFunc ->
      if model.intersectionPoints.Count > 0 then
        let rand = RandomSystem()

        let colors =
          [|
            for _ in 1 .. 20 do
              yield rand.UniformC3f().ToC4b()
          |]

        let testBrushes = 
          Seq.initInfinite (fun _ ->
            let mutable dir = rand.UniformV3dDirection()
            if dir.Z < 0.0 then dir.Z <- -dir.Z
            let p0 = (model.intersectionPoints.[rand.UniformInt(model.intersectionPoints.Count)])

            match pointsOnAxisFunc (PList.single p0) with
            | Some center ->
              let center = center.midPoint

              let dir = p0 - center |> Vec.normalize
              let t = Trafo3d.FromNormalFrame(p0, dir)

              let o = rand.UniformV2dDirection() * rand.UniformDouble()
              let p1 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()
              let p2 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()

              let p0 = t.Forward.TransformPos(V3d(o, 0.0))
              let p1 = t.Forward.TransformPos(V3d(p1, 0.0))
              let p2 = t.Forward.TransformPos(V3d(p2, 0.0))

              let pts = PList.ofList [ p0; p1; p2; p0]
              match pointsOnAxisFunc (pts |> PList.skip 1) with
              | Some aps ->
                Some { 
                  // 1m shift for scene with axis outside of tunnel....REMOVE
                  pointsOnAxis = Some aps
                  points = pts
                  segments = PList.empty
                  color = colors.[rand.UniformInt colors.Length]
                }
              | None ->
                None
            | _ ->
              None)
          |> Seq.choose id
          |> Seq.take 200
          |> PList.ofSeq

        let newGrouped =
          testBrushes |> Seq.fold (fun groupedBrushes newBrush -> 
            groupedBrushes|> HMap.alter newBrush.color (fun x -> 
              match x with 
              | Some y -> Some (y |> PList.append newBrush)
              | None -> Some (PList.single newBrush))
           ) model.groupedBrushes

        { model with brush = model.brush |> PList.concat2 testBrushes; intersectionPoints = PList.empty; segments = PList.empty; groupedBrushes = newGrouped }
      else 
        model
    | AddBrush pointsOnAxisFunc ->
      if model.intersectionPoints.Count > 2 then
        
        let subdivision = false

        let p, pa =
          if subdivision then
            // SEGMENT TEST-CASE - add starting point to end (last edge is NOT subdivided - only for testing)
            let points = 
              model.segments 
              |> PList.map(fun x -> x.points |> PList.ofList) 
              |> PList.concat

            let p = points |> PList.prepend (model.intersectionPoints |> PList.last)
            let pa = points |> pointsOnAxisFunc
            (p,pa) 
          else
            // REGULAR CASE - add starting point to end
            let p = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last)
            let pa = pointsOnAxisFunc model.intersectionPoints
            (p,pa)
        
        let p, pa = 
          // Fix winding order (if axis is available!)
          match pa with
          | None -> (p,pa)
          | Some paa -> 
            // for higher Precision shift by AxisPoint
            let axisPoint = paa.pointsOnAxis |> PList.skip 1 |> PList.first
            let p0 = p |> PList.first                  |> fun x -> x - axisPoint
            let p1 = p |> PList.skip 1 |> PList.first  |> fun x -> x - axisPoint
            let p2 = p |> PList.skip 2 |> PList.first  |> fun x -> x - axisPoint

            let dir1 = p1.Normalized  // already shifted by axisPoint
            let x1 = (p0-p1).Normalized
            let x2 = (p2-p1).Normalized
            let dir2 = (x1.Cross(x2)).Normalized

            if dir1.Dot(dir2) |> sign < 0 then
              let pRev = p |> PList.toList |> List.rev |> PList.ofList
              let aRev = { paa with pointsOnAxis = paa.pointsOnAxis |> PList.toList |> List.rev |> PList.ofList }
              printfn "\n\n\nFixed winding order \n\n\n"
              (pRev, Some aRev)
            else 
              (p,pa)

        let newBrush = 
          { 
            // 1m shift for scene with axis outside of tunnel....REMOVE
            pointsOnAxis = pa |> Option.map(fun x -> {x with pointsOnAxis = x.pointsOnAxis |> PList.map(fun v -> v + V3d.OOI); midPoint = x.midPoint + V3d.OOI})
            points = p
            segments = model.segments
            color = 
              match model.brush.Count % 2 with
              | 0 -> C4b.DarkGreen
              | 1 -> C4b.DarkCyan
              | 2 -> C4b.DarkBlue
              | 3 -> C4b.DarkMagenta
              | 4 -> C4b.DarkRed
              | _ -> C4b.DarkYellow
          }
        
        let newGrouped =
            model.groupedBrushes |> HMap.alter newBrush.color (fun x -> 
              match x with 
              | Some y -> Some (y |> PList.prepend newBrush)
              | None -> Some (PList.single newBrush))
           
        { model with brush = model.brush |> PList.prepend newBrush; intersectionPoints = PList.empty; segments = PList.empty; groupedBrushes = newGrouped }
      else
        model
    | ShowDebugVis -> { model with debugShadowVolume = not (model.debugShadowVolume) }
    | UseGrouping -> { model with useGrouping = not (model.useGrouping) }
    | ShowOutline  -> { model with showOutline = not (model.showOutline) }
    | ShowOutlineDetail  -> { model with showDetailOutline = not (model.showDetailOutline) }
    | SetAlpha a -> { model with alpha = a } 
    | SetExtrusionOffset o -> { model with extrusionOffset = o }
    | SetVolumeGeneration x -> {model with volumeGeneration = x }

  let view (model : MPickingModel) =
    
    let lineWidth = 2.0
    let depthOffset = 0.01

    let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
    let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

    let debugClippingVolumeSg polygonSG = 
      let debugVolume = 
        polygonSG
          |> Sg.onOff model.debugShadowVolume
          |> Sg.effect [
            toEffect DefaultSurfaces.stableTrafo
            Shader.DebugColor.Effect
            ]

      let debugShadowVolume =
        debugVolume
          |> Sg.uniform "UseDebugColor" (Mod.constant(false))
          |> Sg.depthTest (Mod.constant DepthTestMode.Less)
          |> Sg.cullMode (Mod.constant (CullMode.Front))
          |> Sg.blendMode (Mod.constant(BlendMode.Blend))
        
      let debugShadowVolumeLines =
        debugVolume
          |> Sg.uniform "UseDebugColor" (Mod.constant(true))
          |> Sg.fillMode (Mod.constant (FillMode.Line))

      [ debugShadowVolume; debugShadowVolumeLines] |> Sg.ofList

    let brushOutlineSg (b:MBrush) = 
      let detailOutline = 
        b.segments 
          |> AList.map(fun x -> x.points |> AList.ofList) 
          |> AList.concat
          |> Sg.drawOutlineDetail (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showDetailOutline

      let outline = 
        b.points
          |> Sg.drawOutlineSimple (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showOutline

      [outline; detailOutline] |> Sg.ofList

    let debugAxisSg (b:MBrush) = 
      
      let pointSG size t = 
        Sg.sphere 3 b.color (Mod.constant size)
        |> Sg.noEvents
        |> Sg.trafo t
        |> Sg.uniform "WorldPos" (t |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" (Mod.constant(size))
        |> Sg.effect [
          toEffect <| DefaultSurfaces.stableTrafo
          toEffect <| DefaultSurfaces.vertexColor
        ]
      
      b.pointsOnAxis 
        |> Mod.map(fun x ->
          x |> Option.map(fun a -> 
            let points = 
              a.pointsOnAxis 
                |> AList.map(fun x -> 
                  let t = (Mod.constant(Trafo3d.Translation x))
                  pointSG 0.1 t) 

            let midPoint =
              let t = a.midPoint |> Mod.map(fun x -> Trafo3d.Translation x)
              pointSG 0.2 t

            points 
              |> AList.append (AList.single midPoint)
              |> ASet.ofAList 
              |> Sg.set)
            |> Option.defaultValue Sg.empty) 
        |> Sg.dynamic
        |> Sg.onOff model.debugShadowVolume

    let drawSeqBrushes = 
      model.brush 
      |> AList.map(fun b ->
        
        let clippingPolygon = 
          model.volumeGeneration 
            |> Mod.map (fun x -> 
              match x with
              | None -> Sg.empty
              | Some vg -> 
                match vg with
                | Plane -> Sg.drawFinishedBrushPlane b model.alpha model.extrusionOffset
                | _ -> Sg.drawFinishedBrushAxis b model.alpha model.extrusionOffset vg
              ) |> Sg.dynamic

        let coloredPolygon =
          clippingPolygon 
            |> Sg.effect [
              toEffect DefaultSurfaces.stableTrafo
              toEffect DefaultSurfaces.vertexColor
              ]
            |> StencilAreaMasking.stencilAreaSG maskPass areaPass
  
        maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
        areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
           
        [
          coloredPolygon
          debugClippingVolumeSg clippingPolygon
          brushOutlineSg b
          //debugAxisSg b
        ] |> Sg.ofList) 
      |> AList.toASet 
      |> Sg.set

    let drawGroupedBrushes =
      model.groupedBrushes 
      |> AMap.map(fun groupColor x -> 
        let clippingPolygon = 
          x |> AList.map (fun b ->
            model.volumeGeneration 
              |> Mod.map (fun x -> 
                match x with
                | None -> Sg.empty
                | Some vg -> 
                  match vg with
                  | Plane -> Sg.drawFinishedBrushPlane b model.alpha model.extrusionOffset
                  | _ -> Sg.drawFinishedBrushAxis b model.alpha model.extrusionOffset vg)
              |> Sg.dynamic)
            |> AList.toASet 
            |> Sg.set
        
        let coloredPolygon =
          
          let groupColorAlpha = model.alpha |> Mod.map(fun a -> groupColor.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f())
          
          clippingPolygon
            |> Sg.effect [
              toEffect DefaultSurfaces.stableTrafo
              toEffect DefaultSurfaces.vertexColor
              ]
            |> StencilAreaMasking.stencilAreaGroupedSG maskPass areaPass groupColorAlpha

        let brushOutlines = 
          x |> AList.map( fun b -> brushOutlineSg b) |> AList.toASet |> Sg.set

        maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
        areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

        [
         coloredPolygon
         debugClippingVolumeSg clippingPolygon
         brushOutlines
        ] |> Sg.ofList)
    |> AMap.toASet
    |> ASet.map snd
    |> Sg.set

    let sketchOutlineDetail = 
      model.segments 
        |> AList.map(fun x -> x.points |> AList.ofList) 
        |> AList.concat
        |> Sg.drawOutlineDetail (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) model.showDetailOutline

    let sketchOutline = 
      model.intersectionPoints 
        |> Sg.drawOutlineSimple (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) (Mod.constant(true))

    let finishedBrushes = 
      model.useGrouping |> Mod.map(fun x ->
        match x with
        | true -> drawGroupedBrushes
        | false -> drawSeqBrushes ) |> Sg.dynamic

    [ 
      sketchOutline
      sketchOutlineDetail 
      finishedBrushes
    ] |> Sg.ofList