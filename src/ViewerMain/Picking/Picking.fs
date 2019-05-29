namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open OpcSelectionViewer
open Uncodium
open System.Collections.Generic

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

  let stencilAreaSG pass1 pass2 sg =
    [
      maskSG pass1 sg   // one pass by using EXT_stencil_two_side :)
      fillSG pass2 sg
    ] |> Sg.ofList

module Sg =

  let drawOutline (points:alist<V3d>) (colorLine : IMod<C4b>) (colorPoint : IMod<C4b>) (offset:IMod<float>) (width : IMod<float>) =           
    
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
      |> Sg.uniform "PointSize" (Mod.constant 10.0)
      |> Sg.uniform "depthOffset" offset
      |> Sg.effect [
        toEffect DefaultSurfaces.stableTrafo
        Shader.PointSprite.Effect
        toEffect DefaultSurfaces.sgColor
        ]

    [ lines; points] |> Sg.ofSeq |> Sg.translate' shift

  let planeFit (points:seq<V3d>) : Plane3d =
    let length = points |> Seq.length |> float

    let c = 
        let sum = points |> Seq.reduce (fun x y -> V3d.Add(x,y))
        sum / length

    let pDiffAvg = points |> Seq.map(fun x -> x - c)
        
    let mutable matrix = M33d.Zero
    pDiffAvg |> Seq.iter(fun x -> matrix.AddOuterProduct(&x))
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

  let planeFitPolygon (points:plist<V3d>) offset color = 
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
      |> List.pairwise    // TODO PLIST.pairwise only reason to change type
      |> List.mapi (fun i (a,b) -> 
        // Shift points 
        let aPos = a + extrudeNormal * offset
        let bPos = b + extrudeNormal * offset
        let aNeg = a - extrudeNormal * offset
        let bNeg = b - extrudeNormal * offset
               
        // Generate Triangles for watertight polygon
        [
          if i <> 0 then // first edge has to be skipped for top and bottom triangle generation
              yield Triangle3d(startPos, bPos, aPos), color // top
              yield Triangle3d(startNeg, aNeg, bNeg), color // bottom
             
          yield Triangle3d(aPos, bNeg, aNeg), color // side1
          yield Triangle3d(aPos, bPos, bNeg), color // side2
        ]
      ) |> List.concat

  let drawFinishedBrushPlane (brush:MBrush) (alpha:IMod<float>) (offset:IMod<float>) =

    let colorAlpha = Mod.map2(fun (c:C4b) a -> c.ToC4d() |> fun x -> C4d(x.R, x.G, x.B, a).ToC4b()) brush.color alpha

    let generatePolygonTriangles (color : C4b) (offset : float) (points:alist<V3d>) =
      points 
      |> AList.toMod
      |> Mod.map(fun x -> 
        // increase Precision
        let shift = x |> PList.tryAt 0 |> Option.defaultValue V3d.Zero
        let shiftedPoints = x |> PList.toSeq |> Seq.map(fun (y:V3d) -> (y-shift)) |> PList.ofSeq

        let triangles = planeFitPolygon shiftedPoints offset color

        triangles
          |> IndexedGeometryPrimitives.triangles
          |> Sg.ofIndexedGeometry
          |> Sg.uniform "Color" colorAlpha
          |> Sg.translate' (Mod.constant(shift)))

    let sg = Mod.bind2(fun c o -> generatePolygonTriangles c o brush.points) colorAlpha offset
  
    sg |> Mod.toASet |> Sg.set

  let drawFinishedBrushAxis (brush:MBrush) (alpha:IMod<float>) (offset:IMod<float>) (volumeGeneration:VolumeGeneration) :ISg<'a> =
    
    let colorAlpha = Mod.map2(fun (c:C4b) a -> c.ToC4d() |> fun x -> C4d(x.R, x.G, x.B, a).ToC4b()) brush.color alpha
    
    let sg = 
      brush.pointsOnAxis |> Mod.bind(fun axisInfo -> 
      match axisInfo with
      | None -> Mod.constant(Sg.empty)
      | Some (api:MAxisPointInfo) ->
        offset |> Mod.map(fun o -> 
          
          // increase Precision
          let shiftVec p = 
            p
              |> AList.toMod 
              |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)

          let shiftPoints p shiftVec =
            let asdf = p |> AList.toMod
            Mod.map2(fun x (s:V3d) -> 
              x 
              |> PList.toSeq 
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
              |> Sg.index indexArray
              |> Sg.uniform "Color" colorAlpha
              |> Sg.translate' shift
              
          triangles)
        ) 
  
    sg |> Mod.toASet |> Sg.set

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit, axisPoint) -> 
      IntersectionController.intersect model sceneHit box axisPoint
    | RemoveLastPoint ->
      let points, infos = 
        match model.intersectionPoints.AsList with
          | [] -> [], HMap.empty
          | first :: rest -> 
            rest, model.hitPointsInfo.Remove first
      { model with intersectionPoints = points |> PList.ofList; hitPointsInfo = infos }
    | ClearPoints -> 
      { model with intersectionPoints = PList.empty; hitPointsInfo = HMap.empty}
    | AddBrush pointsOnAxis -> 
      if model.intersectionPoints |> PList.count > 2 then
        let newBrush = 
          { // add starting point to end
            points = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last)
            // 1m shift for scene with axis outside of tunnel....REMOVE
            pointsOnAxis = pointsOnAxis |> Option.map(fun x -> {x with pointsOnAxis = x.pointsOnAxis |> PList.map(fun v -> v + V3d.OOI); midPoint = x.midPoint + V3d.OOI})
            // TODO...close
            segments = model.segments
            color = 
              match (model.brush |> PList.count) % 6 with
              | 0 -> C4b.DarkGreen
              | 1 -> C4b.DarkCyan
              | 2 -> C4b.DarkBlue
              | 3 -> C4b.DarkMagenta
              | 4 -> C4b.DarkRed
              | _ -> C4b.DarkYellow
          }
        { model with brush = model.brush |> PList.append newBrush; intersectionPoints = PList.empty; segments = PList.empty }
      else
        model
    | ShowDebugVis -> { model with debugShadowVolume = not (model.debugShadowVolume) }
    | SetAlpha a -> { model with alpha = a } 
    | SetExtrusionOffset o -> { model with extrusionOffset = o }
    | SetVolumeGeneration x -> {model with volumeGeneration = x }


  let view (model : MPickingModel) =
    let lineWidth = 2.0
    let depthOffset = 0.01

    let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
    let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

    let drawBrush = 
      model.brush 
      |> AList.map(fun b ->
        
        let polygonSG = 
          model.volumeGeneration 
            |> Mod.map (fun x -> 
              match x with
              | None -> Sg.empty
              | Some vg -> 
                match vg with
                | Plane -> Sg.drawFinishedBrushPlane b model.alpha model.extrusionOffset
                | _ -> Sg.drawFinishedBrushAxis b model.alpha model.extrusionOffset vg
              ) |> Sg.dynamic

        let axisDebugPoints = 
          b.pointsOnAxis 
            |> Mod.map(fun x ->
              x |> Option.map(fun a -> 
                let points = 
                  a.pointsOnAxis 
                    |> AList.map(fun x -> 
                      let t = (Mod.constant(Trafo3d.Translation x))
                      Sg.sphere 3 b.color (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.trafo t
                        |> Sg.uniform "WorldPos" (t |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
                        |> Sg.uniform "Size" (Mod.constant(0.1))
                        |> Sg.effect [
                          toEffect <| DefaultSurfaces.stableTrafo
                          toEffect <| DefaultSurfaces.vertexColor
                        ]) 

                let midPoint =
                  let t = a.midPoint |> Mod.map(fun x -> Trafo3d.Translation x)
                  Sg.sphere 3 b.color (Mod.constant 0.2)
                    |> Sg.noEvents
                    |> Sg.trafo t
                    |> Sg.uniform "WorldPos" (t |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
                    |> Sg.uniform "Size" (Mod.constant(0.2))
                    |> Sg.effect [
                      toEffect <| DefaultSurfaces.stableTrafo
                      toEffect <| DefaultSurfaces.vertexColor
                    ]

                points 
                  |> AList.append (AList.single midPoint)
                  |> ASet.ofAList 
                  |> Sg.set)
                |> Option.defaultValue Sg.empty) |> Sg.dynamic
            |> Sg.onOff model.debugShadowVolume

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

        let coloredPoylogn =
          polygonSG 
            |> Sg.effect [
              toEffect DefaultSurfaces.stableTrafo
              toEffect DefaultSurfaces.sgColor
              ]
            |> StencilAreaMasking.stencilAreaSG maskPass areaPass

        let segmentOutline = 
          b.segments |> AList.map(fun x -> x.points |> AList.ofList) |> AList.concat

        let output = 
          [
            coloredPoylogn
            Sg.drawOutline b.points (Mod.constant(C4b.Yellow)) (Mod.constant(C4b.Red)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) 
            Sg.drawOutline segmentOutline (Mod.constant(C4b.DarkYellow)) (Mod.constant(C4b.DarkRed)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) 
            axisDebugPoints
            debugShadowVolume 
            debugShadowVolumeLines
          ] |> Sg.ofList
            
        maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
        areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
           
        output) |> AList.toASet |> Sg.set
    
    let projOutline = 
      model.segments |> AList.map(fun x -> x.points |> AList.ofList) |> AList.concat
        
    [ 
      Sg.drawOutline model.intersectionPoints (Mod.constant(C4b.Yellow)) (Mod.constant(C4b.Red)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth))
      Sg.drawOutline projOutline (Mod.constant(C4b.DarkYellow)) (Mod.constant(C4b.Red)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth))
      drawBrush
    ] |> Sg.ofList