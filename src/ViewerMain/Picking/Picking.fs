namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open OpcSelectionViewer
open Uncodium

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

  // old version
  let drawColoredPolygon (points:list<V3d>) (color:C4b) (alpha:float) (offset:float) =

    let generatePolygonTriangles (color : C4b) (offset : float) (points:list<V3d>) =
     
      let plane = planeFit points
      let extrudeNormal = plane.Normal
      let projPointsOnPlane = points |> List.map(plane.Project)

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
              yield Triangle3d(startPos, bPos, aPos), color // top
              yield Triangle3d(startNeg, aNeg, bNeg), color // bottom
           
            yield Triangle3d(aPos, bNeg, aNeg), color // side1
            yield Triangle3d(aPos, bPos, bNeg), color // side2
          ])
        |> List.concat

    let colorAlpha = color.ToC4d() |> (fun x -> C4d(x.R, x.G, x.B, alpha).ToC4b())
    let polygon = generatePolygonTriangles colorAlpha offset points
    polygon 
      |> IndexedGeometryPrimitives.triangles 
      |> Sg.ofIndexedGeometry
      |> Sg.effect [
        toEffect DefaultSurfaces.stableTrafo
        toEffect DefaultSurfaces.vertexColor
      ]

  let drawFinishedBrush (points:alist<V3d>) (color:IMod<C4b>) (alpha:IMod<float>) (offset:IMod<float>) :ISg<'a> =
        
    let generatePolygonTriangles (color : C4b) (offset : float) (points:alist<V3d>) =
      points 
      |> AList.toMod
      |> Mod.map(fun x -> 
        let plane = planeFit x
        let extrudeNormal = plane.Normal
        let projPointsOnPlane = x |> PList.map(plane.Project) |> PList.toList

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
        )

    let colorAlpha = Mod.map2(fun (c:C4b) a  -> 
      let col = c.ToC4d() 
      C4d(col.R, col.G, col.B, a).ToC4b()) color alpha

    adaptive {
        let! colorAlpha = colorAlpha
        let! o = offset
        let! polygon = generatePolygonTriangles colorAlpha o points
            
        return 
          polygon
            |> IndexedGeometryPrimitives.triangles 
            |> Sg.ofIndexedGeometry
            |> Sg.effect [
              toEffect DefaultSurfaces.stableTrafo
              toEffect DefaultSurfaces.vertexColor
            ]

    } |> Sg.dynamic

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit) -> 
      IntersectionController.intersect model "" sceneHit box
    | RemoveLastPoint ->
      let points, infos = 
        match model.intersectionPoints.AsList with
          | [] -> [], HMap.empty
          | first :: rest -> 
            rest, model.hitPointsInfo.Remove first
      { model with intersectionPoints = points |> PList.ofList; hitPointsInfo = infos }
    | ClearPoints -> 
      { model with intersectionPoints = PList.empty; hitPointsInfo = HMap.empty}
    | AddBrush axisPoints -> 
      if model.intersectionPoints |> PList.count > 2 then
        //{ model with brush = Some { points = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last) |> PList.toList; color = C4b.Blue} } // add starting point to end
        let newBrush = 
          { 
            points = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last) |> PList.toList
            pointsOnAxis = axisPoints |> Option.map(fun x -> x |> PList.prepend (x |> PList.last) |> PList.toList)
            color = 
              match (model.brush |> PList.count) % 6 with
              | 0 -> C4b.DarkGreen
              | 1 -> C4b.DarkCyan
              | 2 -> C4b.DarkBlue
              | 3 -> C4b.DarkMagenta
              | 4 -> C4b.DarkRed
              | _ -> C4b.DarkYellow
          }
        { model with brush = model.brush |> PList.append newBrush; intersectionPoints = PList.empty }
      else
        //{ model with brush = None }
        model
    | ShowDebugVis -> { model with debugShadowVolume = not (model.debugShadowVolume) }
    | SetAlpha a -> { model with alpha = Numeric.update model.alpha a }

  let view (model : MPickingModel) =
    let lineWidth = 2.0
    let depthOffset = 0.01

    let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
    let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

    let drawBrush = 
      model.brush 
      |> AList.map(fun b ->
        let alpha = 0.6
        let volumeExtrusion = 1.0

        let polygonSG = 
          //Sg.drawColoredPolygon b.points b.color alpha volumeExtrusion 
          Sg.drawFinishedBrush (b.points |> AList.ofList) (Mod.constant(b.color)) (Mod.constant(alpha)) (Mod.constant(volumeExtrusion))
            
        let debugVis =
          polygonSG
            |> Sg.depthTest (Mod.constant DepthTestMode.Less)
            |> Sg.cullMode (Mod.constant (CullMode.Clockwise))
            |> Sg.pass RenderPass.main
            |> Sg.onOff model.debugShadowVolume

        let output = 
          [
            polygonSG |> StencilAreaMasking.stencilAreaSG maskPass areaPass
            Sg.drawOutline (b.points |> AList.ofList) (Mod.constant(C4b.Yellow)) (Mod.constant(C4b.Red)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth)) 
            debugVis
          ] |> Sg.ofList
            
        maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
        areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
           
        output) |> AList.toASet |> Sg.set
    
    [ 
      Sg.drawOutline model.intersectionPoints (Mod.constant(C4b.Yellow)) (Mod.constant(C4b.Red)) (Mod.constant(depthOffset)) (Mod.constant(lineWidth))
      drawBrush
    ] |> Sg.ofList