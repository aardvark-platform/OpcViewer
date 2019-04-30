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

  let maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
  let areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

  let maskSG sg = 
    sg
      |> Sg.pass maskPass
      |> Sg.stencilMode (Mod.constant (writeZFail))
      |> Sg.cullMode (Mod.constant (CullMode.None))
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])

  let fillSG sg =
    sg
      |> Sg.pass areaPass
      |> Sg.stencilMode (Mod.constant (readMaskAndReset))
      //|> Sg.cullMode (Mod.constant CullMode.CounterClockwise)  // for zpass -> backface-culling
      //|> Sg.depthTest (Mod.constant DepthTestMode.Less)        // for zpass -> active depth-test
      |> Sg.cullMode (Mod.constant CullMode.None)
      |> Sg.depthTest (Mod.constant DepthTestMode.None)
      |> Sg.blendMode (Mod.constant BlendMode.Blend)
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Stencil])

  let stencilAreaSG sg =
    [
      maskSG sg   // one pass by using EXT_stencil_two_side :)
      fillSG sg
    ] |> Sg.ofList

module Sg =

  let toV3f (input:V3d) : V3f= input |> V3f

  let pointsGetHead points =    
    points 
      |> AList.toMod 
      |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)

  let drawColoredPoints (points : alist<V3d>) (color:IMod<C4f>) =
    
    let head = pointsGetHead points
      
    let pointsF = 
      points 
        |> AList.toMod 
        |> Mod.map2(
          fun h points -> 
            points |> PList.map(fun x -> (x-h) |> toV3f) |> PList.toArray
            ) head
       
    Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.effect [
         toEffect DefaultSurfaces.stableTrafo
         toEffect DefaultSurfaces.sgColor
         Shader.PointSprite.Effect
      ]
      |> Sg.translate' head
      |> Sg.uniform "PointSize" (Mod.constant 10.0)
      |> Sg.uniform "Color" color
      |> Sg.depthTest (Mod.constant(DepthTestMode.None))

  let drawColoredConnectionLines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) =           
    
    let toColoredEdges (color : C4b) (points : alist<V3d>) =
      points
        |> AList.toMod
        |> Mod.map(fun x -> 
          x |> PList.toList 
            |> List.pairwise 
            |> List.map(fun (a,b) -> new Line3d(a,b), color))

    let drawColoredEdges (width:IMod<float>) edges = 
      edges
        |> IndexedGeometryPrimitives.lines
        |> Sg.ofIndexedGeometry
        |> Sg.depthTest (Mod.constant(DepthTestMode.None))
        |> Sg.uniform "LineWidth" width
        |> Sg.effect [
          toEffect DefaultSurfaces.stableTrafo
          toEffect DefaultSurfaces.thickLine 
          toEffect DefaultSurfaces.vertexColor      
        ]

    adaptive {
      let! c = color
      let! edges = toColoredEdges c points
      return drawColoredEdges width edges
    } |> Sg.dynamic

  let drawColoredPolygon (points:list<V3d>) (color:C4b) (offset:float) (alpha:float) =

    let planeFit (points:list<V3d>) : Plane3d =
      let length = points |> List.length |> float

      let c = 
        let sum = points |> List.reduce (fun x y -> V3d.Add(x,y))
        sum / length

      let pDiffAvg = points |> List.map(fun x -> x - c)
    
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
    | AddBrush -> 
      if model.intersectionPoints |> PList.count > 2 then
        //{ model with brush = Some { points = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last) |> PList.toList; color = C4b.Blue} } // add starting point to end
        let newBrush = 
          { 
            points = (model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last) |> PList.toList)
            color = 
              match (model.brush |> List.length) % 6 with
              | 0 -> C4b.DarkGreen
              | 1 -> C4b.DarkCyan
              | 2 -> C4b.DarkBlue
              | 3 -> C4b.DarkMagenta
              | 4 -> C4b.DarkRed
              | _ -> C4b.DarkYellow
          }
        { model with brush = model.brush |> List.append [newBrush]; intersectionPoints = PList.empty }
      else
        //{ model with brush = None }
        model
    | ShowDebugVis -> { model with debugShadowVolume = not (model.debugShadowVolume) }
    | SetAlpha a -> { model with alpha = Numeric.update model.alpha a }

  let view (model : MPickingModel) =

    let drawBrush brush = 
      brush 
      |> Mod.map2(fun alpha x ->
          x |> List.map(fun b ->
            let volumeExtrusion = 1.0
            let polygonSG = Sg.drawColoredPolygon b.points b.color volumeExtrusion alpha

            let debugVis =
              polygonSG
                |> Sg.depthTest (Mod.constant DepthTestMode.Less)
                |> Sg.cullMode (Mod.constant (CullMode.CounterClockwise))
                |> Sg.pass RenderPass.main
                |> Sg.onOff model.debugShadowVolume

            [
              Sg.drawColoredConnectionLines (b.points |> AList.ofList) (Mod.constant(b.color)) (Mod.constant(2.0))
              polygonSG |> StencilAreaMasking.stencilAreaSG
              debugVis
            ] |> Sg.ofList
            )
            |> Sg.ofList) model.alpha.value
      |> Sg.dynamic
    
    [ 
      drawBrush model.brush
      Sg.drawColoredPoints model.intersectionPoints (Mod.constant(C4f.Red))
      Sg.drawColoredConnectionLines model.intersectionPoints (Mod.constant(C4b.Yellow)) (Mod.constant(2.0))
    ] |> Sg.ofList