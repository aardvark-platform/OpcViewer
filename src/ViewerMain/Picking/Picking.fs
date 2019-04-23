namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open OpcSelectionViewer
open Uncodium

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
        { model with brush = Some { points = model.intersectionPoints |> PList.prepend (model.intersectionPoints |> PList.last); color = C4b.Blue} } // add starting point to end
      else
        { model with brush = None }

  let toV3f (input:V3d) : V3f= input |> V3f

  let drawColoredPoints (points : alist<V3d>) =
    
    let head = 
      points 
        |> AList.toMod 
        |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)
      
    let pointsF = 
      points 
        |> AList.toMod 
        |> Mod.map2(
          fun h points -> 
            points |> PList.map(fun (x:V3d) -> (x-h) |> toV3f) |> PList.toArray
            ) head
       
    Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.effect [
         toEffect DefaultSurfaces.stableTrafo
         toEffect (DefaultSurfaces.constantColor C4f.Red)
         Shader.PointSprite.Effect
      ]
      |> Sg.translate' head
      |> Sg.uniform "PointSize" (Mod.constant 10.0)

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

  let projectedPointAndPlaneNormal points =
    points |> Mod.map(fun x -> 
      let plane = planeFit x
      let extrudeNormal = plane.Normal
      let projectedPointsOnPlane = x |> PList.map(plane.Project)
      projectedPointsOnPlane, extrudeNormal
     )

  let generatePolygonTriangles (color : C4b) (offset : float) (points:alist<V3d>) =
    points 
     |> AList.toMod
     |> projectedPointAndPlaneNormal
     |> Mod.map(fun (points, extrusionDir) -> 
       let list = points |> PList.toList
       
       // Top and Bottom triangle-fan startPoint
       let startPoint = list |> List.head
       let startPos = startPoint + extrusionDir * offset
       let startNeg = startPoint - extrusionDir * offset
       
       if list |> List.length < 3 then
         []
       else 
         list
           |> List.pairwise    // TODO PLIST.pairwise only reason to change type
           |> List.mapi (fun i (a,b) -> 
             // Shift points 
             let aPos = a + extrusionDir * offset
             let bPos = b + extrusionDir * offset
             let aNeg = a - extrusionDir * offset
             let bNeg = b - extrusionDir * offset
             
             // Generate Triangles for watertight polygon
             [
               if i <> 0 then // first edge has to be skipped for top and bottom triangle generation
                   yield Triangle3d(startPos, bPos, aPos), color // top
                   yield Triangle3d(startNeg, aNeg, bNeg), color // bottom
           
               yield Triangle3d(aPos, bNeg, aNeg), color // side1
               yield Triangle3d(aPos, bPos, bNeg), color // side2
             ]
           )|> List.concat
     )

  let drawColoredPolygon sides =
    sides 
     |> IndexedGeometryPrimitives.triangles 
     |> Sg.ofIndexedGeometry
     |> Sg.effect [
       toEffect Aardvark.UI.Trafos.Shader.stableTrafo // toEffect DefaultSurfaces.trafo
       toEffect DefaultSurfaces.vertexColor
     ]

  let viewPolygon points (color:IMod<C4b>) (offset:IMod<float>) (alpha:IMod<float>) =
    adaptive {
        let! c = color
        let! o = offset
        let! a = alpha
        let colorAlpha = c.ToC4d() |> (fun x -> C4d(x.R, x.G, x.B, a).ToC4b())
        let! sides = generatePolygonTriangles colorAlpha o points
        return sides |> drawColoredPolygon
    } |> Sg.dynamic

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

  let maskSG sv = 
    sv
     |> Sg.pass maskPass
     |> Sg.stencilMode (Mod.constant (writeZFail))
     |> Sg.cullMode (Mod.constant (CullMode.None))
     |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])

  let fillSG sv =
    sv
     |> Sg.pass areaPass
     |> Sg.stencilMode (Mod.constant (readMaskAndReset))
     //|> Sg.cullMode (Mod.constant CullMode.CounterClockwise)  // for zpass -> backface-culling
     //|> Sg.depthTest (Mod.constant DepthTestMode.Less)        // for zpass -> active depth-test
     |> Sg.cullMode (Mod.constant CullMode.None)
     |> Sg.depthTest (Mod.constant DepthTestMode.None)
     |> Sg.blendMode (Mod.constant BlendMode.Blend)
     |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Stencil])

  let areaSG sv =
    [
      maskSG sv   // one pass by using EXT_stencil_two_side :)
      fillSG sv
    ] |> Sg.ofList

  let view (model : MPickingModel) =

    let brush = 
      model.brush 
      |> Mod.map(function
        | Some brush -> viewPolygon brush.points brush.color (Mod.constant(2.0)) (Mod.constant(0.6))    
        | None -> Sg.empty
        ) 
    
    [ 
      drawColoredPoints model.intersectionPoints
      areaSG (brush |> Sg.dynamic)
    ] |> Sg.ofList