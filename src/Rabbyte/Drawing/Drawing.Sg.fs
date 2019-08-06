namespace Rabbyte.Drawing
 
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI

open OpcViewer.Base

open DrawingModel

module PlaneFitting =
    open Uncodium

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
          (matrix).AddOuterProduct(&y))

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

module StencilAreaMasking =

  let private writeZFail =
      let compare = new StencilFunction(StencilCompareFunction.Always, 0, 0xffu)
      let front   = new StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.DecrementWrap, StencilOperationFunction.Keep)
      let back    = new StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.IncrementWrap, StencilOperationFunction.Keep)
      StencilMode(front, compare, back, compare)

  //let writeZPass =
  //    let compare = new StencilFunction(StencilCompareFunction.Always, 0, 0xffu)
  //    let front   = new StencilOperation(StencilOperationFunction.IncrementWrap, StencilOperationFunction.Keep, StencilOperationFunction.Keep)
  //    let back    = new StencilOperation(StencilOperationFunction.DecrementWrap, StencilOperationFunction.Keep, StencilOperationFunction.Keep)
  //    StencilMode(front, compare, back, compare)

  let private readMaskAndReset = 
      let compare = new StencilFunction(StencilCompareFunction.NotEqual, 0, 0xffu)
      let operation = new StencilOperation(StencilOperationFunction.Zero, StencilOperationFunction.Zero, StencilOperationFunction.Zero)
      StencilMode(operation, compare)

  //let maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
  //let areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

  let private maskSG maskPass sg = 
    sg
      |> Sg.pass maskPass
      |> Sg.stencilMode (Mod.constant (writeZFail))
      |> Sg.cullMode (Mod.constant (CullMode.None))
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])

  let private fillSG areaPass sg =
    sg
      |> Sg.pass areaPass
      |> Sg.stencilMode (Mod.constant (readMaskAndReset))
      //|> Sg.cullMode (Mod.constant CullMode.CounterClockwise)  // for zpass -> backface-culling
      //|> Sg.depthTest (Mod.constant DepthTestMode.Less)        // for zpass -> active depth-test
      |> Sg.cullMode (Mod.constant CullMode.None)
      |> Sg.depthTest (Mod.constant DepthTestMode.None)
      |> Sg.blendMode (Mod.constant BlendMode.Blend)
      |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Stencil])

  let stencilArea (pass1:RenderPass) (pass2:RenderPass) (sg1:ISg<'a>) (sg2:ISg<'a>) : ISg<'a>=
    [
      maskSG pass1 sg1   // one pass by using EXT_stencil_two_side :)
      fillSG pass2 sg2
    ] |> Sg.ofList

  let stencilArea' (pass1:RenderPass) (pass2:RenderPass) (sg:ISg<'a>) : ISg<'a>=
    stencilArea pass1 pass2 sg sg

  let stencilAreaGrouped (pass1:RenderPass) (pass2:RenderPass) (color:IMod<V4f>) (sg:ISg<'a>) : ISg<'a> =

    let fullscreenSg =     
      Aardvark.SceneGraph.SgPrimitives.Sg.fullScreenQuad
      |> Sg.noEvents
      |> Sg.vertexBufferValue DefaultSemantic.Colors color
      |> Sg.effect [ toEffect DefaultSurfaces.vertexColor]

    stencilArea pass1 pass2 sg fullscreenSg

module ClippingVolume =
    
    // TODO...move this helper func to OPCViewer.Base ?
    let private colorAlpha (color:IMod<C4b>) (alpha:IMod<float>) = 
        Mod.map2(fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) color alpha

    let private createPlaneFittedExtrusionVolume (points:plist<V3d>) extrusionOffset = 
        let plane = PlaneFitting.planeFit points
        let extrudeNormal = plane.Normal
        let projPointsOnPlane = points |> PList.map(plane.Project) |> PList.toList

        // Top and Bottom triangle-fan startPoint
        let startPoint = projPointsOnPlane |> List.head
        let startPos = startPoint + extrudeNormal * extrusionOffset
        let startNeg = startPoint - extrudeNormal * extrusionOffset
         
        if projPointsOnPlane |> List.length < 3 then
          []
        else 
          projPointsOnPlane
          |> List.pairwise
          |> List.mapi (fun i (a,b) -> 
            // Shift points 
            let aPos = a + extrudeNormal * extrusionOffset
            let bPos = b + extrudeNormal * extrusionOffset
            let aNeg = a - extrudeNormal * extrusionOffset
            let bNeg = b - extrudeNormal * extrusionOffset
               
            // Generate Triangles for watertight polygon
            [
              if i <> 0 then // first edge has to be skipped for top and bottom triangle generation
                  yield Triangle3d(startPos, bPos, aPos) // top
                  yield Triangle3d(startNeg, aNeg, bNeg) // bottom
             
              yield Triangle3d(aPos, bNeg, aNeg) // side1
              yield Triangle3d(aPos, bPos, bNeg) // side2
            ]
          ) |> List.concat

    let planeFittedClippingVolume (color:IMod<C4b>) (alpha:IMod<float>) (extrusionOffset:IMod<float>) (points:alist<V3d>) =

        let colorAlpha = colorAlpha color alpha

        let generatePolygonTriangles (extrusionOffset : float) (points:alist<V3d>) =
          let shiftAndPosAndCol =
            points 
            |> AList.toMod
            |> Mod.bind(fun x -> 
              // increase Precision
              let shift = x |> PList.tryAt 0 |> Option.defaultValue V3d.Zero
              let shiftedPoints = x |> PList.toSeq |> Seq.map(fun (y:V3d) -> (y-shift)) |> PList.ofSeq

              let triangles = createPlaneFittedExtrusionVolume shiftedPoints extrusionOffset
              let pos = triangles |> Seq.collect (fun t -> [| V3f t.P0; V3f t.P1; V3f t.P2 |]) |> Seq.toArray
              colorAlpha |> Mod.map (fun cc -> shift, pos, cc))

          Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.map (fun (_,p,_) -> p) shiftAndPosAndCol)
            |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.map (fun (_,_,c) -> c) shiftAndPosAndCol)
            |> Sg.translate' (Mod.map (fun (s,_,_) -> s) shiftAndPosAndCol)

        let sg = extrusionOffset |> Mod.map(fun o -> generatePolygonTriangles o points) 
  
        sg |> Mod.toASet |> Sg.set

    let axisRingClippingVolume (color:IMod<C4b>) (alpha:IMod<float>) (offset:IMod<float>) (points:alist<V3d>) (pointsOnAxis:alist<V3d>) =
    
        let colorAlpha = colorAlpha color alpha

        pointsOnAxis 
            |> AList.toMod
            |> Mod.bind (fun axisInfo -> 
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

                    let shift = shiftVec points 
                    let pointsF = (shiftPoints points shift) |> Mod.map(fun x -> x |> Array.skip 1) // undo closing polygon (duplicates not needed) // TODO CHECK THIS!

                    let vertices = 
                    //match volumeGeneration with
                    //| AxisMidPoint -> 
                    //  let axisMidPointF = Mod.map2(fun (m:V3d) (s:V3d) -> V3f(m-s)) api.midPoint shift
                    //  Mod.map2(fun ps (midPoint:V3f) -> 
                    //    let dir = ps |> Array.map(fun (p:V3f) -> ((midPoint-p).Normalized))
                    //    let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))
                    //    let inner = Array.create ps.Length midPoint // use center point....
                    //    //let inner = Array.map2(+) ps offsetP  // shift from lasso points
                    //    let outer = Array.map2(-) ps offsetP
                    //    inner |> Array.append outer
                    //  ) pointsF axisMidPointF
                    //| AxisPoints -> 
                    //  let axisPsF = shiftPoints api.pointsOnAxis shift
                    //  Mod.map2(fun ps aps ->
                    //    let dir = Array.map2(fun (p:V3f) (a:V3f) -> ((a-p).Normalized)) ps aps 
                    //    let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))
                    //    let inner = Array.map2 (-) aps dir // use axisPoints and shift 1m to points
                    //    //let inner = Array.map2(+) ps offsetP  // shift from lasso points
                    //    let outer = Array.map2(-) ps offsetP
                    //    inner |> Array.append outer) pointsF axisPsF
                    //| AxisPointsMidRing -> 
                        let axisPsF = shiftPoints pointsOnAxis shift
              
                        Mod.map2(fun ps aps ->
                        let dir = Array.map2(fun (p:V3f) (a:V3f) -> ((a-p).Normalized)) ps aps 
                        let offsetP = dir |> Array.map(fun (p:V3f) -> p * (float32 o))

                        let inner = Array.map2 (-) aps dir // use axisPoints and shift 1m to points
                        //let inner = Array.map2(+) ps offsetP
                        let outer = Array.map2(-) ps offsetP
                        ps |> Array.append inner |> Array.append outer ) pointsF axisPsF

                    let indexArray = 
                        //match volumeGeneration with 
                        //| Plane -> failwith ""
                        //| AxisPoints  // same as MidPoint
                        //| AxisMidPoint -> 
                        //  pointsF |> Mod.map(fun x -> 
                        //    let l = x.Length
                        //    let indices = List<int>()
                        //    // TOP
                        //    for i in 0 .. (l - 3) do 
                        //      indices.Add(0)      
                        //      indices.Add(i + 1)  
                        //      indices.Add(i + 2)  
                        //    // BOTTOM
                        //    for i in 0 .. ( l - 3 ) do
                        //      indices.Add(l)
                        //      indices.Add(2*l - i - 1)
                        //      indices.Add(2*l - i - 2)
                        //    // SIDE
                        //    for i in 0 .. l - 1 do
                        //      indices.Add(i)          
                        //      indices.Add(l + i)      
                        //      indices.Add((i + 1) % l)
                        //      indices.Add(l + i)            
                        //      indices.Add(l + ((i + 1) % l))
                        //      indices.Add((i + 1) % l)      
                        //    indices.ToArray())
                        //| AxisPointsMidRing -> 
                        pointsF |> Mod.map(fun x -> 
              
                            let l = x.Length
                            let indices = System.Collections.Generic.List<int>()
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

                    Sg.draw IndexedGeometryMode.TriangleList
                        |> Sg.vertexAttribute DefaultSemantic.Positions vertices 
                        |> Sg.vertexBufferValue DefaultSemantic.Colors colorAlpha
                        |> Sg.index indexArray
                        |> Sg.translate' shift)
                ) |> Mod.toASet |> Sg.set

module DrawingSg = 

    let drawIndexedOutline (colorLine : IMod<C4b>) (colorPoint : IMod<C4b>) (pointSize : IMod<float>) (offset:IMod<float>) (width : IMod<float>) (points:alist<V3d>) =           
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
          |> Sg.uniform "depthOffset" offset    // WORKING? TODO...check
          |> Sg.effect [
            Shader.StableTrafo.Effect
            Shader.ThickLineNew.Effect
            toEffect DefaultSurfaces.sgColor
            ]

        let points = 
          Sg.draw IndexedGeometryMode.PointList
          |> Sg.vertexAttribute DefaultSemantic.Positions pointsF 
          |> Sg.uniform "Color" colorPoint
          |> Sg.uniform "PointSize" pointSize
          |> Sg.uniform "depthOffset" offset // WORKING? TODO...check
          |> Sg.effect [
            Shader.StableTrafo.Effect
            Shader.PointSprite.Effect
            toEffect DefaultSurfaces.sgColor
            ]

        [ lines; points] |> Sg.ofSeq |> Sg.translate' shift

    let convertLines close points = 
        points 
        |> AList.toMod
        |> Mod.map(fun k -> 
        let k = k |> PList.toList 
        match k  with
            | h::_ -> 
                let start = if close then k @ [h] else k
                start 
                    |> List.pairwise 
                    |> List.map (fun (a,b) -> new Line3d(a,b)) 
                    |> Array.ofList                                                        
            | _ -> [||])

    let discISg color size height trafo =
        Sg.cylinder 30 color size height              
            |> Sg.noEvents
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            |> Sg.uniform "Size" size
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.StableLight.Effect
            ]
            |> Sg.trafo(trafo)

    let coneISg color radius height trafo =  
        Sg.cone 30 color radius height
            |> Sg.noEvents
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.StableLight.Effect
            ]                   
            |> Sg.trafo(trafo)

    let lineISg color lineWidth trafo segments = 
        Sg.lines color segments
            |> Sg.noEvents
            |> Sg.uniform "LineWidth" lineWidth 
            |> Sg.uniform "depthOffset" (Mod.constant 1.0)
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.ThickLineNew.Effect
            ]
            |> Sg.trafo trafo

    let sphereISg color radius position =
        let trafo = Mod.constant (Trafo3d.Translation position)
        Sg.sphere 3 color radius 
            |> Sg.noEvents
            |> Sg.trafo trafo
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            //|> Sg.uniform "Size" radius // 5.0
            //|> Sg.effect [
            //    Shader.ScreenSpaceScale.Effect
            //    Shader.StableTrafo.Effect
            //    toEffect DefaultSurfaces.vertexColor
            //]

    let drawVertices (m : MDrawingModel) = 
        alist {
            for p in m.points do
                yield sphereISg (m.style.primary.c) (Mod.constant 1.0) p
            for s in m.segments do
                for p in s.points do
                    yield sphereISg (m.style.secondary.c) (Mod.constant 0.5) p
        }
        |> ASet.ofAList 
        |> Sg.set
        |> Sg.uniform "Size" (Mod.constant(5.0))
        |> Sg.effect [
            Shader.ScreenSpaceScale.Effect
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
        ]

    let drawSegment (m :MDrawingModel) = 
        // CAREFUL! duplicated vertices!!! most likely additional edges between segments (startNode is also Endnode)
        let segments = m.segments |> AList.map (fun x -> x.points |> AList.ofPList) |> AList.concat 
        let lines = convertLines false segments
        
        let color = m.style.secondary.c
        let lineWidth = m.style.thickness |> Mod.map (fun x -> x * 0.8)
        let trafo = Mod.constant (Trafo3d.Identity)
        lineISg color lineWidth trafo lines

    let drawLines (m : MDrawingModel) = 
        let lines = convertLines false m.points

        let color = m.style.primary.c
        let lineWidth = m.style.thickness
        let trafo = Mod.constant (Trafo3d.Identity)
        lineISg color lineWidth trafo lines