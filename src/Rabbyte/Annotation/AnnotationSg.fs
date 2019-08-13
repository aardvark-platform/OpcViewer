namespace Rabbyte.Annotation

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI

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
        |> Sg.stencilMode (Mod.constant writeZFail)
        |> Sg.cullMode (Mod.constant CullMode.None)
        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])

    let private fillSG areaPass sg =
        sg
        |> Sg.pass areaPass
        |> Sg.stencilMode (Mod.constant readMaskAndReset)
        //|> Sg.cullMode (Mod.constant CullMode.CounterClockwise)  // for zpass -> backface-culling
        //|> Sg.depthTest (Mod.constant DepthTestMode.Less)        // for zpass -> active depth-test
        |> Sg.cullMode (Mod.constant CullMode.None)
        |> Sg.depthTest (Mod.constant DepthTestMode.None)
        |> Sg.blendMode (Mod.constant BlendMode.Blend)
        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Stencil])

    let stencilArea (pass1:RenderPass) (pass2:RenderPass) sg1 sg2 =
        [
          maskSG pass1 sg1   // one pass by using EXT_stencil_two_side :)
          fillSG pass2 sg2
        ] |> Sg.ofList

    let stencilArea' (pass1:RenderPass) (pass2:RenderPass) sg=
        stencilArea pass1 pass2 sg sg

    let stencilAreaGrouped (pass1:RenderPass) (pass2:RenderPass) (color:IMod<V4f>) sg =

        let fullscreenSg =     
          Aardvark.SceneGraph.SgPrimitives.Sg.fullScreenQuad
          |> Sg.noEvents
          |> Sg.vertexBufferValue DefaultSemantic.Colors color
          |> Sg.effect [ toEffect DefaultSurfaces.vertexColor]

        stencilArea pass1 pass2 sg fullscreenSg

module ClippingVolume =
    
    open OpcViewer.Base

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

    let planeFittedClippingVolume (colorAlpha:IMod<V4f>) (extrusionOffset:IMod<float>) (points:alist<V3d>) =

        let generatePolygonTriangles (extrusionOffset : float) (points:alist<V3d>) =
            let shiftAndPosAndCol =
                points 
                |> AList.toMod
                |> Mod.bind(fun x -> 
                  // increase Precision
                  let shift = x |> PList.tryAt 0 |> Option.defaultValue V3d.Zero
                  let shiftedPoints = x |> PList.toSeq |> Seq.map (fun (y:V3d) -> (y-shift)) |> PList.ofSeq

                  let triangles = createPlaneFittedExtrusionVolume shiftedPoints extrusionOffset
                  let pos = triangles |> Seq.collect (fun t -> [| V3f t.P0; V3f t.P1; V3f t.P2 |]) |> Seq.toArray
                  colorAlpha |> Mod.map (fun cc -> shift, pos, cc))

            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.map (fun (_,p,_) -> p) shiftAndPosAndCol)
            |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.map (fun (_,_,c) -> c) shiftAndPosAndCol)
            |> Sg.translate' (Mod.map (fun (s,_,_) -> s) shiftAndPosAndCol)

        let sg = extrusionOffset |> Mod.map (fun o -> generatePolygonTriangles o points) 
  
        sg |> Mod.toASet |> Sg.set

    let clippingVolume (colorAlpha:IMod<V4f>) (extrusionOffset:IMod<float>) (creation:IMod<ClippingVolumeType>) (points:alist<V3d>) = 
        
        let offsetAndCreation = Mod.map2 (fun o c -> o,c) extrusionOffset creation
        
        points 
        |> AList.toMod
        |> Mod.bind (fun pxs -> 
            offsetAndCreation |> Mod.map(fun (extOff, creation) -> 
          
                // increase Precision
                let shift = 
                    pxs |> PList.tryAt 0 |> Option.defaultValue V3d.Zero

                let shiftsPoints p =
                    p |> PList.map (fun (x:V3d) -> V3f(x-shift)) |> PList.toArray

                let pointsF = shiftsPoints pxs |> Array.skip 1 // undo closing polygon (duplicates not needed) // TODO CHECK THIS!

                let vertices = 
                    let o = float32 extOff
                        
                    let (inner, outer) =
                        match creation with
                        | Point p -> 
                            let shiftedCenter = V3f(p - shift)
                            let dir = pointsF |> Array.map (fun x -> (x-shiftedCenter).Normalized)

                            let inner = dir |> Array.map (fun d -> shiftedCenter + d)   // innerRing is created at 1 unit from center to actual points
                            let outer = Array.map2 (fun x d  -> x + d * o) pointsF dir  // outerRing is created outside by offset
                            inner, outer
                        | Points aps ->
                            // NOTE: EACH POINT MUST HAVE IT'S COUNTERPART! (care about duplicated start/end point)
                            let axisPsF = shiftsPoints aps
                            let dir = Array.map2(fun (p:V3f) (a:V3f) -> ((p-a).Normalized)) pointsF axisPsF 

                            let inner = Array.map2 (+) axisPsF dir                      // innerRing is created at 1 unit from axis to actual points
                            let outer = Array.map2 (fun x d -> x + d * o) pointsF dir   // outerRing is created outside by offset
                            inner, outer
                        | Direction dir ->
                            let d = V3f dir

                            let inner = pointsF |> Array.map (fun x -> x - d * o)   // innerRing is created by negative offset
                            let outer = pointsF |> Array.map (fun x -> x + d * o)   // outerRing is created by offset
                            inner, outer

                    pointsF |> Array.append inner |> Array.append outer

                let indexArray = 
                    let l = pointsF.Length
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
              
                    // SIDE (subdevided by actual points ring)
                    for i in 0 .. l - 1 do
                        indices.Add(i)                      // 0  
                        indices.Add(2*l + i)                // 6
                        indices.Add((i + 1) % l)            // 1
                  
                        indices.Add(2*l + i)                // 6
                        indices.Add(l + i)                  // 3 
                        indices.Add(2*l + ((i + 1) % l))    // 7

                        indices.Add(l + i)                  // 3 
                        indices.Add(l + ((i + 1) % l))      // 4
                        indices.Add(2*l + ((i + 1) % l))    // 7
                  
                        indices.Add(2*l + i)                // 6
                        indices.Add(2*l + ((i + 1) % l))    // 7
                        indices.Add((i + 1) % l)            // 1

                    indices.ToArray()

                Sg.draw IndexedGeometryMode.TriangleList
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant vertices) 
                |> Sg.vertexBufferValue DefaultSemantic.Colors colorAlpha
                |> Sg.index (Mod.constant indexArray)
                |> Sg.translate' (Mod.constant shift)
            )
        ) 
        |> Mod.toASet 
        |> Sg.set

    let drawClippingVolumeDebug clippingVolume = 
    
        let debugVolume =
            clippingVolume
            |> Sg.effect [
                Shader.StableTrafo.Effect
                Shader.DebugColor.Effect
            ]

        let debugShadowVolume =
            debugVolume
            |> Sg.uniform "UseDebugColor" (Mod.constant false)
            |> Sg.depthTest (Mod.constant DepthTestMode.Always)
            |> Sg.cullMode (Mod.constant CullMode.Front)
            |> Sg.blendMode (Mod.constant BlendMode.Blend)
        
        let debugShadowVolumeLines =
            debugVolume
            |> Sg.uniform "UseDebugColor" (Mod.constant true)
            |> Sg.fillMode (Mod.constant FillMode.Line)

        [ debugShadowVolume; debugShadowVolumeLines] |> Sg.ofList

module AnnotationSg =
    open OpcViewer.Base
    open StencilAreaMasking
    open ClippingVolume
    open AnnotationModel

    // grouped...fast -> alpha broken
    let drawAnnotationsFilledGrouped  (model: MAnnotationModel) =
    
        let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
        let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

        model.annotationsGrouped 
        |> AMap.map (fun groupColor annotations -> 
            let colorAlpha = SgUtilities.colorAlpha (Mod.constant groupColor) (Mod.constant 0.5)
            let groupedSg = 
                annotations
                |> AList.map (fun x -> clippingVolume colorAlpha model.extrusionOffset x.clippingVolume x.points)
                |> AList.toASet
                |> Sg.set
                |> Sg.effect [
                    Shader.StableTrafo.Effect
                    toEffect DefaultSurfaces.vertexColor
                ]
            let coloredPolygon =
                groupedSg
                |> StencilAreaMasking.stencilAreaGrouped maskPass areaPass colorAlpha   

            maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
            areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
                
            [
                coloredPolygon
                model.showDebug |> Mod.map (fun show -> if show then groupedSg |> drawClippingVolumeDebug else Sg.empty) |> Sg.dynamic
            ] |> Sg.ofList)
        |> AMap.toASet
        |> ASet.map snd
        |> Sg.set

    // sequentiel...correct Alphablending
    let drawAnnotationsFilledSeq (model: MAnnotationModel) =
    
        let mutable maskPass = RenderPass.after "mask" RenderPassOrder.Arbitrary RenderPass.main
        let mutable areaPass = RenderPass.after "area" RenderPassOrder.Arbitrary maskPass

        model.annotations 
        |> AList.map (fun x -> 
            let colorAlpha = SgUtilities.colorAlpha x.style.primary.c (Mod.constant 0.5)
            let sg = 
                clippingVolume colorAlpha model.extrusionOffset x.clippingVolume x.points
                |> Sg.effect [
                    Shader.StableTrafo.Effect
                    toEffect DefaultSurfaces.vertexColor
                ]
            let coloredPolygon =
                sg  |> StencilAreaMasking.stencilAreaGrouped maskPass areaPass colorAlpha

            maskPass <- RenderPass.after "mask" RenderPassOrder.Arbitrary areaPass
            areaPass <- RenderPass.after "area" RenderPassOrder.Arbitrary maskPass
                
            [
                coloredPolygon
                model.showDebug |> Mod.map (fun show -> if show then sg |> drawClippingVolumeDebug else Sg.empty) |> Sg.dynamic
            ] |> Sg.ofList)
        |> AList.toASet
        |> Sg.set