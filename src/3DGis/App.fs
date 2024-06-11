namespace  ElevationProfileViewer


open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open Aardvark.Application

open Adaptify.FSharp.Core

open Aether
open Aether.Operators


[<AutoOpen>]
module SceneGraphExtension = 

    type OverrideProjTrafo(overrideTrafo : aval<Option<Trafo3d>>, child : ISg) =
        inherit Sg.AbstractApplicator(child)
        member x.OverrideTrafo = overrideTrafo

    [<Rule>]
    type OverrideProjTrafoSem() =
        member x.ProjTrafo(o : OverrideProjTrafo, scope : Ag.Scope) =
            let myTrafo = 
                AVal.map2 (fun (o : Option<Trafo3d>) (r : Trafo3d) -> 
                    match o with
                        | None -> r
                        | Some o -> o
                ) o.OverrideTrafo scope.ProjTrafo
            o.Child?ProjTrafo <- myTrafo

    module Sg = 
        let overrideProjTrafo (o : aval<Option<Trafo3d>>) (sg : ISg) = 
            OverrideProjTrafo(o,sg) :> ISg
        

open FShade
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes

open Aardvark.VRVis.Opc



module App = 
    open Rabbyte.Drawing
    open Rabbyte.Annotation
    open Aardvark.UI.Static
    
    let updateFreeFlyConfig (incr : float) (cam : CameraControllerState) = 
        let s' = cam.freeFlyConfig.moveSensitivity + incr
        Log.line "[App] sensitivity: %A" s'
        let config = 
            { 
            cam.freeFlyConfig with
                panMouseSensitivity       = exp(s') * 0.0025
                dollyMouseSensitivity     = exp(s') * 0.0025
                zoomMouseWheelSensitivity = exp(s') * 0.1
                moveSensitivity           = s'          
            }    
        
        { cam with freeFlyConfig = config }
    
    let toCameraStateLean (view : CameraView) : CameraStateLean = 
        {
          location = view.Location
          forward  = view.Forward
          sky      = view.Sky
        }
    
    let fromCameraStateLean (c : CameraStateLean) : CameraView = 
        CameraView.lookAt c.location (c.location + c.forward) c.sky    
    
    let totalTime = System.Diagnostics.Stopwatch.StartNew()
    
    module ThreadPool =
        let unionMany xs = List.fold ThreadPool.union ThreadPool.empty xs
    
    let threads (m : Model) = 
        let rec time() =
            proclist {
                do! Proc.Sleep 10
                yield Tick (float System.DateTime.Now.Ticks)
                yield! time()
            }
    
        let animations = 
            ThreadPool.add "timer" (time()) ThreadPool.empty
    
        animations
    
      
    let rec update (model : Model) (msg : Message) =   
        match msg with
            | Camera m when model.pickingActive = false -> 
                if model.persToOrthoValue = 0.0 then
                    { model with cameraState = FreeFlyController.update model.cameraState m }
                else
                    model 
            | Message.KeyDown m ->
                match m with
                    | Keys.LeftAlt ->
                        if not model.perspectiveView then
                            update { model with jumpSelectionActive = true; pickingActive = true } Message.AnimateCameraViewSwitch       
                        else 
                            if not model.inJumpedPosition then
                                { model with jumpSelectionActive = true; pickingActive = true }
                            else
                                { model with jumpSelectionActive = false; pickingActive = false }
                    | Keys.LeftCtrl -> 
                        { model with hover3dActive = true }
                    | Keys.LeftShift -> 
                        let p = { model.picking with intersectionPoints = IndexList.Empty }             
                        let clearedDrawingModel = 
                            { 
                            DrawingModel.initial with 
                                style = 
                                { 
                                    DrawingModel.initial.style with 
                                        thickness = 0.0
                                        primary = { c = C4b(50,208,255) }
                                        secondary = { c = C4b(132,226,255) }
                                }
                            } 
                         
                        { 
                            model with 
                                pickingActive = true
                                lineSelectionActive = true
                                picking = p
                                drawing = clearedDrawingModel
                                drawing2 = clearedDrawingModel
                                annotations = AnnotationModel.initial; 
                                numSampledPoints = 0
                                stepSampleSize = {min = 0.01; max = 10000.0; value = model.stepSampleSize.value; step = 0.05; format = "{0:0.00}"}; 
                                linearDistance = 0.0
                                samplingDistance = 0.0
                                minHeight = 0.0
                                maxHeight = 0.0
                                accDistance = 0.0
                                hoverTriangles = [|Triangle3d(V3d.Zero, V3d.Zero, V3d.Zero)|] 
                                pointList = []
                                altitudeList = []
                                errorHitList = []
                                numofPointsinLineList = []
                                hoveredCircleIndex = None
                                markerCone = { height = 0.0; radius = 0.0; color = C4b.Red; trafoRot = Trafo3d.Identity; trafoTrl = Trafo3d.Identity}                       
                        } |> SurfaceSampling.caluculateSVGDrawingPositions
                    | _ -> model
            | Message.KeyUp m ->
                match m with
                    | Keys.LeftAlt ->           
                        if model.perspectiveView && model.jumpSelectionActive && not model.camJumpAnimRunning then 
                            update { model with jumpSelectionActive = false; pickingActive = false }             
                            <| Message.AnimateCameraViewSwitch 
                        else 
                            { model with jumpSelectionActive = false; pickingActive = false }
                    | Keys.LeftShift ->        
                        { model with pickingActive = false; lineSelectionActive = false; } 
                    | Keys.LeftCtrl -> 
                        { 
                            model with 
                                hover3dActive = false; 
                                markerCone = 
                                    { 
                                        height =0.0
                                        radius = 0.0
                                        color = C4b.Red
                                        trafoRot = Trafo3d.Identity
                                        trafoTrl = Trafo3d.Identity
                                    } 
                        }
                    | Keys.Delete ->            
                        { model with picking = PickingApp.update model.picking (PickingAction.ClearPoints) }
                    | Keys.Back ->
                        { model with picking = PickingApp.update model.picking (PickingAction.RemoveLastPoint) }
                    | Keys.PageUp ->             
                        { model with cameraState = model.cameraState |>  updateFreeFlyConfig +0.5 }
                    | Keys.PageDown ->             
                        { model with cameraState = model.cameraState |>  updateFreeFlyConfig -0.5 }
                    | Keys.Space ->    
                        Log.line "[App] saving camstate"
                        model.cameraState.view |> toCameraStateLean |> OpcSelectionViewer.Serialization.save ".\camstate" |> ignore
                        model
                    | Keys.Enter ->
                        let finished = { model with drawing = DrawingApp.update model.drawing DrawingAction.Finish } // TODO add dummy-hitF
                        let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, None))
                        { finished with annotations = newAnnotation; drawing = DrawingModel.initial} // clear drawingApp
                    | Keys.B ->
                        update model Message.AnimateCameraComplete
                    | Keys.N ->
                        update model Message.AnimateCameraReturn
                    | Keys.J ->
                        update model Message.AnimateCameraJump
                    | Keys.O ->
                        update model Message.AnimateCameraViewSwitch
                    | _ -> model
            | Message.Down(button,pos) ->   
                match button with
                    | MouseButtons.Right -> 
                        let model = { model with mouseDragStart = pos }
                        { model with zoom = true }                   
                    | MouseButtons.Left -> 
                        let model = { model with mouseDragStart = pos }
                        { model with pan = true }      
                    | _ -> model
            | Message.Up button ->        
                match button with
                    | MouseButtons.Right -> 
                         { model with zoom = false }               
                    | MouseButtons.Left -> 
                         { model with pan = false }        
                    | _ -> model
            | Message.Zoom pos ->
              if model.persToOrthoValue = 1.0 then
                  let newLoc = 
                      if pos.Y > model.mouseDragStart.Y then
                          model.cameraState.view.Location + model.cameraState.view.Forward*10.0
                      elif pos.Y < model.mouseDragStart.Y then
                          model.cameraState.view.Location - model.cameraState.view.Forward*10.0
                      else
                          model.cameraState.view.Location
                        
                  let cam = 
                      CameraView(
                          model.cameraState.view.Sky,
                          newLoc,
                          model.cameraState.view.Forward, 
                          model.cameraState.view.Up, 
                          model.cameraState.view.Right
                      )
                        
                  let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }
                        
                  { model with cameraState = newCamState }

              else
                  model
            | Message.Pan pos ->
                if model.persToOrthoValue = 1.0 && not model.hover3dActive then
                    let direction = -(pos - model.mouseDragStart)
                    
                    let camPos = model.cameraState.view.Location
                
                    let r = camPos.Length
                    let normDir = V2d((float)direction.X / direction.Length,  (float)direction.Y / direction.Length)
                    let theta = Math.Acos (camPos.Z / r) + 0.000005 * normDir.Y
                    let phi = Math.Atan2 (camPos.Y, camPos.X) + 0.000005 * normDir.X
                    
                    let newCord = 
                        V3d(
                            r * Math.Sin(theta) * Math.Cos(phi),
                            r * Math.Sin(theta) * Math.Sin(phi),
                            r * Math.Cos(theta)
                        ) 
                
                    let cam =  
                        CameraView(
                            model.cameraState.view.Sky, 
                            newCord, 
                            model.cameraState.view.Forward, 
                            model.cameraState.view.Up, 
                            model.cameraState.view.Right
                        )
                    
                    let newCamState : CameraControllerState =
                        { model.cameraState with view = cam }
                
                    { model with cameraState = newCamState }        
                else
                    model   
            | Message.AnimateCameraViewSwitch ->
                let duration = TimeSpan.FromSeconds 0.4
                let total = DateTime.Now.AddTicks (duration.Ticks)
                
                { model with cameraAnimEndTime = float total.Ticks; camViewAnimRunning = true }
            | Message.AnimateCameraJump ->
                let duration = TimeSpan.FromSeconds 2.0
                let total = DateTime.Now.AddTicks (duration.Ticks)
                let targetPos = model.selectedJumpPosition
                
                { 
                    model with 
                        cameraAnimEndTime = float total.Ticks
                        camJumpAnimRunning = true
                        targetPosition = targetPos
                        originalCamPos = model.cameraState.view.Location 
                }
            | Message.AnimateCameraComplete ->
                let duration = TimeSpan.FromSeconds 0.4
                let total = DateTime.Now.AddTicks (duration.Ticks)
                let targetPos = model.selectedJumpPosition
                
                { 
                    model with 
                        cameraAnimEndTime = float total.Ticks
                        camCompAnimRunning = true
                        camViewAnimRunning = true
                        camJumpAnimRunning = false
                        targetPosition = targetPos
                        originalCamPos = model.cameraState.view.Location 
                }
            | Message.AnimateCameraReturn ->
                let duration = TimeSpan.FromSeconds 2.0
                let total = DateTime.Now.AddTicks (duration.Ticks)
                
                { 
                    model with 
                        cameraAnimEndTime = float total.Ticks
                        camRetAnimRunning = true
                        camViewAnimRunning = false
                        camJumpAnimRunning = true
                        targetPosition = model.cameraState.view.Location
                }
            | Message.Tick t ->       
                if not model.camCompAnimRunning && not model.camRetAnimRunning && model.camViewAnimRunning then
                    CameraAnimation.orthographicPerspectiveAnimation model t
                elif not model.camCompAnimRunning && not model.camRetAnimRunning && model.camJumpAnimRunning then
                    CameraAnimation.cameraJumpAnimation model t
                elif model.camCompAnimRunning && model.camViewAnimRunning then
                    CameraAnimation.orthographicPerspectiveAnimation model t
                elif model.camCompAnimRunning && model.camJumpAnimRunning then
                    CameraAnimation.cameraJumpAnimation model t
                elif model.camRetAnimRunning && model.camJumpAnimRunning then
                    CameraAnimation.cameraReturnJumpAnimation model t
                elif model.camRetAnimRunning && model.camViewAnimRunning then
                    CameraAnimation.orthographicPerspectiveAnimation model t
                else 
                    model
            | PickingAction msg ->    
                let pickingModel, drawingModel =
                    match msg with
                    | HitSurface (a,b) -> 
                        let updatePickM = PickingApp.update model.picking (HitSurface (a,b))
                        let lastPick = updatePickM.intersectionPoints |> IndexList.tryFirst         
                        
                        let updatedDrawM =
                            match lastPick with
                            | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                            | None -> model.drawing
                        
                        updatePickM, updatedDrawM
                    | _ -> PickingApp.update model.picking msg, model.drawing
                    
                let newDrawingModel = 
                    { drawingModel with 
                        style = 
                        { drawingModel.style with 
                            thickness = 2.0; 
                            primary = { c = C4b(50,208,255) }
                            secondary = { c = C4b(132,226,255) } 
                        } 
                    } 
                
                if model.jumpSelectionActive && not model.camViewAnimRunning && pickingModel.intersectionPoints.AsList.Length > 0 then
                    update 
                        { model with 
                            selectedJumpPosition = pickingModel.intersectionPoints.Item(0)
                            jumpSelectionActive = false
                            inJumpedPosition = true 
                        } <| Message.AnimateCameraJump            
                elif model.camViewAnimRunning then
                    model
                elif model.lineSelectionActive && pickingModel.intersectionPoints.AsList.Length >= 2 then       
                    SurfaceSampling.caluculateSVGDrawingPositions <| SurfaceSampling.sampleSurfacePointsForCutView model pickingModel newDrawingModel
                else
                    { model with picking = pickingModel; drawing = newDrawingModel }          
            | DrawingAction msg -> 
                { model with drawing = DrawingApp.update model.drawing msg }
            | AnnotationAction msg -> 
                { model with annotations = AnnotationApp.update model.annotations msg}   
            | SetProjection a ->
                if model.inJumpedPosition then
                    model
                elif a = O && model.perspectiveView then
                    update { model with currentOption = a } Message.AnimateCameraViewSwitch
                elif a = P && not model.perspectiveView then
                    update { model with currentOption = a } Message.AnimateCameraViewSwitch
                else
                    { model with currentOption = a }
            | SetSamplingRate f -> 
                if model.picking.intersectionPoints.AsList.Length >= 2 then 
                     SurfaceSampling.sampleSurfacePointsForCutView 
                         { model with 
                             stepSampleSize = Numeric.update model.stepSampleSize f 
                         } 
                         model.picking 
                         model.drawing
                     |> SurfaceSampling.caluculateSVGDrawingPositions
                else 
                    { model with stepSampleSize = Numeric.update model.stepSampleSize f }
            | MouseWheel v ->       
                let newCutViewZoom =
                    if v = V2d(0,1) then
                        Math.Max (model.cutViewZoom + 2.0, 0.0)
                    else
                        Math.Max (model.cutViewZoom - 2.0, 0.0)
                
                let originalXDim = Math.Round ((float) model.cutViewDim.X * (100.0/(100.0+model.cutViewZoom)))
                let newXDim = int <| Math.Round (originalXDim * ((100.0 + newCutViewZoom)/100.0))
                
                let orignalOffset = model.offsetUIDrawX * (float) model.cutViewDim.X / originalXDim
                let newXOffset = (orignalOffset * float originalXDim) / float newXDim
                 
                { model with 
                    cutViewZoom = newCutViewZoom
                    cutViewDim = V2i(newXDim, model.cutViewDim.Y)
                    offsetUIDrawX = newXOffset 
                } |> SurfaceSampling.caluculateSVGDrawingPositions 
            | ResizeRenderView d ->
                { model with renderViewDim = d}
            | ResizeCutView d ->
                { model with cutViewDim = d}
            | HoveredCircleEnter id -> 
                match model.hoveredCircleIndex with 
                | None -> 
                    update { model with hoveredCircleIndex = Some id} Message.HighlightIn3DView
                | Some oldID when not (id = oldID) -> 
                    update { (update model HoveredCircleLeave) with hoveredCircleIndex = Some id} Message.HighlightIn3DView
                | _ -> model
            | HoveredCircleLeave -> 
                if model.hoveredCircleIndex.IsSome then
                    { model with 
                        hoveredCircleIndex = None 
                        markerCone = { height =0.0; radius = 0.0; color = C4b.Red; trafoRot = Trafo3d.Identity; trafoTrl = Trafo3d.Identity}                
                    }
                else
                    model
            | HighlightIn3DView ->
                let pointList = model.pointList
                let numofPointsinLineList = model.numofPointsinLineList
                let currentHoveredIndex = model.hoveredCircleIndex.Value
                
                let intersectionPoints = (model.picking.intersectionPoints).AsListBackward
                
                let lineOrigin, lineEnd =
                    let rec getIndices i curV = 
                        if (curV - numofPointsinLineList.Item(i)) < 0 then
                            if i = 0 then
                                (intersectionPoints.Item(i)), (intersectionPoints.Item(i+1))
                            elif i = numofPointsinLineList.Length-1 then
                                (intersectionPoints.Item(i)), (intersectionPoints.Item(i+1))
                            else
                                (intersectionPoints.Item(i)), (intersectionPoints.Item(i+1))
                        else
                            getIndices (i+1) (curV - numofPointsinLineList.Item(i))
                
                    getIndices 0 currentHoveredIndex
                
                let line = lineEnd-lineOrigin  
                
                let projectedPointOnLine = 
                    let a = line
                    let b = pointList.Item(currentHoveredIndex) - lineOrigin
                    lineOrigin + a * (((Vec.Dot(b,a)/(a.Length*a.Length)) * a)/a.Length).Length
                
                let height =
                    let h = (projectedPointOnLine - pointList.Item(currentHoveredIndex)).Length
                    if projectedPointOnLine.Length >= pointList.Item(currentHoveredIndex).Length then
                        h
                    else 
                        -h
                               
                { model with 
                    markerCone = 
                        { 
                        height = height
                        radius = ((pointList.Item(currentHoveredIndex) - model.cameraState.view.Location).Length)/100.0
                        color = C4b.Red 
                        trafoRot = Trafo3d.RotateInto(V3d.OOI, model.opcCenterPosition.Normalized ) 
                        trafoTrl = Trafo3d.Translation(pointList.Item(currentHoveredIndex))
                        }                 
                }        
            | Hoverin3D pos ->
                if model.hover3dActive then
                    let pointList = model.pointList 
                
                    let fray = FastRay3d(V3d.Zero, pos.Normalized)   
                    let hitpoint=
                        model.picking.pickingInfos 
                        |> HashMap.tryFind model.opcBox
                        |> Option.map (fun opcData -> 
                            match OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some opcData.kdTree) fray with
                            | Some t -> 
                                fray.Ray.GetPointOnRay t 
                            | None -> 
                                pos
                            )
                
                    let minElementIndex = 
                        let rec indexofMin i currMinDist index =
                            if i = pointList.Length - 1 then indexofMin (i-1) (pointList.Item(i) - hitpoint.Value).Length i 
                            elif i > 0 then 
                                let dist = (pointList.Item(i) - hitpoint.Value).Length
                                if currMinDist > dist then indexofMin (i-1) dist i else indexofMin (i-1) currMinDist index
                            else
                                index
                        indexofMin (pointList.Length-1) -1.0 -1
                
                    update model (HoveredCircleEnter minElementIndex)
                else
                    model
            | UpdateTriagleStrips pos ->
                pos |> ignore
                
                let numIntersectionPoints = model.picking.intersectionPoints.Count
                
                if numIntersectionPoints >= 2 then
                    let camLoc = model.cameraState.view.Location
                
                    let rec createTriangleStrips i arr = 
                        if i >= 1 then
                            let a = model.picking.intersectionPoints.Item(i)
                            let b = model.picking.intersectionPoints.Item(i-1)
                        
                            let aDir = b-a
                            let bDir = a-b
                        
                            let aCorrected = a + bDir / 5.0 
                            let bCorrected = b + aDir / 5.0 
                        
                            let aV = aCorrected - camLoc
                            let bV = bCorrected - camLoc
                        
                            let aDublicated1 = V4d(aCorrected,0.0)
                            let aDublicated2 = V4d(aCorrected,1.0)
                        
                            let bDublicated1 = V4d(bCorrected,0.0)
                            let bDublicated2 = V4d(bCorrected,1.0)
                        
                            let stripWidth = (aV.Length + bV.Length)  / 10.0
                        
                            let aCross, bCross = 
                                if numIntersectionPoints > 2 then
                                    if i > 1 then
                                        (Vec.Cross(bDir,b)).Normalized, (Vec.Cross(bDir,b)).Normalized
                                    else 
                                        (Vec.Cross(aDir,a)).Normalized, (Vec.Cross(aDir,a)).Normalized
                                else
                                    (Vec.Cross(bDir,bV)).Normalized, (Vec.Cross(aV,aDir)).Normalized
                            
                        
                            let a0Moved = aDublicated1.XYZ + aCross * (aDublicated1.W - 0.5) * stripWidth 
                            let a1Moved = aDublicated2.XYZ + aCross * (aDublicated2.W - 0.5) * stripWidth 
                        
                            let b0Moved = bDublicated1.XYZ + bCross * (bDublicated1.W - 0.5) * stripWidth 
                            let b1Moved = bDublicated2.XYZ + bCross * (bDublicated2.W - 0.5) * stripWidth 
                        
                            let triangle1 = Triangle3d(a0Moved,a1Moved,b0Moved)
                            let triangle2 = Triangle3d(a1Moved,b0Moved,b1Moved)
                        
                            createTriangleStrips (i-1) (Array.append [|triangle1;triangle2|] arr)
                        else 
                            arr      
                    let triangleStrips = createTriangleStrips (numIntersectionPoints-1) [||]            
                    { model with hoverTriangles = triangleStrips}
                else
                    model
            | UpdateDockConfig cfg ->
                { model with dockConfig = cfg }
            | _ -> model
    
    
    let view (m : AdaptiveModel) =
        let opcs = 
            m.opcInfos
                |> AMap.toASet
                |> ASet.map(fun info -> Sg.createSingleOpcSg (AVal.constant None) m.pickingActive m.cameraState.view info)
                |> Sg.set
                |> Sg.effect [ 
                  toEffect Shader.stableTrafo
                  toEffect DefaultSurfaces.diffuseTexture  
                  ]  

        let projTrafo =
            adaptive {           
                let radians = 20.0 * Math.PI / 180.0
                let ratioSizePerDepth = Math.Atan(radians) * 2.0
            
                let! camPos = m.cameraState.view
                let! opcPos = m.opcCenterPosition
            
                let plane = Plane3d(camPos.Location, camPos.Location + camPos.Right, camPos.Location + camPos.Up)
                let distance = (opcPos - plane.NearestPoint(opcPos)).Length
            
                let! renderViewDim = m.renderViewDim
                
                let aspect = (float renderViewDim.X) / (float renderViewDim.Y)
            
                let sizeY = ratioSizePerDepth * distance
                let sizeX = ratioSizePerDepth * distance * aspect
            
                let o = Frustum.projTrafo (Frustum.ortho (Box3d(V3d(-sizeX, -sizeY, 0.0),V3d(sizeX, sizeY, 2.0*distance))))
                let p = Frustum.projTrafo (Frustum.perspective 60.0 0.01 10000.0 aspect)
                
                let! t = m.persToOrthoValue
                let t = t ** (1.0 / 50.0)
            
                return Trafo3d (
                    o.Forward * t + (1.0 - t) * p.Forward,
                    o.Backward * t + (1.0 - t) * p.Backward
                )       
            }
    
        let projFrustum =
            adaptive {                       
                let radians = 20.0 * Math.PI / 180.0
                let ratioSizePerDepth = Math.Atan(radians) * 2.0
            
                let! camPos = m.cameraState.view
                let! opcPos = m.opcCenterPosition
            
                let plane = Plane3d(camPos.Location, camPos.Location + camPos.Right, camPos.Location + camPos.Up)
                let distance = (opcPos - plane.NearestPoint(opcPos)).Length
            
                let! renderViewDim = m.renderViewDim
                
                let aspect = (float renderViewDim.X) / (float renderViewDim.Y)
            
                let sizeY = ratioSizePerDepth * distance
                let sizeX = ratioSizePerDepth * distance * aspect
            
                let o = Frustum.ortho <| Box3d(V3d(-sizeX, -sizeY, 0.0), V3d(sizeX, sizeY, 2.0*distance))
                let p = Frustum.perspective 60.0 0.01 10000.0 aspect
                
                let! t = m.persToOrthoValue
            
                if t = 0.0 then
                    return p
                else 
                    return o
            
            }
        
        let near = m.mainFrustum |> AVal.map(fun x -> x.near)
        let far = m.mainFrustum |> AVal.map(fun x -> x.far)
        
        let filledPolygonSg, afterFilledPolygonRenderPass = 
            m.annotations 
            |> AnnotationApp.viewGrouped near far (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)
    
        let afterFilledPolygonSg = 
            [
              m.drawing |> DrawingApp.viewPointSize near far (AVal.constant 2.0) (AVal.constant 10.0)
            ] 
            |> Sg.ofList
            |> Sg.pass afterFilledPolygonRenderPass
          
        let afterFilledPolygonSg2 = 
            [
              m.drawing2 |> DrawingApp.viewPointSize near far (AVal.constant 0.0) (AVal.constant 10.0)
            ] 
            |> Sg.ofList
            |> Sg.pass afterFilledPolygonRenderPass
    
        let trafo =
            adaptive {
                let! running = m.camViewAnimRunning
                if running then 
                    let! t = projTrafo
                    return Some t 
                else return None
            }   
    
        let markerCone = 
            Sg.cone 
                16 
                (m.markerCone |> AVal.map ( fun x -> x.color)) 
                (m.markerCone |> AVal.map ( fun x -> x.radius)) 
                (m.markerCone |> AVal.map ( fun x -> x.height))
                |> Sg.noEvents
                |> Sg.effect [
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                ]
                |> Sg.trafo (m.markerCone |> AVal.map ( fun x -> x.trafoRot))
                |> Sg.trafo (m.markerCone |> AVal.map ( fun x -> x.trafoTrl))
             
        
        let hoverTriangle = 
            Sg.empty 
                |> Sg.pickable' 
                    ( m.hoverTriangles |> 
                        AVal.map ( fun s -> { shape = PickShape.Triangles(KdTree(Spatial.triangle, s)); trafo = Trafo3d.Identity } )
                    )
                |> Sg.noEvents
                |> Sg.requirePicking
                |> Sg.withEvents[ 
                    Sg.onEnter (fun x -> Hoverin3D x) 
                    Sg.onLeave (fun _ -> HoveredCircleLeave)   
                   ]       
                
        let scene = 
            [
                opcs |> Sg.map PickingAction
                filledPolygonSg |> Sg.map PickingAction
                afterFilledPolygonSg |> Sg.map PickingAction
                afterFilledPolygonSg2 |> Sg.map PickingAction
                markerCone
                hoverTriangle
            ]
            |> Sg.ofList
            |> Sg.overrideProjTrafo trafo
            |> Sg.noEvents
            |> Sg.pass RenderPass.main
            
        let onResize (cb : V2i -> 'msg) =
            onEvent "onresize" ["{ X: $(document).width(), Y: $(document).height()  }"] (List.head >> Pickler.json.UnPickleOfString >> cb)
    
        let renderControl (state : AdaptiveModel) (f : Message -> Message)=
            FreeFlyController.controlledControl m.cameraState Camera projFrustum
                (AttributeMap.ofListCond [ 
                  always (style "width: 100%; height:100%"; )
                  always (attribute "showFPS" "true"; )      
                  always (attribute "useMapping" "true")
                  always (attribute "data-renderalways" "false")
                  always (attribute "data-samples" "4")
                  always (onKeyDown (Message.KeyDown))
                  always (onKeyUp (Message.KeyUp))
                  always (onMouseDown (fun b p -> f (Message.Down(b,p))))
                  always (onMouseUp (fun b p -> f (Message.Up(b))))
                  onlyWhen state.zoom (onMouseMove (Message.Zoom ))
                  onlyWhen state.pan (onMouseMove (Message.Pan >> f))
                  onlyWhen state.hover3dActive (onMouseMove (Message.UpdateTriagleStrips))
                ])
                scene 
     
        let dropDownValues = m.dropDownOptions |> AMap.map (fun k v -> text v)
        
        let dependencies = 
            Html.semui @ 
            [ 
               { kind = Stylesheet; name = "CutView6.css"; url = "CutView6.css" }
            ]  
    
        page (fun request -> 
            match Map.tryFind "page" request.queryParams with
            | Some "render" ->
                require Html.semui ( 
                    body[onResize (Message.ResizeRenderView >> id)][
                        div [clazz "ui"; style "background: #1B1C1E"] [renderControl m id]
                        onBoot "$(window).trigger('resize')" (
                            body [onResize (Message.ResizeRenderView >> id)] []
                        )
                    ]
                )   
            | Some "controls" -> 
                body [style "width: 100%; height:100%; background: transparent; overflow-y:visible"] [
                    div[style "color:white; margin: 5px 15px 5px 5px"] [
                        Html.SemUi.accordion "Camera" "camera" true [     
                            div [ clazz "item" ] [ 
                                dropdownUnclearable [ clazz "ui simple inverted selection dropdown" ] dropDownValues m.currentOption SetProjection
                            ]                                                                       
                        ]
                        br[]
                        br[]
                        Html.SemUi.accordion "Elevation Info" "map" true [     
                            Html.table [  
                                Html.row "Number of Points:" [Incremental.text (m.numSampledPoints |> AVal.map (fun f -> f.ToString())) ]
                                Html.row "Linear Distance:" [Incremental.text (m.linearDistance |> AVal.map (fun f -> Math.Round(f,2).ToString())) ]
                                Html.row "Accumulated Distance:" [Incremental.text (m.accDistance |> AVal.map (fun f -> f.ToString())) ]
                                Html.row "Sampling Rate:"  [Numeric.view m.stepSampleSize |> UI.map Message.SetSamplingRate]                             
                                Html.row "Sampling Distance:" [Incremental.text (m.samplingDistance |> AVal.map (fun f -> Math.Round(f,4).ToString())) ]
                                Html.row "Min Height:" [Incremental.text (m.minHeight |> AVal.map (fun f -> Math.Round(f,2).ToString())) ]
                                Html.row "Max Height:" [Incremental.text (m.maxHeight |> AVal.map (fun f -> Math.Round(f,2).ToString())) ]                                         
                            ]                       
                        ]
                    ]
                ]       
            | Some "cutview" ->
                require dependencies ( 
                    body [style "width: 100%; height: 100%; background: transparent; overflow-x: scroll "; onResize (Message.ResizeCutView >> id)] [                  
                        DrawingProfileView.drawSVGElements m
                    
                        onBoot "$(window).trigger('resize')" (
                            body [onResize (Message.ResizeCutView >> id)] []                      
                        ) 
                    ]              
                )
            | Some other -> 
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ]  
            | None -> 
                m.dockConfig
                    |> docking [
                      style "width:100%; height:100%; background:#F00"
                      onLayoutChanged UpdateDockConfig ]
            )
           
            
    let app dir axisFile (rotate : bool) =
        OpcSelectionViewer.Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)
    
        let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton
    
        let patchHierarchies =         
          [ 
              for h in phDirs do
                  yield PatchHierarchy.load OpcSelectionViewer.Serialization.binarySerializer.Pickle OpcSelectionViewer.Serialization.binarySerializer.UnPickle (h |> OpcPaths)
          ]    
    
        let box = 
            patchHierarchies 
                |> List.map(fun x -> x.tree |> QTree.getRoot) 
                |> List.map(fun x -> x.info.GlobalBoundingBox)
                |> List.fold (fun a b -> Box3d(a, b)) Box3d.Invalid
    
        let opcInfos = 
            [
                for h in patchHierarchies do
                  
                  let rootTree = h.tree |> QTree.getRoot
                  let kd = KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ OpcSelectionViewer.Serialization.binarySerializer false false (fun _ _ -> failwith "no triangleset function") false false Aardvark.VRVis.Opc.KdTrees.KdTreeParameters.legacyDefault

                  yield {
                      patchHierarchy = h
                      kdTree         = Aardvark.VRVis.Opc.KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath kd
                      localBB        = rootTree.info.LocalBoundingBox 
                      globalBB       = rootTree.info.GlobalBoundingBox
                      neighborMap    = HashMap.empty
                  }
            ]
            |> List.map (fun info -> info.globalBB, info)
            |> HashMap.ofList      
                        
        let up = V3d.OOI 
        let sky = box.Center.Normalized
        let r = Trafo3d.RotateInto(V3d.OOI, sky)
        let camPos = V3d(box.Center.X,box.Center.Y,box.Center.Z)+r.Forward.TransformPos(V3d(0.0,0.0,10.0*2.0*100.0))
        
        let restoreCamState : CameraControllerState =
            { FreeFlyController.initial with view = CameraView.lookAt camPos V3d.Zero up } 
    
    
        let camState = restoreCamState
    
        let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
        let camState = camState |> ffConfig ^=  CameraControllerState.freeFlyConfig_
    
        let initialDockConfig = 
            config {
                content (
                    vertical 10.0 [
                        horizontal 7.0 [
                            element { id "render"; title "Render View"; weight 4.5 }
                            element { id "controls"; title "Controls"; weight 2.5 } 
                        ]
                        element { id "cutview"; title "Elevation Profile Viewer"; weight 3.0 }
                    ]    
                )
                appName "OpcSelectionViewer"
                useCachedConfig true
            }
       
    
        let initialModel : Model = 
            { 
                cameraState                     = camState
                mainFrustum                     = Frustum.perspective 60.0 0.01 10000.0 1.0
                fillMode                        = FillMode.Fill                    
                patchHierarchies                = patchHierarchies          
                axis                            = None
                                                
                threads                         = FreeFlyController.threads camState |> ThreadPool.map Camera
                boxes                           = List.empty 
                                                
                pickingActive                   = false
                opcInfos                        = opcInfos
                picking                         = { PickingModel.initial with pickingInfos = opcInfos }
                drawing                         = DrawingModel.initial
                drawing2                        = DrawingModel.initial
                annotations                     = AnnotationModel.initial
                dockConfig                      = initialDockConfig  
                mouseDragStart                  = V2i.Zero
                zoom                            = false
                pan                             = false
                persToOrthoValue                = 1.0
                camViewAnimRunning              = false
                cameraAnimEndTime               = 0.0
                targetPosition                  = V3d.Zero        
                perspectiveView                 = false  
                camJumpAnimRunning              = false
                originalCamPos                  = V3d.Zero
                camCompAnimRunning              = false
                camRetAnimRunning               = false
                opcCenterPosition               = box.Center
                selectedJumpPosition            = V3d.Zero
                jumpSelectionActive             = false
                inJumpedPosition                = false
                lineSelectionActive             = false
                opcBox                          = box
                numSampledPoints                = 0
                stepSampleSize                  = {min = 0.01; max = 10000.0; value = 2.0; step = 0.05; format = "{0:0.00}"}
                linearDistance                  = 0.0
                accDistance                     = 0.0
                maxHeight                       = 0.0
                minHeight                       = 0.0
                dropDownOptions                 = HashMap.ofList [P, "Perspective"; O, "Orthographic";]
                currentOption                   = O
                offsetUIDrawX                   = 0.05
                offsetUIDrawY                   = 0.15
                pointList                       = []
                altitudeList                    = []
                errorHitList                    = []
                samplingDistance                = 0.0
                cutViewZoom                     = 0.0
                renderViewDim                   = V2i(1,1)
                cutViewDim                      = V2i(1,1)
                svgPointsCoord                  = ""
                svgPointsErrorCoord             = ""
                svgSurfaceUnderLineCoord        = ""
                svgSurfaceUnderLineErrorCoord   = ""
                svgCircleSize                   = 0.0
                hoveredCircleIndex              = None
                hover3dActive                   = false   
                markerCone                      = { height =0.0; radius = 0.0; color = C4b.Red; trafoRot = Trafo3d.Identity; trafoTrl = Trafo3d.Identity}
                numofPointsinLineList           = []
                hoverTriangles                  = [|Triangle3d(V3d.Zero, V3d.Zero, V3d.Zero)|]
            }
    
        {
            initial   = initialModel             
            update    = update
            view      = view          
            threads   = threads 
            unpersist = Unpersist.instance<Model, AdaptiveModel>
        }

      


  