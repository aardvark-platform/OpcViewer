namespace  ElevationProfileViewer


open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open Aardvark.Application


[<AutoOpen>]
module SceneGraphExtension = 

    type OverrideProjTrafo(overrideTrafo : IMod<Option<Trafo3d>>, child : ISg) =
        inherit Sg.AbstractApplicator(child)
        member x.OverrideTrafo = overrideTrafo

    [<Semantic>]
    type OverrideProjTrafoSem() =
        member x.ProjTrafo(o : OverrideProjTrafo) =
            let myTrafo = 
                Mod.map2 (fun (o : Option<Trafo3d>) (r : Trafo3d) -> 
                    match o with
                        | None -> r
                        | Some o -> o
                ) o.OverrideTrafo o?ProjTrafo
            o.Child?ProjTrafo <- myTrafo

    module Sg = 
        let overrideProjTrafo (o : IMod<Option<Trafo3d>>) (sg : ISg) = 
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

  let cameraReturnJumpAnimation (model : Model) (t : float) = 
    if model.cameraAnimEndTime > t then 
        let durationTicks = TimeSpan.FromSeconds 2.0 
        let remaingTicks = model.cameraAnimEndTime - t
        let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

        let camToTarget = model.targetPosition - model.originalCamPos

        let cam = 
            CameraView(model.cameraState.view.Sky, model.originalCamPos + camToTarget * percent, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

        let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

        { model with cameraState = newCamState }
     
    elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
        let duration = TimeSpan.FromSeconds 0.4
        let total = DateTime.Now.AddTicks (duration.Ticks)
        { model with camJumpAnimRunning = false; camViewAnimRunning = true; cameraAnimEndTime = float total.Ticks; inJumpedPosition = false }     
    else 
        model

  let cameraJumpAnimation (model : Model) (t : float) = 
    if model.cameraAnimEndTime > t then 
        let durationTicks = TimeSpan.FromSeconds 2.0 
        let remaingTicks = model.cameraAnimEndTime - t
        let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

        let targetToCam = model.originalCamPos - model.targetPosition
        let offset = targetToCam/10.0
        let targetPos = model.targetPosition + offset
        let camToTarget = targetPos - model.originalCamPos

        let cam = 
            CameraView(model.cameraState.view.Sky, model.originalCamPos + camToTarget * percent, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

        let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

        { model with cameraState = newCamState }
     
    elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
        { model with camJumpAnimRunning = false; camCompAnimRunning = false }     
    else 
        model

  let orthographicPerspectiveAnimation (model : Model) (t: float) = 
      if model.cameraAnimEndTime > t then
         if model.perspectiveView then
            let durationTicks = TimeSpan.FromSeconds 0.4 
            let remaingTicks = model.cameraAnimEndTime - t

            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            { model with persToOrthoValue = percent }

         else
            let durationTicks = TimeSpan.FromSeconds 0.4 
            let remaingTicks = model.cameraAnimEndTime - t

            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            { model with persToOrthoValue = 1.0 - percent }


      elif model.camViewAnimRunning && model.cameraAnimEndTime < t then
        if model.perspectiveView then
            { model with perspectiveView = false; persToOrthoValue = 1.0; camViewAnimRunning = false; currentOption = Some O }
        else
            let duration = TimeSpan.FromSeconds 2.0
            let total = DateTime.Now.AddTicks (duration.Ticks)
            { model with perspectiveView = true; persToOrthoValue = 0.0; camViewAnimRunning = false; camJumpAnimRunning = model.camCompAnimRunning; camRetAnimRunning = false; cameraAnimEndTime = float total.Ticks;  currentOption = Some P }
      else
        model

  let caluculateSVGDrawingPositions (model : Model) = 
      let xOffset = model.offsetUIDrawX
      let yOffset = model.offsetUIDrawY
                                          
      let dimensions = model.cutViewDim
      let xWidth = (float) dimensions.X 
      let yWidth = (float) dimensions.Y
     
      let space = " "
      let comma = ","

      let sX = (sprintf "%f" (xOffset * xWidth)) 

      let wX = (sprintf "%f" ((1.0-xOffset) * xWidth)) 
      let wY = (sprintf "%f" ((1.0-yOffset) * yWidth))
     
      let altitudeList = model.altitudeList
      let errorHitList = model.errorHitList
     
      let maxAltitude = model.maxHeight
      let minAltitude = model.minHeight
      let range = maxAltitude - minAltitude
                                      
      let drawingAvailableCoord, drawingMissingCoord, drawingMissingSurfaceCoord = 
          let mutable currentCorrectPoints = ""
          let mutable currentErrorPoints = ""
          let mutable currentErrorSurfacePoints = ""

          for i = 0 to altitudeList.Length-1 do
              let currentX = (100.0/ (float) (altitudeList.Length-1)) * (float i)
              let currentY = (altitudeList.Item(i) - minAltitude) / range
     
              let normalizeX = (sprintf "%f" ((xOffset+ (currentX/100.0) * (1.0-xOffset*2.0)) * xWidth) )
              let normalizeY = (sprintf "%f" ((yOffset+ (1.0-currentY) * (1.0-yOffset*2.0)) * yWidth) )
              currentCorrectPoints <- currentCorrectPoints + normalizeX + comma + normalizeY + space

              if errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = 0 then
                  currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space
                  let initY = (sprintf "%f" ((1.0-yOffset) * yWidth))
                  let initialPointsCoord = normalizeX + comma + initY + space + normalizeX + comma + normalizeY + space
                  currentErrorSurfacePoints <- currentErrorSurfacePoints + initialPointsCoord
              elif errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = -1 && errorHitList.Item(i+1) = -1 then
                  currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space
                  currentErrorSurfacePoints <- currentErrorSurfacePoints + normalizeX + comma + normalizeY + space
              elif errorHitList.Item(i) = -1 && errorHitList.Item(i+1) = 0 then
                  currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space + space
                  let endY = (sprintf "%f" ((1.0-yOffset) * yWidth))
                  let endPointsCoord = normalizeX + comma + normalizeY + space + normalizeX + comma + endY + space
                  currentErrorSurfacePoints <- currentErrorSurfacePoints + endPointsCoord

          currentCorrectPoints, currentErrorPoints, currentErrorSurfacePoints                       
                                      
      let initialPointsCoord = wX + comma + wY + space + sX + comma + wY + space
      let surfaceUnderLineCoord =  initialPointsCoord + drawingAvailableCoord 

      let circleSize = 
          let x1, x2 =
              if altitudeList.Length > 2 then
                  ((100.0/ (float) (altitudeList.Length-1)) * 1.0), ((100.0/ (float) (altitudeList.Length-1)) * 2.0)
              else  
                  0.0, 0.0
          x2-x1
      

      { model with svgPointsCoord = drawingAvailableCoord; svgSurfaceUnderLineCoord = surfaceUnderLineCoord; svgPointsErrorCoord = drawingMissingCoord; svgSurfaceUnderLineErrorCoord = drawingMissingSurfaceCoord; svgCircleSize = circleSize }
      



  let getNumberOfErrors (errorHitList : int list) (index : int) = 
      let mutable keepCounting = true
      let mutable i = index
      let mutable numErrors = 0

      while keepCounting && i < errorHitList.Length do
          if errorHitList.Item(i) = -1 then
                 numErrors <- numErrors + 1
                 i <- i + 1
             else 
                 keepCounting <- false
      numErrors
      
  let createInterpolatedSubList (min : float) (max : float) (amount : int) =
      let mutable subList = []
      for i = 1 to amount do
          subList <- subList @ [min + ((max-min)/float (amount + 1)) * float i]
      subList

  let correctSamplingErrors (altitudeList : float list) (errorHitList : int list) =
      let correctedList = 
          let mutable finalList = altitudeList
          let mutable i = 0
          while i < finalList.Length do
              if errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = 0 then
                  let newArray = finalList |> List.toArray                
                  let leftArray, rightArray = newArray |> Array.splitAt i
                  let leftList = leftArray |> Array.toList
                  let rightList = rightArray |> Array.toList
                  
                  let numMissingValues = getNumberOfErrors errorHitList i
                  let interpolatedSubList = createInterpolatedSubList (leftList.Item(leftList.Length-1)) rightList.Head numMissingValues
                  finalList <- (leftList @ interpolatedSubList @ rightList)
              i <- i + 1
          finalList   
                                    
      correctedList
  
  let sampleBetweenTwoPoints (model : Model) (firstPoint : V3d) (secondPoint : V3d) (pointList : V3d list) (altitudeList : float list) (errorHitList: int list) = 
      let initialListLength = pointList.Length

      let dir = secondPoint - firstPoint
      let samplingSize = ( Math.Round (firstPoint-secondPoint).Length) * model.stepSampleSize.value
      let step = dir / samplingSize  
    
      let sampleAndFillLists ((pointList : V3d list), (altitudeList : float list), (errorHitList: int list)) i =
          let fray = FastRay3d(V3d.Zero, (firstPoint + (step * float i)).Normalized)         
          model.picking.pickingInfos 
          |> HMap.tryFind model.opcBox
          |> Option.map (fun opcData -> 
              match OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some opcData.kdTree) fray with
              | Some t -> 
                  let hitpoint = fray.Ray.GetPointOnRay t
                  let pointHeight = CooTransformation.getLatLonAlt hitpoint Planet.Mars
    
                  (hitpoint :: pointList), (pointHeight.altitude :: altitudeList), (0 :: errorHitList)
                      
              | None -> 
                  (pointList.Head :: pointList), altitudeList, (-1 :: errorHitList)                 
              )
          |> Option.defaultValue (pointList, altitudeList, errorHitList)
                            
      let pL, aL, eL = List.fold sampleAndFillLists (pointList, altitudeList, errorHitList) [0.0..samplingSize]
      let numofElemBtw2Points = pL.Length - initialListLength

      pL, aL, eL, numofElemBtw2Points
     


  let sampleSurfacePointsForCutView (model : Model) (pickingModel : PickingModel) (drawingModel : DrawingModel) = 
    
      let numIntersectionPoints = pickingModel.intersectionPoints.Count
        
      let rec sampleBetweenMultiplePoints i pL aL eL numElemList=
          if (numIntersectionPoints-1) > i then 
              let pl, aL, eL, numElem = sampleBetweenTwoPoints model (pickingModel.intersectionPoints.Item(i)) (pickingModel.intersectionPoints.Item(i+1) ) pL aL eL 
              sampleBetweenMultiplePoints (i+1) pl aL eL (numElem :: numElemList)
          else 
            pL, aL, eL, numElemList


      let pointList, altitudeList, errorHitList, numofElemsBtw2PointsList = sampleBetweenMultiplePoints 0 [] [] [] []

      let numSampledPoints = altitudeList.Length

      let correctedAltitudeList = correctSamplingErrors altitudeList errorHitList  
      
      let rec linearDistance i acc = 
          if i >= 1 then
              let A = pickingModel.intersectionPoints.Item(i)
              let B = pickingModel.intersectionPoints.Item(i-1)
              let distance = (A-B).Length
              linearDistance (i-1) (acc+distance)
          else 
              acc

      let linDist = linearDistance (numIntersectionPoints-1) 0.0

      let rec accDistance i acc = 
          if i >= 1 then
              let A = pointList.Item(i)
              let B = pointList.Item(i-1)
              let distance = (A-B).Length
              accDistance (i-1) (acc+distance)
          else 
              acc
 
      
      //let rec hoverSpherePos i acc = 
      //    if i < numIntersectionPoints then
      //        hoverSpherePos (i+1) (acc+pickingModel.intersectionPoints.Item(i))
      //    else acc

      let camLoc = model.cameraState.view.Location

      let rec createTriangleStrips i arr = 
          if i >= 1 then
              let a = pickingModel.intersectionPoints.Item(i)
              let b = pickingModel.intersectionPoints.Item(i-1)

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
                          (V3d.Cross(bDir,b)).Normalized, (V3d.Cross(bDir,b)).Normalized
                      else 
                          (V3d.Cross(aDir,a)).Normalized, (V3d.Cross(aDir,a)).Normalized
                  else
                      (V3d.Cross(bDir,bV)).Normalized, (V3d.Cross(aV,aDir)).Normalized
                

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
            
      { model with 
              picking                    = pickingModel 
              drawing                    = drawingModel 
              numSampledPoints           = numSampledPoints 
              samplingDistance           = linDist / float (pointList.Length - 1)
              linearDistance             = Math.Round(linDist,2)
              minHeight                  = altitudeList.Min (altitudeList.Item(0))
              maxHeight                  = altitudeList.Max (altitudeList.Item(0))
              accDistance                = Math.Round(accDistance (pointList.Length-1) 0.0,2)
              pointList                  = pointList
              altitudeList               = correctedAltitudeList  
              errorHitList               = errorHitList
              numofPointsinLineList      = numofElemsBtw2PointsList
              hoverTriangles             = triangleStrips
            
      }
    
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
            let p = { model.picking with intersectionPoints = plist.Empty }             
            let clearedDrawingModel = { DrawingModel.initial with style = { DrawingModel.initial.style with thickness = 0.0; primary = { c = C4b(50,208,255) }; secondary = { c = C4b(132,226,255) } } } 
             
            { model with 
                    pickingActive = true; 
                    lineSelectionActive = true; 
                    picking = p; 
                    drawing = clearedDrawingModel; 
                    annotations = AnnotationModel.initial; 
                    numSampledPoints = 0; 
                    stepSampleSize = {min = 0.01; max = 10000.0; value = model.stepSampleSize.value; step = 0.05; format = "{0:0.00}"}; 
                    linearDistance = 0.0; 
                    minHeight = 0.0; 
                    maxHeight = 0.0; 
                    accDistance = 0.0; 
                    hoverTriangles = [|Triangle3d(V3d.Zero, V3d.Zero, V3d.Zero)|] 
                    pointList = []
                    altitudeList = []
                    errorHitList = []
                    numofPointsinLineList = []
                    hoveredCircleIndex = None
                    markerCone = { height = 0.0; radius = 0.0; color = C4b.Red; trafoRot = Trafo3d.Identity; trafoTrl = Trafo3d.Identity}

                } |> caluculateSVGDrawingPositions
          | _ -> model
      | Message.KeyUp m ->
        match m with
          | Keys.LeftAlt ->           
            if model.perspectiveView && model.jumpSelectionActive && not model.camJumpAnimRunning then 
                update { model with jumpSelectionActive = false; pickingActive = false } Message.AnimateCameraViewSwitch            
            else 
                { model with jumpSelectionActive = false; pickingActive = false }
          | Keys.LeftShift -> 
            { model with pickingActive = false; lineSelectionActive = false }
          | Keys.LeftCtrl -> 
            { model with hover3dActive = false; markerCone = { height =0.0; radius = 0.0; color = C4b.Red; trafoRot = Trafo3d.Identity; trafoTrl = Trafo3d.Identity} }
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
            if pos.Y > model.mouseDragStart.Y then
               let cam = 
                 CameraView(model.cameraState.view.Sky, model.cameraState.view.Location + model.cameraState.view.Forward*10.0, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

               let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

               { model with cameraState = newCamState }
            elif pos.Y < model.mouseDragStart.Y then
               let cam = 
                 CameraView(model.cameraState.view.Sky, model.cameraState.view.Location - model.cameraState.view.Forward*10.0, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

               let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

               { model with cameraState = newCamState }
            else 
                model
        else
            model
      | Message.Pan pos ->
         if model.persToOrthoValue = 1.0 && not model.hover3dActive then
             let direction = pos - model.mouseDragStart
             
             let camPos = model.cameraState.view.Location

             let r = camPos.Length
             let normDir = V2d((float)direction.X / direction.Length,  (float)direction.Y / direction.Length)
             let theta = Math.Acos (camPos.Z / r) + 0.000005 * normDir.Y
             let phi = Math.Atan2 (camPos.Y, camPos.X) + 0.000005 * normDir.X
             
             let newCord = V3d(r * Math.Sin(theta) * Math.Cos(phi), r * Math.Sin(theta) * Math.Sin(phi),  r * Math.Cos(theta)) 
      
             let cam =  
                 CameraView(model.cameraState.view.Sky, newCord, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)
             
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
        
        { model with cameraAnimEndTime = float total.Ticks; camJumpAnimRunning = true; targetPosition = targetPos; originalCamPos = model.cameraState.view.Location }
      | Message.AnimateCameraComplete ->
        let duration = TimeSpan.FromSeconds 0.4
        let total = DateTime.Now.AddTicks (duration.Ticks)
        let targetPos = model.selectedJumpPosition

        { model with cameraAnimEndTime = float total.Ticks; camCompAnimRunning = true; camViewAnimRunning = true; camJumpAnimRunning = false; targetPosition = targetPos; originalCamPos = model.cameraState.view.Location }
      | Message.AnimateCameraReturn ->
        let duration = TimeSpan.FromSeconds 2.0
        let total = DateTime.Now.AddTicks (duration.Ticks)

        { model with cameraAnimEndTime = float total.Ticks; camRetAnimRunning = true; camViewAnimRunning = false; camJumpAnimRunning = true; targetPosition = model.originalCamPos; originalCamPos = model.cameraState.view.Location }
      | Message.Tick t ->       
        if not model.camCompAnimRunning && not model.camRetAnimRunning && model.camViewAnimRunning then
            orthographicPerspectiveAnimation model t
        elif not model.camCompAnimRunning && not model.camRetAnimRunning && model.camJumpAnimRunning then
            cameraJumpAnimation model t
        elif model.camCompAnimRunning && model.camViewAnimRunning then
            orthographicPerspectiveAnimation model t
        elif model.camCompAnimRunning && model.camJumpAnimRunning then
            cameraJumpAnimation model t
        elif model.camRetAnimRunning && model.camJumpAnimRunning then
            cameraReturnJumpAnimation model t
        elif model.camRetAnimRunning && model.camViewAnimRunning then
            orthographicPerspectiveAnimation model t
        else 
            model
      
      | PickingAction msg ->    
        let pickingModel, drawingModel =
          match msg with
          | HitSurface (a,b) -> 
            let updatePickM = PickingApp.update model.picking (HitSurface (a,b))
            let lastPick = updatePickM.intersectionPoints |> PList.tryFirst         
            
            let updatedDrawM =
                match lastPick with
                | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                | None -> model.drawing

            updatePickM, updatedDrawM
          | _ -> PickingApp.update model.picking msg, model.drawing
        
        let newDrawingModel = { drawingModel with style = { drawingModel.style with thickness = 2.0; primary = { c = C4b(50,208,255) }; secondary = { c = C4b(132,226,255) } } } 
        
        if model.jumpSelectionActive && not model.camViewAnimRunning then
            update { model with selectedJumpPosition = pickingModel.intersectionPoints.Item(0); jumpSelectionActive = false; inJumpedPosition = true } Message.AnimateCameraJump            
        elif model.camViewAnimRunning then
            model
        elif model.lineSelectionActive && pickingModel.intersectionPoints.AsList.Length >= 2 then       
           caluculateSVGDrawingPositions <| sampleSurfacePointsForCutView model pickingModel newDrawingModel
        else
            { model with picking = pickingModel; drawing = newDrawingModel }

      | DrawingAction msg -> 
        { model with drawing = DrawingApp.update model.drawing msg }
      | AnnotationAction msg -> 
        { model with annotations = AnnotationApp.update model.annotations msg}   
      | SetProjection a ->
        if model.inJumpedPosition then
            model
        elif a = Some O && model.perspectiveView then
            update { model with currentOption = a } Message.AnimateCameraViewSwitch
        elif a = Some P && not model.perspectiveView then
            update { model with currentOption = a } Message.AnimateCameraViewSwitch
        else
            { model with currentOption = a }
      | SetSamplingRate f -> 
        if model.picking.intersectionPoints.AsList.Length >= 2 then 
            caluculateSVGDrawingPositions <| sampleSurfacePointsForCutView { model with stepSampleSize = Numeric.update model.stepSampleSize f } model.picking model.drawing
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
         
        caluculateSVGDrawingPositions { model with cutViewZoom = newCutViewZoom; cutViewDim = V2i(newXDim, model.cutViewDim.Y); offsetUIDrawX = newXOffset }
      | ResizeRenderView d ->
        { model with renderViewDim = d}
      | ResizeCutView d ->
        { model with cutViewDim = d}
      | HovereCircleEnter id -> 
        match model.hoveredCircleIndex with 
        | None -> 
            update { model with hoveredCircleIndex = Some id} Message.HighlightIn3DView
        | Some oldID when not (id = oldID) -> 
            update { (update model HovereCircleLeave) with hoveredCircleIndex = Some id} Message.HighlightIn3DView
        | _ -> model
      | HovereCircleLeave -> 
        if model.hoveredCircleIndex.IsSome then
            { model with hoveredCircleIndex = None; 
                         drawing2 = DrawingModel.initial;
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
                lineOrigin + a * (((V3d.Dot(b,a)/(a.Length*a.Length)) * a)/a.Length).Length

        let height =
            let h = (projectedPointOnLine - pointList.Item(currentHoveredIndex)).Length
            if projectedPointOnLine.Length >= pointList.Item(currentHoveredIndex).Length then
                h
            else 
                -h
                       
            
        { model with markerCone = { height = height;
                                    radius = 1.0; 
                                    color = C4b.Red; 
                                    trafoRot = Trafo3d.RotateInto(V3d.OOI, model.opcCenterPosition.Normalized ); 
                                    trafoTrl = Trafo3d.Translation(pointList.Item(currentHoveredIndex))
                                  }                 
        }
        
      | Hoverin3D pos ->
        if model.hover3dActive then
            let pointList = model.pointList 

            let fray = FastRay3d(V3d.Zero, pos.Normalized)   
            let hitpoint=
                model.picking.pickingInfos 
                |> HMap.tryFind model.opcBox
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

            update model (HovereCircleEnter minElementIndex)
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
                             (V3d.Cross(bDir,b)).Normalized, (V3d.Cross(bDir,b)).Normalized
                          else 
                             (V3d.Cross(aDir,a)).Normalized, (V3d.Cross(aDir,a)).Normalized
                      else
                          (V3d.Cross(bDir,bV)).Normalized, (V3d.Cross(aV,aDir)).Normalized
                  

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


  let view (m : MModel) =
      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg (Mod.constant None) m.pickingActive m.cameraState.view info)
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

            let o = (Frustum.projTrafo (Frustum.ortho (Box3d(V3d(-sizeX, -sizeY, 0.0),V3d(sizeX, sizeY, 2.0*distance)))))
            let p = (Frustum.projTrafo (Frustum.perspective 60.0 0.01 10000.0 aspect))
            
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

            let o = ((Frustum.ortho (Box3d(V3d(-sizeX, -sizeY, 0.0),V3d(sizeX, sizeY, 2.0*distance)))))
            let p = ((Frustum.perspective 60.0 0.01 10000.0 aspect))
            
            let! t = m.persToOrthoValue

            if t = 0.0 then
                return p
            else 
                return o

        }
      
      let near = m.mainFrustum |> Mod.map(fun x -> x.near)
      let far = m.mainFrustum |> Mod.map(fun x -> x.far)
      

      let filledPolygonSg, afterFilledPolygonRenderPass = 
        m.annotations 
        |> AnnotationApp.viewGrouped near far (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)

      let afterFilledPolygonSg = 
        [
          m.drawing |> DrawingApp.viewPointSize near far (Mod.constant 2.0)
        ] 
        |> Sg.ofList
        |> Sg.pass afterFilledPolygonRenderPass
        
      let afterFilledPolygonSg2 = 
        [
          m.drawing2 |> DrawingApp.viewPointSize near far (Mod.constant 10.0)
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
        Sg.cone 16  (m.markerCone |> Mod.map ( fun x -> x.color)) (m.markerCone |> Mod.map ( fun x -> x.radius)) (m.markerCone |> Mod.map ( fun x -> x.height))
            |> Sg.noEvents
            |> Sg.effect [
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.vertexColor
            ]
            |> Sg.trafo (m.markerCone |> Mod.map ( fun x -> x.trafoRot))
            |> Sg.trafo (m.markerCone |> Mod.map ( fun x -> x.trafoTrl))
           
      
      let hoverTriangle = Sg.empty 
                         |> Sg.pickable' (m.hoverTriangles |> Mod.map ( fun s -> { shape = PickShape.Triangles(KdTree(Spatial.triangle, s)); trafo = Trafo3d.Identity } ))
                         |> Sg.requirePicking
                         |> Sg.noEvents
                         |> Sg.withEvents[ 
                             Sg.onEnter (fun x -> Hoverin3D x) 
                             Sg.onLeave (fun _ -> HovereCircleLeave)   
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

      let renderControl (state : MModel) (f : Message -> 'msg)=
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
            { kind = Stylesheet; name = "CutView5.css"; url = "CutView5.css" }
         ]  

      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          
          require Html.semui ( 
              body[onResize (Message.ResizeRenderView >> id)][
                div [clazz "ui"; style "background: #1B1C1E"] [renderControl m id]
                onBoot "$(window).trigger('resize')" (
                        body [onResize (Message.ResizeRenderView >> id)] [
                        ]
                    )
              ]
          )
          
        | Some "controls" -> 
              body [style "width: 100%; height:100%; background: transparent; overflow-y:visible"] [
                  div[style "color:white; margin: 5px 15px 5px 5px"] [
                        Html.SemUi.accordion "Camera" "camera" true [     
                            div [ clazz "item" ] [ 
                                dropdown { placeholder = "Thingy"; allowEmpty = false } [ clazz "ui simple inverted selection dropdown" ] dropDownValues m.currentOption SetProjection
                            ]                                                                       
                        ]
                        br[]
                        br[]
                        Html.SemUi.accordion "Elevation Info" "map" true [     
                            Html.table [  
                                Html.row "Number of Points:" [Incremental.text (m.numSampledPoints |> Mod.map (fun f -> f.ToString())) ]
                                Html.row "Linear Distance:" [Incremental.text (m.linearDistance |> Mod.map (fun f -> Math.Round(f,2).ToString())) ]
                                Html.row "Accumulated Distance:" [Incremental.text (m.accDistance |> Mod.map (fun f -> f.ToString())) ]
                                Html.row "Sampling Rate:"  [Numeric.view m.stepSampleSize |> UI.map Message.SetSamplingRate]                             
                                Html.row "Sampling Distance:" [Incremental.text (m.samplingDistance |> Mod.map (fun f -> Math.Round(f,4).ToString())) ]
                                Html.row "Min Height:" [Incremental.text (m.minHeight |> Mod.map (fun f -> Math.Round(f,2).ToString())) ]
                                Html.row "Max Height:" [Incremental.text (m.maxHeight |> Mod.map (fun f -> Math.Round(f,2).ToString())) ]                                         
                            ]                       
                        ]
                  ]
              ]       
        | Some "cutview" ->
            require dependencies ( 

              let percent = "%"
              let px = "px"
              let strokeWidthMainRect = "0.1px"
              let strokeColor = "rgb(255,255,255)"
              //let strokeWidthContorLineEdge = "1.5px"
              let lineOpacity = "0.15"
              let polygonOpacity = "0.25"
              let polygonColor = "rgb( 132,226,255)"
              let heightRectOpacity = "0.15"

              let inline (=>) a b = Attributes.attribute a b
 
              let mainBoxRect =
                 Incremental.Svg.rect (
                    amap {                     
                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let sX = (sprintf "%f" (xOffset * xWidth)) + px
                        let sY = (sprintf "%f" (yOffset * yWidth)) + px

                        let wX = (sprintf "%f" ((1.0-xOffset*2.0) * xWidth)) + px
                        let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px

                        yield attribute "x" sX
                        yield attribute "y" sY 
                        yield attribute "rx" "1px"
                        yield attribute "ry" "1px" 
                        yield attribute "width" wX
                        yield attribute "height" wY
                        yield attribute "fill" "url(#grad2)"
                        yield attribute "opacity" "0.25"
                        yield attribute "stroke" strokeColor
                        yield attribute "stroke-width" strokeWidthMainRect
                        yield onMouseLeave (fun _ -> HovereCircleLeave)
                    } |> AttributeMap.ofAMap
                 )
    

              let contourLine (order : float) (strokeWidth : float) (opacity : string) =
                  Incremental.Svg.line ( 
                    amap {

                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY
                        
                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let heightRectH = (1.0-yOffset*2.0)/5.0

                        let sX = (sprintf "%f" ((xOffset) * xWidth-5.0)) + px
                        let sY = (sprintf "%f" ((yOffset + heightRectH * order) * yWidth)) + px

                        let wX = (sprintf "%f" ((1.0-xOffset) * xWidth+5.0)) + px

                        yield attribute "x1" sX
                        yield attribute "y1" sY  
                        yield attribute "x2" wX
                        yield attribute "y2" sY 
                        yield attribute "stroke" strokeColor
                        yield attribute "stroke-width" (sprintf "%f" strokeWidth)
                        yield attribute "stroke-opacity" opacity
                    } |> AttributeMap.ofAMap
                )

              let verticalLine (i : int) (numofPointsinLineList : int list) (coordArray : string[])=
                  Incremental.Svg.line ( 
                    amap {
                        let rec countElemOffset i acc = 
                            if i >= 1 then
                                countElemOffset (i-1) (acc + numofPointsinLineList.Item(i))
                            else 
                                acc + numofPointsinLineList.Item(0)
                        
                        let numElemOffset = countElemOffset i 0

                        let xy1 = coordArray.[numElemOffset-1].Split([|","|], StringSplitOptions.None)
                        let xy2 = coordArray.[numElemOffset].Split([|","|], StringSplitOptions.None)
                        
                        let avgX = ((xy1.[0] |> float) + (xy2.[0] |> float)) / 2.0

                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let yWidth = (float) dimensions.Y
                            
                        let sX = sprintf "%f" avgX
                        
                        let sY = (sprintf "%f" ((yOffset - 0.025) * yWidth)) + px

                        let wY = (sprintf "%f" ((1.0-yOffset+0.025) * yWidth)) + px

                        yield attribute "x1" sX
                        yield attribute "y1" sY
                        yield attribute "x2" sX
                        yield attribute "y2" wY
                        yield attribute "stroke" strokeColor
                        yield attribute "stroke-width" "1.0"
                        yield attribute "stroke-opacity" "0.6"
                    } |> AttributeMap.ofAMap
                 )

              let containerAttribs = 
                amap {
                    let! dimensions = m.cutViewDim

                    let widthValue = (sprintf "%i" (dimensions.X))

                    yield onWheelPrevent true (fun x -> id (Message.MouseWheel x))
                    yield clazz "mySvg"
                    yield style ("width: " + widthValue + "px; height: 100%; user-select: none;")
                } |> AttributeMap.ofAMap

              

              body [style "width: 100%; height: 100%; background: transparent; overflow-x: scroll "; onResize (Message.ResizeCutView >> id)] [
                    
                    Incremental.Svg.svg containerAttribs <|
                        alist {
                            
                            //Gradient Color
                            yield Svg.defs[][                
                                Svg.linearGradient [attribute "id" "grad2"; attribute "x1" "0%"; attribute "y1" "0%"; attribute "x2" "0%"; attribute "y2" "100%"]   [
                                    Svg.stop [attribute "offset" "0%"; attribute "style" "stop-color:rgb(255,255,255);stop-opacity:1.0" ]
                                    Svg.stop [attribute "offset" "100%"; attribute "style" "stop-color:rgb(16,16,16);stop-opacity:1.0"] 
                                ]
                            ]
                            
                            
                            //Box
                            yield mainBoxRect
                           

                            //contour line (very high)
                            yield contourLine 0.0 2.5 "1.0"     

                            //contour line (high)
                            yield contourLine 1.0 1.5 lineOpacity                       

                            //contour line (medium high)
                            yield contourLine 2.0 1.5 lineOpacity      

                            //contour line (medium low)
                            yield contourLine 3.0 1.5 lineOpacity      

                            //contour line (low)
                            yield contourLine 4.0 1.5 lineOpacity      

                            //contour line (very low)
                            yield contourLine 5.0 2.5 "1.0"      
                                                                         
                            
                            //chart curve 
                            yield Incremental.Svg.polyline ( 
                                amap {
                                    let! drawingCoord = m.svgPointsCoord

                                    yield attribute "points" drawingCoord
                                    yield attribute "fill" "none"
                                    yield attribute "opacity" "0.8"
                                    yield attribute "stroke" "rgb(50,208,255)"
                                    yield attribute "stroke-width" "2.0"
                                } |> AttributeMap.ofAMap
                            )

                            let! drawingErrorCoord = m.svgPointsErrorCoord                                  
                            let errorCoordArray = drawingErrorCoord.Split([|"  "|], StringSplitOptions.None)
                            for i in 0 .. errorCoordArray.Length - 1 do
                                //chart curve error
                                yield Incremental.Svg.polyline ( 
                                    amap {
                                        let drawingCoord = errorCoordArray.[i]

                                        yield attribute "points" drawingCoord
                                        yield attribute "fill" "none"
                                        yield attribute "opacity" "0.8"
                                        yield attribute "stroke" "rgb(255,71,50)"
                                        yield attribute "stroke-width" "2.0"
                                    } |> AttributeMap.ofAMap
                                )

                            //chart surface under curve
                            yield Incremental.Svg.polygon ( 
                                amap {
                                    let! drawingCoord = m.svgSurfaceUnderLineCoord

                                    yield attribute "points" drawingCoord
                                    yield attribute "fill" polygonColor
                                    yield attribute "opacity" polygonOpacity
                                    yield attribute "stroke-width" "0.0"
                                } |> AttributeMap.ofAMap
                            )

                            //chart surface under curve error
                            yield Incremental.Svg.polygon ( 
                                amap {
                                    let! drawingCoord = m.svgSurfaceUnderLineErrorCoord
                                   
                                    yield attribute "points" drawingCoord
                                    yield attribute "fill" "rgb(255,132,132)"
                                    yield attribute "opacity" polygonOpacity
                                    yield attribute "stroke-width" "0.0"
                                } |> AttributeMap.ofAMap
                            )    


                            

                            let! circleSize = m.svgCircleSize
                            let! dim = m.cutViewDim
                            
                            let r = sprintf "%f" (Math.Min(circleSize/4.0 * float dim.X/100.0, 6.8))                              
                            let stw = sprintf "%f" (Math.Min(circleSize/12.0 * float dim.X/100.0, 2.25) |> (fun x -> if x >= 1.0 then x else 0.0)) + "px"       

                            //draw correct circles
                            let! drawingCoord = m.svgPointsCoord                                  
                            let coordArray = drawingCoord.Split([|" "|], StringSplitOptions.None)                                
                            for i in 0 .. coordArray.Length - 2 do
                                let xy = coordArray.[i].Split([|","|], StringSplitOptions.None)
                                if xy.Length > 1 then
                                    let x = xy.[0]
                                    let y = xy.[1]
                                    yield Incremental.Svg.circle ([attribute "cx" x; attribute "cy" y; attribute "r" r; attribute "stroke" "black"; attribute "stroke-width" stw; attribute "fill" "rgb(50,208,255)"; onMouseEnter (fun _ -> HovereCircleEnter i) ] |> AttributeMap.ofList)      

                                    let boxX = x |> float
                                    
                                    let boxR = r |> float
                                    let! yOffset = m.offsetUIDrawY
                                    let! dimensions = m.cutViewDim
                                    let yWidth = (float) dimensions.Y

                                    let sY = (sprintf "%f" (yOffset * yWidth)) + px

                                    let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px

                                    yield Incremental.Svg.rect ([attribute "x" (sprintf "%f" (boxX-boxR)); attribute "y" sY; attribute "width" (sprintf "%f" (boxR * 2.0)); attribute "height" wY; attribute "stroke-width" "0"; attribute "fill" "rgba(50,208,255,0)"; clazz "hoverRect" ;onMouseEnter (fun _ -> HovereCircleEnter i);] |> AttributeMap.ofList)      
                                    
                            // draw separating vertical lines
                            let! numofPointsinLineList = m.numofPointsinLineList
                            for i in 0 .. numofPointsinLineList.Length-2 do
                                yield verticalLine i numofPointsinLineList coordArray

                            // draw error circles
                            let! drawingErrorCoord = m.svgPointsErrorCoord                                  
                            let coordArray = drawingErrorCoord.Split([|" "|], StringSplitOptions.None)
                            for i in 0 .. coordArray.Length - 2 do
                                let xy = coordArray.[i].Split([|","|], StringSplitOptions.None)
                                if xy.Length > 1 then
                                    let x = xy.[0]
                                    let y = xy.[1]
                                    yield Incremental.Svg.circle ([attribute "cx" x; attribute "cy" y; attribute "r" r; attribute "stroke" "black"; attribute "stroke-width" stw; attribute "fill" "rgb(255,71,50)" ] |> AttributeMap.ofList)                              

                            let! hoverCircle = m.hoveredCircleIndex
                            
                            if hoverCircle.IsSome then 
                                let! yOffset = m.offsetUIDrawY

                                let! dimensions = m.cutViewDim
                                let xWidth = (float) dimensions.X
                                let yWidth = (float) dimensions.Y

                                let! aL = m.altitudeList

                                let! samplingDistance = m.samplingDistance

                                let coordArray = drawingCoord.Split([|" "|], StringSplitOptions.None) 

                                let xy = coordArray.[hoverCircle.Value].Split([|","|], StringSplitOptions.None)
                                if xy.Length > 1 then
                                    let x = xy.[0]
                                    let y = xy.[1]
                                    
                                    let boxR = r |> float
                                    let sY = (sprintf "%f" (yOffset * yWidth)) + px
                                    let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px
                                    yield Incremental.Svg.rect ([attribute "x" (sprintf "%f" ((x |> float)-boxR)); attribute "y" sY; attribute "width" (sprintf "%f" (boxR * 2.0)); attribute "height" wY; attribute "stroke-width" "0"; attribute "fill" "rgba(237,55,66,0.9)";  ] |> AttributeMap.ofList)      
                                    
                                    yield Incremental.Svg.circle ([attribute "cx" x; attribute "cy" y; attribute "r" r; attribute "stroke" "black"; attribute "stroke-width" stw; attribute "fill" "rgb(225,225,225)"; onMouseLeave (fun _ -> HovereCircleLeave) ] |> AttributeMap.ofList)                              
                                    
                                    let textAnchor = 
                                        let textPosition = (x |> float) / xWidth
                                        if textPosition < 0.15 then
                                            "begin"
                                        elif textPosition < 0.85 then
                                            "middle"
                                        else
                                            "end"

                                    yield Svg.text ([ attribute "x" x; attribute "y" (sprintf "%f" (yOffset * yWidth - 10.0) + px); attribute "font-size" "16"; attribute "fill" "#ffffff"; clazz "UIText"; attribute "text-anchor" textAnchor]) ( "Altitude: " + sprintf "%.2f" (aL.Item(hoverCircle.Value)) + " m; Distance: " + sprintf "%.2f" (samplingDistance * float (hoverCircle.Value)) + " m") 
                        }

                    onBoot "$(window).trigger('resize')" (
                        body [onResize (Message.ResizeCutView >> id)] [
                        ]                      
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

      let axis = 
        axisFile |> Option.map(fun fileName -> OpcSelectionViewer.AxisFunctions.loadAxis fileName)
                 |> Option.defaultValue None

      let patchHierarchies =
        [ 
          for h in phDirs do
            yield PatchHierarchy.load OpcSelectionViewer.Serialization.binarySerializer.Pickle OpcSelectionViewer.Serialization.binarySerializer.UnPickle (h |> OpcPaths)
        ]    

      let box = 
        patchHierarchies 
          |> List.map(fun x -> x.tree |> QTree.getRoot) 
          |> List.map(fun x -> x.info.GlobalBoundingBox)
          |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid

      let opcInfos = 
        [
          for h in patchHierarchies do
            
            let rootTree = h.tree |> QTree.getRoot

            yield {
              patchHierarchy = h
              kdTree         = Aardvark.VRVis.Opc.KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath (KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ OpcSelectionViewer.Serialization.binarySerializer)
              localBB        = rootTree.info.LocalBoundingBox 
              globalBB       = rootTree.info.GlobalBoundingBox
              neighborMap    = HMap.empty
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HMap.ofList      
                      
      let up = V3d.OOI 
      let sky = box.Center.Normalized
      let r = Trafo3d.RotateInto(V3d.OOI, sky)
      let camPos = V3d(box.Center.X,box.Center.Y,box.Center.Z)+r.Forward.TransformPos(V3d(0.0,0.0,10.0*2.0*100.0))
      Log.line "box centereeeeeeeeee %A" box.Center
      let restoreCamState : CameraControllerState =
        if File.Exists ".\camstate" then          
          Log.line "[App] restoring camstate"
          let csLight : CameraStateLean = OpcSelectionViewer.Serialization.loadAs ".\camstate"
          { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
        else 
          { FreeFlyController.initial with view = CameraView.lookAt camPos V3d.Zero up; } 


      let camState = restoreCamState

      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> OpcSelectionViewer.Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

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
          dropDownOptions                 = HMap.ofList [P, "Perspective"; O, "Orthographic";]
          currentOption                   = Some O
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
          unpersist = Unpersist.instance<Model, MModel>
      }

      


  