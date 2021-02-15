namespace  ElevationProfileViewer

open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI.Primitives

module CameraAnimation =
    let orthographicPerspectiveAnimation (model : Model) (t: float) = 
        if model.cameraAnimEndTime > t then
            let durationTicks = TimeSpan.FromSeconds 0.4 
            let remaingTicks = model.cameraAnimEndTime - t        
            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            if model.perspectiveView then
                { model with persToOrthoValue = percent }     
            else 
               { model with persToOrthoValue = 1.0 - percent }     
        elif model.camViewAnimRunning && model.cameraAnimEndTime < t then
            if model.perspectiveView then
                { 
                    model with 
                        perspectiveView = false
                        persToOrthoValue = 1.0
                        camViewAnimRunning = false 
                        currentOption = Some O 
                }
            else
                let duration = TimeSpan.FromSeconds 2.0
                let total = DateTime.Now.AddTicks (duration.Ticks)
                { 
                    model with 
                        perspectiveView = true
                        persToOrthoValue = 0.0
                        camViewAnimRunning = false
                        camJumpAnimRunning = model.camCompAnimRunning
                        camRetAnimRunning = false
                        cameraAnimEndTime = float total.Ticks
                        currentOption = Some P 
                }
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

            let up = V3d.OOI 

            let originVec = V3d.Zero  - model.originalCamPos 

            let projectedPointOnLine = 
                let a = camToTarget
                let b = originVec

                let s = a.Length * Vec.Dot(b,a) / (b.Length * a.Length)
                model.originalCamPos + b*(s/b.Length)

            let jumpTargetOffset = (projectedPointOnLine - targetPos)/5.0
            
            let bezierCurve (t:float) (b0:V3d) (b1:V3d) (b2:V3d) = 
                (b0 - 2.0*b1 + b2) * Math.Pow(t,2.0) + (-2.0*b0 + 2.0*b1) * t + b0
            
            let jumpTarget = bezierCurve percent model.originalCamPos projectedPointOnLine (targetPos+jumpTargetOffset)

            let cam = 
                CameraView.lookAt 
                    (jumpTarget) 
                    (model.targetPosition * (percent ** (1.0 / 200.0))) 
                    ((model.cameraState.view.Location.Normalized) * percent + (1.0 - percent) * up)
        
            let newCamState : CameraControllerState =
                { model.cameraState with view = cam }

            { model with cameraState = newCamState }
     
        elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
            let cam = CameraView.lookAt (model.cameraState.view.Location) (model.targetPosition) (model.cameraState.view.Sky)
            let newCamState : CameraControllerState =
                { model.cameraState with view = cam }

            { model with camJumpAnimRunning = false; camCompAnimRunning = false; cameraState = newCamState }     
        else 
            model


    let cameraReturnJumpAnimation (model : Model) (t : float) = 
        if model.cameraAnimEndTime > t then 
            let durationTicks = TimeSpan.FromSeconds 2.0 
            let remaingTicks = model.cameraAnimEndTime - t
            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            let targetToCam = model.originalCamPos - model.targetPosition
            let offset = targetToCam/10.0
            let targetPos = model.targetPosition + offset
            let camToTarget = targetPos - model.originalCamPos

            let up = V3d.OOI 

            let originVec = V3d.Zero  - model.originalCamPos 

            let projectedPointOnLine = 
                let a = camToTarget
                let b = originVec

                let s = a.Length * Vec.Dot(b,a) / (b.Length * a.Length)
                model.originalCamPos + b*(s/b.Length)

            let jumpTargetOffset = (projectedPointOnLine - targetPos)/5.0

            let bezierCurve (t:float) (b0:V3d) (b1:V3d) (b2:V3d) = 
                (b0 - 2.0*b1 + b2) * Math.Pow(t,2.0) + (-2.0*b0 + 2.0*b1) * t + b0
            
            let jumpTarget = bezierCurve (1.0-percent) model.originalCamPos projectedPointOnLine (targetPos+jumpTargetOffset)

            let cam = CameraView.lookAt (jumpTarget) (model.targetPosition * ((1.0-percent) ** (1.0 / 200.0))) (up * percent + (1.0 - percent) * (model.cameraState.view.Location.Normalized));
        
            let newCamState : CameraControllerState =
                { model.cameraState with view = cam }

            { model with cameraState = newCamState }
     
        elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
            let duration = TimeSpan.FromSeconds 0.4
            let total = DateTime.Now.AddTicks (duration.Ticks)
            { model with camJumpAnimRunning = false; camViewAnimRunning = true; cameraAnimEndTime = float total.Ticks; inJumpedPosition = false }     
        else 
            model
  
