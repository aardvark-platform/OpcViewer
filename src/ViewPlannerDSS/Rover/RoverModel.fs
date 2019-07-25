namespace ViewPlanner.Rover

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives

[<DomainType>]
type CameraInput = 
    {
    previous : float
    current : float
    delta : float
    }

[<DomainType; ReferenceEquality>]
type RoverModel =
    {

        position :  V3d             
        target   :  V3d            
        tilt     :  CameraInput
        pan      :  CameraInput
        camera   :  CameraControllerState//CameraView
        up       :  V3d
        frustum  :  Frustum

    }

type RoverAction =
    | ChangePosition of V3d     //locate the rover at an intersection point
    | ChangePan of float
    | ChangeTilt of float


module RoverModel =
    
    let initCamera = {
   
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
         }

    let initfrustum = Frustum.perspective 35.0 0.1 10.0 1.0
    
    let initial = 
        {
        position = V3d.III
        target   = V3d.III

        pan = 
            {
                previous = 90.0
                current = 90.0
                delta = 0.0
            }
        
        tilt = 
            {
                previous = 90.0
                current = 90.0
                delta = 0.0
            }

        camera = initCamera
        up     = initCamera.view.Up
        frustum = initfrustum
        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 