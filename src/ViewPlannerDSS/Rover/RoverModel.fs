namespace ViewPlanner.Rover

open Aardvark.Base
open FSharp.Data.Adaptive

open Adaptify

[<ModelType>]
type CameraInput = 
    {
    previous : float
    current : float
    delta : float
    }

[<ModelType>]
type RoverModel =
    {

        position :  V3d             //where the rover is located
        target   :  V3d             //target the rover is looking at
        tilt     :  CameraInput
        pan      :  CameraInput
        camera   :  CameraView
        up       :  V3d
        frustum  :  Frustum

    }

type RoverAction =
    | ChangePosition of V3d     //locate the rover at an intersection point
    | ChangePan of float
    | ChangeTilt of float


module RoverModel =
    
    let initCamera = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI

    let initfrustum = Frustum.perspective 35.0 0.1 1000.0 1.0
    
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
        up     = initCamera.Up
        frustum = initfrustum
        }

    let getViewProj (view : aval<CameraView>) (frustum:aval<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! view = view
            let view = view.ViewTrafo
            return (view * proj)
        } 