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

type CameraType =
    | Camera60   //fov = 60°
    | Camera30   //fov = 30°
    | Camera15   //fov = 15°
    | Stereo    //two cameras

//[<DomainType>]
//type CameraVariant =
//    {
//    cam : CameraControllerState
//    frustum : Frustum
//    }


[<DomainType; ReferenceEquality>]
type RoverModel =
    {

        position :  V3d             
        target   :  V3d            
        tilt     :  CameraInput
        pan      :  CameraInput
        camera   :  CameraControllerState
        up       :  V3d
        frustum  :  Frustum
        currentCamType : Option<CameraType>
        cameraOptions : hmap<CameraType, string>
        panTiltValues : Option<plist<V2d>>
        boxP1 : Option<V3d>
        boxP2 : Option<V3d>
        corners : Option<plist<V3d>>

    }

type RoverAction =
    | ChangePosition of V3d     //locate the rover at an intersection point
    | ChangePan of float
    | ChangeTilt of float
    | SwitchCamera of Option<CameraType>
    | MoveToRegion of plist<V3d>      //move view frustum to a region of interest


module RoverModel =
    
    let initCamera = {
   
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
         }

    let initfrustum = Frustum.perspective 30.0 0.1 10.0 1.0
    
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

        currentCamType = Some Camera30
        cameraOptions = HMap.ofList [Camera60, "Camera60"; Camera30, "Camera30"; Camera15, "Camera15"; Stereo, "Stereo"]
        panTiltValues = None
        boxP1 = None
        boxP2 = None
        corners = None

        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 