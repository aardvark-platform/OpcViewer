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

type ProjectionSphere = 
    {
    radius : float
    position : V3d
    }

//[<DomainType>]
//type CameraVariant =
//    {
//    cam : CameraControllerState
//    frustum : Frustum
//    }


[<DomainType; ReferenceEquality>]
type RoverModel =
    {

        position :       V3d             
        target   :       V3d            
        tilt     :       CameraInput
        pan      :       CameraInput
        camera   :       CameraControllerState
        up       :       V3d
        frustum  :       Frustum
        fov      :       float
        currentCamType : Option<CameraType>
        cameraOptions :  hmap<CameraType, string>
        reg :            Option<plist<V3d>>
        projsphere :     ProjectionSphere
        projPoints :     plist<V3d>
        thetaPhiValues : plist<V2d>
        samplingValues : plist<V2d>
        currIdx :        int
        viewList:        plist<CameraView>



    }

type RoverAction =
    | ChangePosition of V3d     
    | ChangePan of float
    | ChangeTilt of float
    | SwitchCamera of Option<CameraType>
    | MoveToRegion 
    | CalculateAngles
    | RotateToPoint


module RoverModel =
    
    let initCamera = {
   
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
         }

    let initfrustum = Frustum.perspective 30.0 0.1 20.0 1.0
    
    let initial = 
        {
        position = V3d.III
        target   = V3d.III

        pan = 
            {
                previous = 0.0
                current = 0.0
                delta = 0.0
            }
        
        tilt = 
            {
                previous = 0.0
                current = 0.0
                delta = 0.0
            }

        camera = initCamera
        up     = initCamera.view.Up
        frustum = initfrustum

        currentCamType = Some Camera30
        cameraOptions = HMap.ofList [Camera60, "Camera60"; Camera30, "Camera30"; Camera15, "Camera15"; Stereo, "Stereo"]
        
        reg = None

        projsphere =
            {
            radius = 1.0
            position = V3d.III
            }

        projPoints = PList.empty
        thetaPhiValues = PList.empty
        samplingValues = PList.empty
        currIdx = 0
        viewList = PList.empty
        fov = 30.0
        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 