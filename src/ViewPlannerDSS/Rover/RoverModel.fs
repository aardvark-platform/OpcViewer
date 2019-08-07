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
        cornerLBF : Option<V3d> //left bottom front
        cornerLTF : Option<V3d>
        cornerRBF : Option<V3d>
        cornerRTF : Option<V3d>
        cornerLBB : Option<V3d> //left bottom back
        cornerLTB : Option<V3d>
        cornerRBB : Option<V3d>
        cornerRTB : Option<V3d>
        corners : Option<plist<V3d>> //in world space
        reg :   Option<plist<V3d>>

        projsphere : ProjectionSphere
        projPoint1 : V3d
        projPoint2 : V3d

    }

type RoverAction =
    | ChangePosition of V3d     //locate the rover at an intersection point
    | ChangePan of float
    | ChangeTilt of float
    | SwitchCamera of Option<CameraType>
    | MoveToRegion //of plist<V3d>      //move view frustum to a region of interest


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
        panTiltValues = None
        cornerLBF = None
        cornerLTF = None
        cornerRBF = None
        cornerRTF = None
        cornerLBB = None
        cornerLTB = None
        cornerRBB = None
        cornerRTB = None
        corners = None
        reg = None

        projsphere =
            {
            radius = 1.0
            position = V3d.III
            }

        projPoint1 = V3d.OOO
        projPoint2 = V3d.OOO
        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 