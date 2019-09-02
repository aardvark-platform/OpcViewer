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

[<DomainType>]
type CamVariables = 
    {
    camera : CameraControllerState
    position : V3d
    frustum : Frustum
    samplingValues : plist<V2d> //pairs of theta/phi values
    viewList: plist<CameraView>

    }

//type Cam =
//    {
//    camera1 : CamVariables
//    camera2 : Option<CamVariables>
   
//    }


//PanCam; both cameras with equal FOV
[<DomainType>]
type Stereo = 
 {
    //middleCam : CameraControllerState
    camL : CamVariables
    camR : CamVariables
    currIdx : int
 }

[<DomainType>]
type HighRes = 
    {
    cam: CamVariables
    currIdx : int
    }

type CameraType =
    | HighResCam   
    | WACLR     

 type Overlap =
    | Percent_20
    | Percent_30
    | Percent_40
    | Percent_50


type ProjectionSphere = 
    {
    radius : float
    position : V3d
    }

 type initialRoverCoords = 
    {
    coordinates : plist<V3d>
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
        HighResCam   :   HighRes  //single camera
        WACLR :          Stereo  //stereo
        camera:          CameraType
        up       :       V3d
        panOverlap  :    float     
        tiltOverlap :    float
        currentCamType : Option<CameraType>
        currentPanOverlap : Option<Overlap>
        currentTiltOverlap : Option<Overlap>
        cameraOptions :  hmap<CameraType, string>
        panOverlapOptions :  hmap<Overlap, string>
        tiltOverlapOptions :  hmap<Overlap, string>
        reg :            Option<plist<V3d>>
        projsphere :     ProjectionSphere
        projPoints :     plist<V3d>
        thetaPhiValues : plist<V2d>



    }

type RoverAction =
    | ChangePosition of V3d     
    | ChangePan of float
    | ChangeTilt of float
    | SwitchCamera of Option<CameraType>
    | MoveToRegion 
    | CalculateAngles
    | RotateToPoint
    | ChangePanOverlap of Option<Overlap>
    | ChangeTiltOverlap of Option<Overlap>



module RoverModel =
    
    //High resolution camera //FOV 5.0°
    let initCamera = {
        
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
         }

    let initfrustum = Frustum.perspective 5.0 0.1 20.0 1.0

    //Stereo camera //PanCam FOV 37.0°
    let camL = {
                    FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
                }
    let camR = {
                    FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 6.0) V3d.OOO V3d.OOI
                }
    let frustumL = Frustum.perspective 37.0 0.1 20.0 1.0
    let frustumR = Frustum.perspective 37.0 0.1 20.0 1.0
    
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

        HighResCam = 
         {
         
           cam = 
            {
            camera = 
             {
              FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
             }
            
            position = V3d.III
            frustum = initfrustum
            samplingValues = PList.empty
            viewList = PList.empty
            
            
            
            }

           currIdx = 0
        
           
         
         }

        up     = initCamera.view.Up
        camera = WACLR


        

        WACLR = 
         {      
                camL = 
                    {
                    camera = camL
                    position = V3d.III
                    frustum = frustumL
                    samplingValues = PList.empty
                    viewList = PList.empty
         
                    }
                
                camR = 
                
                    {
                    camera = camR
                    position = V3d.III
                    frustum = frustumR
                    samplingValues = PList.empty
                    viewList = PList.empty
                    
                    }

                currIdx = 0
                
   
         }

        currentPanOverlap = Some Percent_20
        currentTiltOverlap = Some Percent_20
        currentCamType = Some WACLR
        cameraOptions = HMap.ofList [WACLR, "WACLR Stereo"; HighResCam, "High Resolution Camera"]
        panOverlapOptions = HMap.ofList [Percent_20, "20%"; Percent_30, "30%"; Percent_40, "40%"; Percent_50, "50%"]
        tiltOverlapOptions = HMap.ofList [Percent_20, "20%"; Percent_30, "30%"; Percent_40, "40%"; Percent_50, "50%"]
        
        reg = None

        projsphere =
            {
            radius = 1.0
            position = V3d.III
            }

        projPoints = PList.empty
        thetaPhiValues = PList.empty
        panOverlap = 20.0       //at least 20% according to https://mars.nasa.gov/msl/mission/instruments/mastcam/
        tiltOverlap = 20.0
        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 