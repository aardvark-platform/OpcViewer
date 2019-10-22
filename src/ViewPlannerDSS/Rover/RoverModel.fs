﻿namespace ViewPlanner.Rover

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI.Primitives


[<DomainType>]
type CameraInput = 
    {
    previous        : float
    current         : float
    delta           : float
    }


type CameraType =
| HighResCam   
| WACLR  


[<DomainType>]
type CamVariables = 
    {
    name            : CameraType
    camera          : CameraControllerState
    position        : V3d
    frustum         : Frustum
    samplingValues  : plist<V2d> 
    viewList        : plist<CameraView>

    }

[<DomainType>]
type Stereo = 
 {
    camL            : CamVariables
    camR            : CamVariables
    currIdx         : int
 }

[<DomainType>]
type HighRes = 
    {
    cam             : CamVariables
    currIdx         : int
    }

//type Cameras = 
//| HighResCam  of CamVariables * int
//| WACLR of left:CamVariables * right:CamVariables * int
   

 type Overlap =
 | Percent_20
 | Percent_30
 | Percent_40
 | Percent_50


 type initialRoverCoords = 
    {
    coordinates     : plist<V3d>
    }

  [<DomainType>]
  type Placement = 
    {
    id              : int
    position        : V3d
    target          : V3d
    }

  [<DomainType>]
  type OutputVars = 
    {
    numberOfSamples : int
    energyRequired  : float
    timeRequired    : float
    datasize        : int
    spatialRes      : float
    }

 [<DomainType>]
 type ViewPlan = 
    {
    id              : int
    instrument      : string
    placement       : Placement
    cameraVariables : plist<CamVariables>
    thetaPhiValues  : plist<V2d>
    projPoints      : plist<V3d>
    panOverlap      : float
    tiltOverlap     : float
    outputParams    : OutputVars
    }

 [<DomainType>]
 type Bin = 
    {
    counter         : int
    min             : float
    max             : float
    }

 

[<DomainType; ReferenceEquality>]
type RoverModel =
    {
    position              : V3d             
    target                : V3d            
    tilt                  : CameraInput
    pan                   : CameraInput
    HighResCam            : HighRes 
    WACLR                 : Stereo  
    camera                : CameraType
    up                    : V3d
    panOverlap            : float     
    tiltOverlap           : float
    currentCamType        : Option<CameraType>
    currentPanOverlap     : Option<Overlap>
    currentTiltOverlap    : Option<Overlap>
    cameraOptions         : hmap<CameraType, string>
    panOverlapOptions     : hmap<Overlap, string>
    tiltOverlapOptions    : hmap<Overlap, string>
    selectedRegion        : Option<plist<V3d>>
    projPoints            : plist<V3d>
    thetaPhiValues        : plist<V2d>

    positionsList         : plist<Placement> 
    selectedPosition      : Option<Placement>

    viewplans             : plist<ViewPlan>
    selectedViewPlan      : Option<ViewPlan>
    walkThroughIdx        : int

    //variables
    //! currently dummy values for test purposes
    energy                : float
    time                  : float        //how long should the rover be in its current place

    //requirements
    //how much energy and time is needed to perform a pan or tilt
    //! currently dummy values for test purposes
    energyForPanTilt      : float
    timeForPanTilt        : float

    //required for calculation of data size
    colorDepth            : int    //bit
    numberOfColorChannels : int    //RGB
    horzRes               : int
    vertRes               : int

    //sampling with dpi
    samplingWithDpi       : bool

    }

type RoverAction = 
| SwitchCamera of Option<CameraType>
| CalculateAngles
| RotateToPoint
| ChangePanOverlap of Option<Overlap>
| ChangeTiltOverlap of Option<Overlap>
| SetRoverPosAndTarget of int
| ShowViewPlan of int
| SampleAllCombinations
| ToggleDpiSampling



module RoverModel =
    
    //High resolution camera //FOV 5.0°
    let initCamera = {
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
        }

    //let initfrustum = Frustum.perspective 6.0 0.1 100.0 1.2
    let initfrustum = Frustum.perspective 5.0 0.1 10.0 1.0

    //Stereo camera //PanCam FOV 37.0°
    //currently at 10 for testing
    let camL = {
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
        }
    let camR = {
        FreeFlyController.initial with view = CameraView.lookAt (V3d.III * 6.0) V3d.OOO V3d.OOI
        }

    let frustumL = Frustum.perspective 23.0 0.1 100.0 1.3
    let frustumR = Frustum.perspective 23.0 0.1 100.0 1.3
    
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
            name = HighResCam
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
        camera = HighResCam
        walkThroughIdx = 0

        WACLR = 
         {      
            camL = 
                {
                name = WACLR
                camera = camL
                position = V3d.III
                frustum = frustumL
                samplingValues = PList.empty
                viewList = PList.empty
                }
                
            camR = 
                {
                name = WACLR
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
        currentCamType = Some HighResCam
        cameraOptions = HMap.ofList [WACLR, "WACLR Stereo"; HighResCam, "High Resolution Camera"]
        panOverlapOptions = HMap.ofList [Percent_20, "20%"; Percent_30, "30%"; Percent_40, "40%"; Percent_50, "50%"]
        tiltOverlapOptions = HMap.ofList [Percent_20, "20%"; Percent_30, "30%"; Percent_40, "40%"; Percent_50, "50%"]
        
        selectedRegion = None

        projPoints = PList.empty
        thetaPhiValues = PList.empty

        positionsList = PList.empty
        selectedPosition = None

        viewplans = PList.empty
        selectedViewPlan = None

        panOverlap = 20.0       //at least 20% according to https://mars.nasa.gov/msl/mission/instruments/mastcam/
        tiltOverlap = 20.0

        energy = 100.0 //percent
        time = 10.0 //hours

        energyForPanTilt = 0.2 //percent for 1°
        timeForPanTilt = 0.5 //0.5 sec for 1°

        colorDepth = 8    //bit
        numberOfColorChannels = 3 //RGB
        horzRes = 1024
        vertRes = 1024

        samplingWithDpi = false

        }

    let getViewProj (cam : IMod<CameraView>) (frustum:IMod<Frustum>) =
        
        adaptive {
            let! fr = frustum 
            let proj = (Frustum.projTrafo(fr))
            let! cam = cam
            let view = cam.ViewTrafo
            return (view * proj)
        } 