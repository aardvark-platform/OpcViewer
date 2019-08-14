﻿namespace  ElevationProfileViewer

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Opc
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open Rabbyte.Drawing
open Rabbyte.Annotation


type Time = float

type Message =
  | Camera                  of FreeFlyController.Message
  | KeyUp                   of key : Keys
  | KeyDown                 of key : Keys  
  | UpdateDockConfig        of DockConfig    
  | PickingAction           of PickingAction
  | DrawingAction           of DrawingAction
  | AnnotationAction        of AnnotationAction
  | Down                    of button : MouseButtons * pos : V2i
  | Up                      of button : MouseButtons
  | Zoom                    of V2i
  | Pan                     of V2i
  | SetT                    of float
  | AnimateCameraViewSwitch          
  | AnimateCameraJump 
  | AnimateCameraComplete
  | AnimateCameraReturn
  | Tick                    of Time



type CameraStateLean = 
  { 
     location : V3d
     forward  : V3d
     sky      : V3d
  }

  type Stationing = {
      sh : double
      sv : double
  }

  type OrientedPoint = {
      direction             : V3d
      offsetToMainAxisPoint : V3d
      position              : V3d
      stationing            : Stationing
  }

[<DomainType>]
type Axis = {
    positions       : list<V3d>
    selectionOnAxis : Option<V3d>
    pointList       : plist<OrientedPoint>
    length          : float
    rangeSv         : Range1d
}

[<DomainType>]
type Model =
    {
        cameraState          : CameraControllerState                       
        fillMode             : FillMode                                
        [<NonIncremental>]
        patchHierarchies     : list<PatchHierarchy>        
        axis                 : Option<Axis>
        boxes                : list<Box3d>        
        opcInfos             : hmap<Box3d, OpcData>
        threads              : ThreadPool<Message>
        dockConfig           : DockConfig
        picking              : PickingModel
        drawing              : DrawingModel
        annotations          : AnnotationModel
        pickingActive        : bool
        
        opcCenterPosition    : V3d
        jumpSelectionActive  : bool
        inJumpedPosition     : bool
        selectedJumpPosition : V3d

        targetPosition       : V3d
        originalCamPos       : V3d

        mouseDragStart       : V2i

        zoom                 : bool      
        pan                  : bool

        perspectiveView      : bool
        persToOrthoValue     : float

        camViewAnimRunning   : bool
        camJumpAnimRunning   : bool
        camCompAnimRunning   : bool
        camRetAnimRunning    : bool

        cameraAnimEndTime    : float


    }

   