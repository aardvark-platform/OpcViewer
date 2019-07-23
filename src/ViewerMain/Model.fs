namespace OpcSelectionViewer

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Geometry
open Aardvark.SceneGraph.Opc
open Aardvark.Geometry
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcSelectionViewer.Picking
//open Aardvark.UI.Primitives.Mutable.CameraControllerStateModule

type Time = float

type Message =
  | Camera                  of FreeFlyController.Message
  | KeyUp                   of key : Keys
  | KeyDown                 of key : Keys  
  | UpdateDockConfig        of DockConfig    
  | PickingAction           of PickingAction
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
        pickingActive        : bool
        zoomFactor           : float
        dragStart            : V2i
        movePos              : V2i
        zoom                 : bool
        panVector            : V2i
        prevPanVec           : V2i
        pan                  : bool
        pers_to_ortho        : float
        camViewAnimRunning   : bool
        camJumpAnimRunning   : bool
        camCompAnimRunning   : bool
        camRetAnimRunning    : bool
        cameraAnimEndTime    : float
        targetPosition       : V3d
        perspectiveView      : bool
        originalCamPos       : V3d

    }

   