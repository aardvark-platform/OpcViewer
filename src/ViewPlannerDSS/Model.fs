namespace ViewPlanner

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Opc
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open ViewPlanner.Rover
open Rabbyte.Drawing
open Rabbyte.Annotation
open System.Xml.Linq

type SaveOption =
    | CameraState   
    | RoverState   
    | PlaneState


type Action =
  | Camera           of FreeFlyController.Message
  | KeyUp            of key : Keys
  | KeyDown          of key : Keys  
  | UpdateDockConfig of DockConfig    
  | PickingAction    of PickingAction
  | RoverAction      of RoverAction
  | SaveConfigs      of Option<SaveOption>




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

  type PlaneCoordinates =
    {
    points : plist<V3d>
    }

  
[<DomainType>]
type Model =
    {
        cameraState          : CameraControllerState 
        fillMode             : FillMode     
        
        [<NonIncremental>]
        patchHierarchies     : list<PatchHierarchy>        
        boxes                : list<Box3d>        
        opcInfos             : hmap<Box3d, OpcData>
        threads              : ThreadPool<Action>
        dockConfig           : DockConfig
        pickingModel         : PickingModel
        drawing              : DrawingModel
        annotations          : AnnotationModel
        pickedPoint          : Option<V3d>
        planePoints          : Option<plist<V3d>>
        pickingActive        : bool
        rover                : RoverModel
        region               : Option<plist<V3d>>
        roiBboxFull          : bool
        saveOptions          : hmap<SaveOption, string>
        currentSaveOption    : Option<SaveOption>

    }

   

