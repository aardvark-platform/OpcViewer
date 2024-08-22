namespace ViewPlanner

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.Data.Opc
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open ViewPlanner.Rover
open Rabbyte.Drawing
open Rabbyte.Annotation

open Adaptify

type Action =
  | Camera           of FreeFlyController.Message
  | KeyUp            of key : Keys
  | KeyDown          of key : Keys  
  | UpdateDockConfig of DockConfig    
  | PickingAction    of PickingAction
  | RoverAction      of RoverAction
  | PickPoint        of V3d

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
    points : IndexList<V3d>
    }

[<ModelType>]
type Model =
    {
        cameraState          : CameraControllerState     
        mainFrustum          : Frustum
        fillMode             : FillMode                                
        [<NonAdaptive>]
        patchHierarchies     : list<PatchHierarchy>        
        boxes                : list<Box3d>        
        opcInfos             : HashMap<Box3d, OpcData>
        threads              : ThreadPool<Action>
        dockConfig           : DockConfig
        pickingModel         : PickingModel
        drawing              : DrawingModel
        annotations          : AnnotationModel
        pickedPoint          : Option<V3d>
        planePoints          : Option<IndexList<V3d>>
        pickingActive        : bool
        rover                : RoverModel
    }