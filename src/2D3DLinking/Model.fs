namespace LinkingView

open Linking

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Opc
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open PRo3D.Minerva
open Rabbyte.Annotation
open Rabbyte.Drawing

type Action =
  | Camera           of FreeFlyController.Message
  | KeyUp            of key : Keys
  | KeyDown          of key : Keys  
  | UpdateDockConfig of DockConfig    
  | PickingAction    of PickingAction
  | DrawingAction    of DrawingAction
  | AnnotationAction of AnnotationAction
  | LinkingAction    of LinkingAction
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
    points : plist<V3d>
    }

[<DomainType>]
type Model =
    {
        cameraState          : CameraControllerState     
        mainFrustum          : Frustum
        fillMode             : FillMode                                
        [<NonIncremental>]
        patchHierarchies     : list<PatchHierarchy>        
        boxes                : list<Box3d>        
        opcInfos             : hmap<Box3d, OpcData>
        threads              : ThreadPool<Action>
        dockConfig           : DockConfig
        pickingModel         : PickingModel
        annotations          : AnnotationModel
        drawing              : DrawingModel
        pickedPoint          : Option<V3d>
        planePoints          : Option<plist<V3d>>
        pickingActive        : bool
        linkingModel         : LinkingModel
        //minervaModel         : MinervaModel
    }

   

