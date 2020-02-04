namespace OpcSelectionViewer

open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.Base.Geometry
open Aardvark.SceneGraph.Opc
open Aardvark.Geometry
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes
open Rabbyte.Drawing
open Rabbyte.Annotation

type Message =
  | Camera           of FreeFlyController.Message
  | KeyUp            of key : Keys
  | KeyDown          of key : Keys  
  | UpdateDockConfig of DockConfig    
  | PickingAction    of PickingAction
  | AttributeAction  of AttributeAction
  | DrawingAction    of DrawingAction
  | AnnotationAction of AnnotationAction

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

[<ModelType>]
type Axis = {
    positions       : list<V3d>
    selectionOnAxis : Option<V3d>
    pointList       : IndexList<OrientedPoint>
    length          : float
    rangeSv         : Range1d
}

[<ModelType>]
type Model =
    {
        cameraState          : CameraControllerState
        mainFrustum          : Frustum
        fillMode             : FillMode                                
        [<NonAdaptive>]
        patchHierarchies     : list<PatchHierarchy> 
        boundingBox          : Box3d
        axis                 : Option<Axis>
        boxes                : list<Box3d>        
        opcInfos             : HashMap<Box3d, OpcData>
        threads              : ThreadPool<Message>
        dockConfig           : DockConfig
        picking              : PickingModel
        pickingActive        : bool

        opcAttributes        : AttributeModel
        drawing              : DrawingModel
        annotations          : AnnotationModel
    }  