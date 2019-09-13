namespace  ElevationProfileViewer

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base.Picking
open Rabbyte.Drawing
open Rabbyte.Annotation
open OpenTK.Input
open System
open ProviderImplementation.ProvidedTypes



type Time = float

type Alternative =
    | P
    | O

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
  | SetProjection           of Option<Alternative>
  | SetSamplingRate         of Numeric.Action
  | MouseWheel              of V2d
  | ResizeRenderView        of V2i
  | ResizeCutView           of V2i
  | HovereCircleEnter       of int
  | HovereCircleLeave       
  | HighlightIn3DView            

  | EnterBox                of V3d




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

type Cone = {
    height : float
    radius : float
    color : C4b
    trafoRot : Trafo3d
    trafoTrl : Trafo3d
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
        cameraState                    : CameraControllerState 
        mainFrustum                    : Frustum
        fillMode                       : FillMode                                
        [<NonIncremental>]             
        patchHierarchies               : list<PatchHierarchy>        
        axis                           : Option<Axis>
        boxes                          : list<Box3d>        
        opcInfos                       : hmap<Box3d, OpcData>
        threads                        : ThreadPool<Message>
        dockConfig                     : DockConfig
        picking                        : PickingModel
        drawing                        : DrawingModel
        drawing2                       : DrawingModel
        annotations                    : AnnotationModel
        pickingActive                  : bool
                                       
        lineSelectionActive            : bool
        opcBox                         : Box3d
        numSampledPoints               : int
        stepSampleSize                 : NumericInput
        samplingDistance               : float
        linearDistance                 : float
        accDistance                    : float
        maxHeight                      : float
        minHeight                      : float
                                       
        opcCenterPosition              : V3d
        jumpSelectionActive            : bool
        inJumpedPosition               : bool
        selectedJumpPosition           : V3d
                                       
        targetPosition                 : V3d
        originalCamPos                 : V3d
                                       
        mouseDragStart                 : V2i
                                       
        zoom                           : bool      
        pan                            : bool
                                       
        perspectiveView                : bool
        persToOrthoValue               : float
        dropDownOptions                : hmap<Alternative, string>
        currentOption                  : Option<Alternative>
                                       
        camViewAnimRunning             : bool
        camJumpAnimRunning             : bool
        camCompAnimRunning             : bool
        camRetAnimRunning              : bool
                                       
        cameraAnimEndTime              : float
                                       
        offsetUIDrawX                  : float
        offsetUIDrawY                  : float
        pointList                      : V3d list   
        altitudeList                   : float list
        errorHitList                   : int list
                                       
        svgPointsCoord                 : string
        svgPointsErrorCoord            : string
        svgSurfaceUnderLineCoord       : string
        svgSurfaceUnderLineErrorCoord  : string
        svgCircleSize                  : float
                                       
        cutViewZoom                    : float
                                       
        renderViewDim                  : V2i
        cutViewDim                     : V2i

        hoveredCircleIndex             : Option<int>
        hover3dActive                  : bool
        hoverCylinder                  : Cylinder3d

        markerCone                     : Cone


    }

   