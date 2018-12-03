namespace OpcOutlineTest

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

type OutlineMessage =
  | Camera           of FreeFlyController.Message
  | UpdateDockConfig of DockConfig    
  | SetLineThickness of Numeric.Action
  | UseOutlines     

[<DomainType>]
type OutlineModel =
    {
        cameraState          : CameraControllerState                       
        fillMode             : FillMode                                
        [<NonIncremental>]
        patchHierarchies     : list<PatchHierarchy>        
        
        boxes                : list<Box3d>
        opcInfos             : hmap<Box3d, OpcData>
        threads              : ThreadPool<OutlineMessage>
        dockConfig           : DockConfig
        lineThickness        : NumericInput
        useOutlines          : bool
    }
  
   