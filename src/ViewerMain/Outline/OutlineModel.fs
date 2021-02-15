namespace OpcOutlineTest

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.Base.Geometry
open Aardvark.SceneGraph.Opc
open Aardvark.Geometry
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open OpcViewer.Base
open OpcViewer.Base.Picking

open Adaptify

type OutlineMessage =
  | Camera           of FreeFlyController.Message
  | UpdateDockConfig of DockConfig    
  | SetLineThickness of Numeric.Action
  | UseOutlines     

[<ModelType>]
type OutlineModel =
    {
        cameraState          : CameraControllerState                       
        fillMode             : FillMode                                
        [<NonAdaptive>]
        patchHierarchies     : list<PatchHierarchy>        
        
        boxes                : list<Box3d>
        opcInfos             : HashMap<Box3d, OpcData>
        threads              : ThreadPool<OutlineMessage>
        dockConfig           : DockConfig
        lineThickness        : NumericInput
        useOutlines          : bool
    }
  
   