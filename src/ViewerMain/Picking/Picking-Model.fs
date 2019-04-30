namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc

type PickingAction = 
  | HitSurface of Box3d*SceneHit    
  | RemoveLastPoint
  | ClearPoints
  | AddBrush
  | ShowDebugVis
  | SetAlpha of Numeric.Action

type BoxNeighbors = {
  neighbors : List<Box3d> 
}

//[<DomainType>]
type Brush =
  {
    points : list<V3d>
    color  : C4b    
  }

[<DomainType>]
type OpcData = {
  [<NonIncremental>]
  patchHierarchy : PatchHierarchy
  kdTree         : hmap<Box3d, Level0KdTree>
  neighborMap    : hmap<Box3d, BoxNeighbors>

  localBB        : Box3d
  globalBB       : Box3d
}

[<DomainType>]
type PickingModel = {
  pickingInfos         : hmap<Box3d, OpcData>
  hitPointsInfo        : hmap<V3d, Box3d>
  intersectionPoints   : plist<V3d>  
  brush                : list<Brush>
  debugShadowVolume    : bool
  alpha                : NumericInput
}  

module PickingModel =

  let alpha =
    {
     min = 0.0
     max = 1.0
     value = 0.6
     step = 0.05
     format = "{0:0.00}"
    }

  let initial = 
    {
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
      brush              = List.empty
      debugShadowVolume  = false
      alpha              = alpha
    }