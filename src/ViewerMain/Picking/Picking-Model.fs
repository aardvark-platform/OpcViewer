namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc

[<DomainType>]
type AxisPointInfo = {
  pointsOnAxis : plist<V3d>
  midPoint     : V3d
}

type PickingAction = 
  | HitSurface of Box3d*SceneHit    
  | RemoveLastPoint
  | ClearPoints
  | AddBrush of Option<AxisPointInfo>
  | ShowDebugVis
  | UseAxisGeneration
  | UseSinglePointAxisGeneration
  | SetAlpha of float
  | SetExtrusionOffset of float

type BoxNeighbors = {
  neighbors : List<Box3d> 
}

[<DomainType>]
type Brush =
  {
    points : plist<V3d>
    pointsOnAxis : Option<AxisPointInfo>
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
  brush                : plist<Brush>
  debugShadowVolume    : bool
  useAxisForShadowV    : bool
  useSinglePointForShadowV : bool
  alpha                : float
  extrusionOffset      : float
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
      brush              = PList.empty
      debugShadowVolume  = false
      alpha              = 0.5
      useAxisForShadowV  = false
      useSinglePointForShadowV = true
      extrusionOffset    = 1.0
    }