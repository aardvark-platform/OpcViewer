namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc

type VolumeGeneration = 
  | Plane
  | AxisMidPoint
  | AxisPoints
  | AxisPointsMidRing

[<DomainType>]
type AxisPointInfo = {
  pointsOnAxis : plist<V3d>
  midPoint     : V3d
}

type Segment = {
  startPoint : V3d
  endPoint   : V3d
  points     : list<V3d>  
}

type PickingAction = 
  | HitSurface of Box3d*SceneHit*(V3d -> V3d)
  | RemoveLastPoint
  | ClearPoints
  | AddBrush of (plist<V3d> -> Option<AxisPointInfo>)
  | AddTestBrushes of (plist<V3d> -> Option<AxisPointInfo>)
  | ShowDebugVis
  | ShowOutline
  | ShowOutlineDetail
  | UseGrouping
  | SetAlpha of float
  | SetExtrusionOffset of float
  | SetVolumeGeneration of Option<VolumeGeneration>

type BoxNeighbors = {
  neighbors : List<Box3d> 
}

[<DomainType>]
type Brush =
  {
    points : plist<V3d>
    segments : plist<Segment>
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
  segments             : plist<Segment>
  brush                : plist<Brush>
  groupedBrushes       : hmap<C4b, plist<Brush>>
  debugShadowVolume    : bool
  useGrouping          : bool
  showOutline          : bool
  showDetailOutline    : bool
  alpha                : float
  extrusionOffset      : float
  volumeGeneration     : Option<VolumeGeneration>
  volumeGenerationOptions : hmap<VolumeGeneration, string>
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
      brush              = PList.empty
      groupedBrushes     = HMap.empty
      debugShadowVolume  = false
      useGrouping        = true
      showOutline        = true
      showDetailOutline  = false
      alpha              = 0.5
      extrusionOffset    = 1.0
      volumeGeneration   = Some VolumeGeneration.Plane
      volumeGenerationOptions = HMap.ofList [Plane, "Plane"; AxisMidPoint, "AxisMidPoint"; AxisPoints, "AxisPoints"; AxisPointsMidRing, "AxisPointsRing"]
      segments = PList.empty
    }