namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc

// move to ViewerMain
type VolumeGeneration = 
  | Plane
  | AxisMidPoint
  | AxisPoints
  | AxisPointsMidRing

  // Debug info....move to ViewerMain -> Axis...
[<DomainType>]
type AxisPointInfo = {
  pointsOnAxis : plist<V3d>
  midPoint     : V3d
}

type PickingAction = 
  | HitSurface of Box3d*SceneHit*(V3d -> V3d)
  | RemoveLastPoint
  | ClearPoints
  //| AddBrush of (plist<V3d> -> Option<AxisPointInfo>)
  //| AddTestBrushes of (plist<V3d> -> Option<AxisPointInfo>)
  //| ShowDebugVis
  //| ShowOutline
  //| ShowOutlineDetail
  //| UseGrouping
  //| SetAlpha of float
  //| SetExtrusionOffset of float
  //| SetVolumeGeneration of Option<VolumeGeneration>

type BoxNeighbors = {
  neighbors : List<Box3d> 
}

//[<DomainType>]
//type Brush =
//  {
//    points : plist<V3d>
//    segments : plist<Segment>
//    pointsOnAxis : Option<AxisPointInfo>
//    color  : C4b    
//  }

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
  intersectionPoints   : plist<V3d>                 // TODO...change to LastIntersectionPoint : Option<V3d> 
  //useGrouping          : bool                     // TODO move to ViewerMain
  //groupedBrushes       : hmap<C4b, plist<Brush>>  // maintained by ViewerMain
  //volumeGeneration     : Option<VolumeGeneration> // TODO move to ViewerMain
  //volumeGenerationOptions : hmap<VolumeGeneration, string> // TODO move to ViewerMain
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
    }