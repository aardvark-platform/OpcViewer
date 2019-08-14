namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc

  // Debug info....move to ViewerMain -> Axis...
[<DomainType>]
type AxisPointInfo = {
  pointsOnAxis : plist<V3d>
  midPoint     : V3d
}

type PickingAction = 
  | HitSurface of Box3d*SceneHit //*(V3d -> V3d)
  | RemoveLastPoint
  | ClearPoints

type BoxNeighbors = {
  neighbors : List<Box3d> 
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
  intersectionPoints   : plist<V3d>                 // TODO...change to LastIntersectionPoint : Option<V3d> 
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HMap.empty
      hitPointsInfo      = HMap.empty
      intersectionPoints = PList.empty
    }