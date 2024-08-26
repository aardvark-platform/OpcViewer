namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open FSharp.Data.Adaptive
open OpcViewer.Base.KdTrees
open Aardvark.Data.Opc

open Adaptify

  // Debug info....move to ViewerMain -> Axis...
[<ModelType>]
type AxisPointInfo = {
  pointsOnAxis : IndexList<V3d>
  midPoint     : V3d
}

type PickingAction = 
  | HitSurface of Box3d*SceneHit //*(V3d -> V3d)
  | RemoveLastPoint
  | ClearPoints

type BoxNeighbors = {
  neighbors : List<Box3d> 
}

[<ModelType>]
type OpcData = {
  [<NonAdaptive>]
  patchHierarchy : PatchHierarchy
  kdTree         : HashMap<Box3d, Level0KdTree>
  neighborMap    : HashMap<Box3d, BoxNeighbors>

  localBB        : Box3d
  globalBB       : Box3d
}

[<ModelType>]
type PickingModel = {
  pickingInfos         : HashMap<Box3d, OpcData>
  hitPointsInfo        : HashMap<V3d, Box3d>
  intersectionPoints   : IndexList<V3d>                 // TODO...change to LastIntersectionPoint : Option<V3d> 
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HashMap.empty
      hitPointsInfo      = HashMap.empty
      intersectionPoints = IndexList.empty
    }