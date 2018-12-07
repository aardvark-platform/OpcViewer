namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Geometry
open OpcSelectionViewer
open OpcSelectionViewer.KdTrees
open Aardvark.SceneGraph.Opc

type PickingAction = 
  | HitSurface of Box3d*SceneHit    
  | RemoveLastPoint
  | ClearPoints

type NeighboringInfo = {
  globalBB3d  : Box3d
  globalBB2d  : Box3d
  neighbors   : List<Box3d> 
  texturePath : string
}
  
type IntersectionHit = {
  hitBB      : Box3d
  position   : V3d
  texCoords  : Option<V2f>
}

[<DomainType>]
type OpcData = {
  [<NonIncremental>]
  patchHierarchy : PatchHierarchy
  kdTree         : hmap<Box3d, Level0KdTree>

  localBB        : Box3d
  globalBB       : Box3d
  globalBB2d     : Box3d
}

[<DomainType>]
type PickingModel = {
  pickingInfos         : hmap<Box3d, OpcData>
  intersectionPoints   : plist<IntersectionHit>  
  neighborMap          : hmap<Box3d, NeighboringInfo>
}  

module PickingModel =

  let initial = 
    {
      pickingInfos       = HMap.empty
      intersectionPoints = PList.empty
      neighborMap        = HMap.empty
    }