namespace OpcViewer.Base.Picking

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

type Interactions =
| NoAction           = 0
| DrawAnnotation     = 1
| PickCrackDetection = 2


type PickingAction =
| HitSurface of Box3d*SceneHit //*(V3d -> V3d)
| HitSurfaceWithTexCoords of Box3d*SceneHit
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
type HitInfo = {
    position  : V3d
    texCoords : V2d
    opcInfo   : OpcData
    kdTree    : LazyKdTree
}

[<DomainType>]
type PickingModel = {
    pickingInfos         : hmap<Box3d, OpcData>
    hitPointsInfo        : hmap<V3d, Box3d>
    intersectionPoints   : plist<V3d>                 // TODO...change to LastIntersectionPoint : Option<V3d>
    interaction          : Interactions
    lastHit              : HitInfo option
}

module PickingModel =

    let initial =
        {
            pickingInfos       = HMap.empty
            hitPointsInfo      = HMap.empty
            intersectionPoints = PList.empty
            interaction        = Interactions.PickCrackDetection
            lastHit            = None
        }