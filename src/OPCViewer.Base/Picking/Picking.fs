namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open OpcViewer.Base

module PickingApp =

    let update (model: PickingModel) (msg: PickingAction) =
        match msg with
        | HitSurface(box, sceneHit) -> Intersect.perform model sceneHit box
        | HitSurfaceWithTexCoords(box, sceneHit) -> Intersect.performTexCoords model sceneHit box
        | RemoveLastPoint ->
            let points, infos =
                match model.intersectionPoints.AsList with
                | [] -> [], HMap.empty
                | first :: rest -> rest, model.hitPointsInfo.Remove first
            { model with
                  intersectionPoints = points |> PList.ofList
                  hitPointsInfo = infos }
        | ClearPoints ->
            { model with
                  intersectionPoints = PList.empty
                  hitPointsInfo = HMap.empty }

    let view (model: MPickingModel) =
        // TODO...maybe add last click point as animation?
        failwith ""
