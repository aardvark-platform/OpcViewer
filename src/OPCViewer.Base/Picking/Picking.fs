namespace OpcViewer.Base.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive

open OpcViewer.Base

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit) -> 
      Intersect.perform model sceneHit box
    | RemoveLastPoint ->
      let points, infos = 
        match model.intersectionPoints.AsList with
          | [] -> [], HashMap.empty
          | first :: rest -> 
            rest, model.hitPointsInfo.Remove first
      { model with intersectionPoints = points |> IndexList.ofList; hitPointsInfo = infos }
    | ClearPoints -> 
      { model with intersectionPoints = IndexList.empty; hitPointsInfo = HashMap.empty}

  let view (model : AdaptivePickingModel) =
    
    // TODO...maybe add last click point as animation?
    failwith ""