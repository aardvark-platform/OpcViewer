namespace OpcSelectionViewer.Picking

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open OpcSelectionViewer

module PickingApp =

  let update (model : PickingModel) (msg : PickingAction) = 
    match msg with
    | HitSurface (box, sceneHit) -> 
      IntersectionController.intersect model "" sceneHit box
    | RemoveLastPoint ->
      let points, infos = 
        match model.intersectionPoints.AsList with
          | [] -> [], HMap.empty
          | first :: rest -> 
            rest, model.hitPointsInfo.Remove first.position
        
      { model with intersectionPoints = points |> PList.ofList; hitPointsInfo = infos }
    | ClearPoints -> 
      { model with intersectionPoints = PList.empty; hitPointsInfo = HMap.empty}
    //| _ -> model

  let toV3f (input:V3d) : V3f= input |> V3f

  let drawColoredPoints (points : alist<V3d>) =
    
    let head = 
      points 
        |> AList.toMod 
        |> Mod.map(fun x -> (PList.tryAt 0 x) |> Option.defaultValue V3d.Zero)
      
    let pointsF = 
      points 
        |> AList.toMod 
        |> Mod.map2(
          fun h points -> 
            points |> PList.map(fun (x:V3d) -> (x-h) |> toV3f) |> PList.toArray
            ) head
       

    Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.effect [
         toEffect DefaultSurfaces.stableTrafo
         toEffect (DefaultSurfaces.constantColor C4f.Red)
         Shader.PointSprite.Effect
      ]
      |> Sg.translate' head
      |> Sg.uniform "PointSize" (Mod.constant 10.0)

  let view (model : MPickingModel) =
    drawColoredPoints (model.intersectionPoints |> AList.map(fun hit -> hit.position))