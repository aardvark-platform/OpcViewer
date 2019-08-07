namespace Rabbyte.Annotation

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives

open OpcViewer.Base
open FShade.Primitives

open AnnotationModel

module AnnotationApp =
    open Rabbyte.Drawing

    let update (model : AnnotationModel) (act : AnnotationAction) =
        match act with
        | AddDrawing brush -> 
            { model with finishedDrawings = (model.finishedDrawings |> PList.prepend brush)}
        | _ -> model

    let view (model : MAnnotationModel) = 
        model.finishedDrawings |> AList.map (fun x -> DrawingApp.view x) |> AList.toASet |> Sg.set