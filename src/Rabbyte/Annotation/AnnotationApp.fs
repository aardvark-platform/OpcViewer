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
        | AddAnnotation drawingModel -> 
            let annotation = AnnotationModel.convertDrawingToAnnotation drawingModel
            let updatedAnnotation = model.annotations |> PList.prepend annotation
            
            let updatedAnnotationsFilledPolygon = 
                match drawingModel.primitiveType with
                    | Polygon -> 
                        model.annotationsGrouped 
                            |> HMap.alter annotation.style.primary.c (fun x -> 
                                match x with 
                                    | Some y -> Some (y |> PList.prepend annotation)
                                    | None -> Some (PList.single annotation))    
                    | _ -> model.annotationsGrouped

            { model with annotations = updatedAnnotation; annotationsGrouped = updatedAnnotationsFilledPolygon }

    let viewOutline (model: MAnnotationModel) = 
        model |> AnnotationSg.drawAnnotationsOutline

    let viewGrouped (model : MAnnotationModel) = 
        [
            model |> AnnotationSg.drawAnnotationsFilledGrouped
            model |> AnnotationSg.drawAnnotationsOutline
        ] |> Sg.ofList

    let viewSeq (model : MAnnotationModel) =
        [
            model |> AnnotationSg.drawAnnotationsFilledSeq
            model |> AnnotationSg.drawAnnotationsOutline
        ] |> Sg.ofList

    let viewGUI (model : MAnnotationModel) =
        failwith "TODO"