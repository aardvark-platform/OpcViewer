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

    let update (model: AnnotationModel) (act: AnnotationAction) =
        match act with
        | ChangeExtrusionOffset offset -> { model with extrusionOffset = offset }
        | ShowDebugVis -> { model with showDebug = not model.showDebug }
        | AddAnnotation (drawingModel, clippinVolumType) -> 
            let annotation = AnnotationModel.convertDrawingToAnnotation drawingModel clippinVolumType
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

    let drawOutline (model: MAnnotationModel) = 
        model.annotations 
        |> AList.map (fun x -> 
            let pointsSg = x.points |> SgUtilities.drawPointList  x.style.primary.c (Mod.constant 10.0) (Mod.constant 0.5)
            let linesSg = x.points |> SgUtilities.lines' (Mod.constant 0.5) x.style.secondary.c x.style.thickness 

            [ pointsSg; linesSg ] |> Sg.group |> Sg.noEvents)
        |> AList.toASet
        |> Sg.set

    let viewOutline (model: MAnnotationModel) = 
        model |> drawOutline

    let viewGrouped (model: MAnnotationModel) = 
        [
            model |> AnnotationSg.drawAnnotationsFilledGrouped
            model |> drawOutline
        ] |> Sg.ofList

    let viewSeq (model: MAnnotationModel) =
        [
            model |> AnnotationSg.drawAnnotationsFilledSeq
            model |> drawOutline
        ] |> Sg.ofList

    let viewGui (model: MAnnotationModel) =
        let style' = "color: white; font-family:Consolas;"

        table [clazz "item"] [
            tr [] [ 
                checkbox [clazz "ui inverted toggle checkbox"] model.showDebug ShowDebugVis "Show Debug Vis:"
            ]
                    
            tr[][
                td[style style'][text "Offset:"]
                td[style style'][numeric { min = 0.1; max = 20.0; smallStep = 0.1; largeStep= 1.0 } [clazz "ui inverted input"] model.extrusionOffset ChangeExtrusionOffset]
            ]
        ]