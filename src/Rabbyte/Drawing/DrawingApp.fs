﻿namespace Rabbyte.Drawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives

open DrawingModel
open DrawingSg
open OpcViewer.Base
open FShade.Primitives

module DrawingApp =

    let private finalPrimitiveType close model  = 
        let primitiveType = 
            match model.points |> PList.count, close with
                | 0, _ -> PrimitiveType.Empty
                | 1, _ -> PrimitiveType.Point
                | 2, _ -> PrimitiveType.Line
                | _, false -> PrimitiveType.PolyLine
                | _, true -> PrimitiveType.Polygon

        { model with primitiveType = primitiveType}

    let private syncPrimType model =
        finalPrimitiveType false model

    let private createSecondaryColor (c : C4b) : C4b = 
    
        let primary = c.ToC3f().ToHSVf()
           
        let v = 
          if (primary.V > 0.5f) then 
            primary.V - 0.25f * primary.V
          else 
            primary.V + 0.25f * primary.V
        
        let secondary = HSVf(primary.H, primary.S, v).ToC3f().ToC3b()
        let secondary = C4b(secondary, c.A)
                                   
        secondary

    let rec update (model : DrawingModel) (act : DrawingAction) =
        match act with
        | ChangeColorPrimary c1 -> 
            { model with style = { model.style with primary = ColorPicker.update model.style.primary c1 }}
        | ChangeColorSecondary c2 -> 
            { model with style = { model.style with secondary = ColorPicker.update model.style.secondary c2 }}
        | ChangeColorAuto c -> 
            let primary = ColorPicker.update model.style.primary c
            let secCol = (createSecondaryColor primary.c)
            let secondary = ColorPicker.update model.style.secondary (ColorPicker.Action.SetColor { c = secCol })
            { model with style = { model.style with primary = primary; secondary = secondary}}
        | ChangeThickness th ->
            { model with style = { model.style with thickness = th }}
        | ChangeSamplingRate sr ->
            { model with style = { model.style with samplingRate = sr }}
        | ChangeLineStyle l ->
            { model with style = { model.style with lineStyle = l }}
        | ChangeAreaStyle a ->
            { model with style = { model.style with areaStyle = a }}
        | Undo _ -> 
            match model.past with
                | None -> model
                | Some p -> { p with future = Some model }
        | Redo _ -> 
            match model.future with
                | None -> model
                | Some f -> f
        // Undo-Able Commands
        | RecalculateSegments hitF -> failwith "" // TODO?
        | AddPoint (p, hitF) -> 
            match model.points |> PList.isEmpty with
                | true -> 
                    syncPrimType { model with points = model.points |> PList.prepend p; past = Some model}
                | false ->
                    let startP = p
                    let endP = model.points |> PList.first 
                    let segPoints = 
                        match hitF with
                        | Some f -> 
                            let vec  = (endP - startP)
                            let dir  = vec.Normalized
                            let l    = vec.Length
        
                            [ 0.0 .. model.style.samplingRate .. l] 
                                |> List.map(fun x -> startP + x * dir)
                                |> List.map f
                                |> List.choose id
                                |> PList.ofList
                        | None -> PList.empty

                    let segment = {
                        startPoint = startP
                        endPoint = endP
                        points = segPoints
                    }
                    syncPrimType { model with points = model.points |> PList.prepend p; segments = model.segments |> PList.prepend segment; past = Some model}
        | RemoveLastPoint ->
            match model.points |> PList.count with
            | 0 -> model
            | 1 -> syncPrimType { model with points = model.points |> PList.skip 1; past = Some model}
            | _ -> syncPrimType { model with points = model.points |> PList.skip 1; segments = model.segments |> PList.skip 1; past = Some model }
        | DrawingAction.Clear -> 
            syncPrimType { model with points = PList.empty; segments = PList.empty; past = Some model}
        | Finish -> 
            let m = finalPrimitiveType false model 
            { m with past = Some model}
        | FinishClose hitF -> 
            //let m = finalPrimitiveType true model 
            match model.primitiveType with
                | PolyLine -> 
                    let newM = update model (AddPoint ((model.points |> PList.last), hitF))
                    let updateType = finalPrimitiveType true newM
                    // TODO -> FIX WINDING ORDER FOR GROUPED ANNOTATIONS!
                    { updateType with past = Some model }
                | _ -> { model with past = Some model }

    let view (model : MDrawingModel) =  
        let vertices = drawVertices model
        let edges = drawLines model
        let segments = drawSegment model
        
        Sg.group [vertices; edges; segments] |> Sg.noEvents

    let viewGui (model : MDrawingModel) = 
        
        let style' = "color: white; font-family:Consolas;"

        div [clazz "ui inverted segment"; style "width: 100%; height: 100%"] [
            div [ clazz "ui vertical inverted menu" ] [
                //div [ clazz "item" ] [ 
                //    checkbox [clazz "ui inverted toggle checkbox"] model.active ToggleActive "Is the thing active?"
                //]
                table [clazz "item"] [
                    tr[][
                        td[style style'][text "Type:"]
                        td[style style'][
                            Incremental.text (model.primitiveType |> Mod.map (fun x -> 
                                match x with
                                    | PrimitiveType.Empty -> "Empty"
                                    | PrimitiveType.Point -> "Point"
                                    | PrimitiveType.Line -> "Line"
                                    | PrimitiveType.PolyLine -> "PolyLine"
                                    | PrimitiveType.Polygon -> "Polygon"    
                                ))]
                    ]
                    tr[][
                        td[style style'][text "Thickness:"]
                        td[style style'][numeric { min = 1.0; max = 8.0; smallStep = 0.5; largeStep= 1.0 } [clazz "ui inverted input"] (model.style.thickness) ChangeThickness]
                        //td[style style'][slider { min = 1.0; max = 8.0; step = 1.0 } [clazz "ui inverted red slider"] (model.style.thickness) ChangeThickness]
                    ]
                    tr[][
                        td[style style'][text "SamplingRate:"]
                        td[style style'][numeric { min = 0.02; max = 10.0; smallStep = 0.02; largeStep= 0.2 } [clazz "ui inverted input"] (model.style.samplingRate) ChangeSamplingRate]
                        //td[style style'][slider { min = 0.02; max = 10.0; step = 0.2 } [clazz "ui inverted red slider"] (model.style.samplingRate) ChangeSamplingRate]
                    ]
                    tr[][
                        td[style style'][text "AreaStyle:"]
                        td[style style'][dropdown { placeholder = "AreaStyle"; allowEmpty = false } [ clazz "ui inverted selection dropdown" ] (model.areaStyleNames |> AMap.map (fun k v -> text v)) (model.style.areaStyle) ChangeAreaStyle]
                    ]
                    tr[][
                        td[style style'][text "LineStyle:"]
                        td[style style'][dropdown { placeholder = "LineStyle"; allowEmpty = false } [ clazz "ui inverted selection dropdown" ] (model.lineStyleNames |> AMap.map (fun k v -> text v)) (model.style.lineStyle) ChangeLineStyle]
                    ]
                    // TODO...add spectrum.css and spectrum.js via Aardvark.UI.Primitives assembly.. (harri hack)
                    //tr[][
                    //    td[style style'][text "PrimaryColor:"]
                    //    td[style style'][ColorPicker.view (model.style.primary) |> UI.map ChangeColorPrimary]
                    //]
                    //tr[][
                    //    td[style style'][text "SecondaryColor:"]
                    //    td[style style'][ColorPicker.view (model.style.secondary) |> UI.map ChangeColorSecondary]
                    //]
                ]
            ]
        ]