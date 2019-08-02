namespace Rabbyte.Drawing

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

    let helperFinishState model close = 
        let status = 
            match model.points |> PList.count, close with
                | 0, _ -> PrimitiveStatus.Empty
                | 1, _ -> PrimitiveStatus.Point
                | 2, _ -> PrimitiveStatus.Line
                | _, false -> PrimitiveStatus.PolyLine
                | _, true -> PrimitiveStatus.Polygon
        { model with status = status}

    let createSecondaryColor (c : C4b) : C4b = 
    
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
            { model with style = { model.style with thickness = Numeric.update model.style.thickness th }}
        | ChangeSamplingRate sr ->
            { model with style = { model.style with samplingRate = Numeric.update model.style.samplingRate sr }}
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
                    { model with points = model.points |> PList.prepend p; status = InProgress; past = Some model}
                | false ->
                    let startP = model.points |> PList.first
                    let endP = p
                    let segPoints = 
                        match hitF with
                        | Some f -> 
                            let vec  = (endP - startP)
                            let dir  = vec.Normalized
                            let l    = vec.Length
        
                            [ 0.0 .. model.style.samplingRate.value .. l] 
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

                    { model with points = model.points |> PList.prepend p; segments = model.segments |> PList.prepend segment; status = InProgress; past = Some model}
        | RemoveLastPoint ->
            match model.points |> PList.count with
            | 0 -> model
            | 1 -> { model with points = model.points |> PList.skip 1; status = PrimitiveStatus.Empty; past = Some model}
            | _ -> { model with points = model.points |> PList.skip 1; segments = model.segments |> PList.skip 1; past = Some model }
        | DrawingAction.Clear -> 
            { model with points = PList.empty; segments = PList.empty; status = PrimitiveStatus.Empty; past = Some model}
        | Finish -> 
            helperFinishState model false  // undo-able?
        | FinishClose -> 
            let m = helperFinishState model true
            match m.status with
                | Polygon -> 
                    let newM = update m (AddPoint ((m.points |> PList.last), None))   // TODO close polygon with same hitF as others
                    { newM with past = Some model }
                | _ -> m

    let view (model : MDrawingModel) =  
        let vertices = drawVertices model
        let edges = drawLines model
        let segments = drawSegment model
        
        Sg.group [vertices; edges; segments] |> Sg.noEvents