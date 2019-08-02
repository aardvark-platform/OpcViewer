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

    let drawSegment (m :MDrawingModel) = failwith ""

    let drawLines (m : MDrawingModel) = 
        let lines = convertLines false m.points

        let color = m.style |> Mod.map (fun x -> x.primary.c)
        let lineWidth = m.style |> Mod.map (fun x -> x.thickness.value)
        let trafo = Mod.constant (Trafo3d.Identity)
        lineISg color lineWidth trafo lines

    let view (model : MDrawingModel) =  
        let vertices = drawVertices model
        let edges = drawLines model
        
        Sg.group [vertices; edges] |> Sg.noEvents

    let updateFinish model close = 
        let status = 
            match model.points |> PList.count, close with
                | 0, _ -> PrimitiveStatus.Empty
                | 1, _ -> PrimitiveStatus.Point
                | 2, _ -> PrimitiveStatus.Line
                | _, false -> PrimitiveStatus.PolyLine
                | _, true -> PrimitiveStatus.Polygon
        { model with status = status }

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
        | ChangeLineStyle l ->
            { model with style = { model.style with lineStyle = l }}
        | ChangeAreaStyle a ->
            { model with style = { model.style with areaStyle = a }}
        | AddPoint p -> 
            match model.points |> PList.isEmpty with
                | true -> 
                    { model with points = model.points |> PList.prepend p; status = InProgress; past = Some model}
                | false ->
                    let s = {
                        startPoint = model.points |> PList.first
                        endPoint = p
                        points = PList.empty
                    }
                    { model with points = model.points |> PList.prepend p; segments = model.segments |> PList.prepend s; past = Some model}
        | RemoveLastPoint ->
            match model.points |> PList.count with
            | 0 -> model
            | 1 -> { model with points = model.points |> PList.skip 1; status = PrimitiveStatus.Empty; past = Some model}
            | _ -> { model with points = model.points |> PList.skip 1; segments = model.segments |> PList.skip 1; past = Some model }
        | DrawingAction.Clear -> 
            { model with points = PList.empty; segments = PList.empty; status = PrimitiveStatus.Empty; past = Some model}
        | Undo _ -> 
            match model.past with
                | None -> model
                | Some p -> { p with future = Some model }
        | Redo _ -> 
            match model.future with
                | None -> model
                | Some f -> f
        | Finish -> 
            updateFinish model false
        | FinishClose -> 
            let m = updateFinish model true
            match m.status with
                | Polygon -> update m (AddPoint (m.points |> PList.last))
                | _ -> m

        //| AddPointAdv (point, hitFunction, name) ->
        //          //let up    = smallConfig.up.Get(bigConfig)
        //          //let north = smallConfig.north.Get(bigConfig)
        //          //let planet = smallConfig.planet.Get(bigConfig)

        //          //let model, newSegment = addPoint up north planet hitFunction point view model name webSocket
                
        //          //match newSegment with
        //          //    | None         -> model
        //          //    | Some segment -> addNewSegment hitFunction model segment
        //          failwith ""

    //let app =
    //    {
    //        unpersist = Unpersist.instance
    //        threads = fun _ -> ThreadPool.Empty
    //        initial = DrawingModel.inital
    //        update = update
    //        view = view
    //    }