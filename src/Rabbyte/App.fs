module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives

open SimpleDrawingModel
open Rabbyte.Drawing

type Action =
    | CameraMessage    of ArcBallController.Message
    | Move     of V3d
    | KeyDown  of key : Keys
    | KeyUp    of key : Keys
    | Exit  
    | UpdateDrawing of DrawingAction

let rec update (model : SimpleDrawingModel) (act : Action) =
    match act, model.draw with
        | CameraMessage m, false -> 
            { model with camera = ArcBallController.update model.camera m }
        | KeyDown k, _ -> 
            match k with 
            | Keys.LeftCtrl -> { model with draw = true }
            | Keys.U -> update model (UpdateDrawing DrawingAction.Undo)
            | Keys.Z -> update model (UpdateDrawing DrawingAction.Redo)
            | Keys.Delete -> update model (UpdateDrawing DrawingAction.RemoveLastPoint)
            | Keys.Enter -> update model (UpdateDrawing DrawingAction.Finish)
            | Keys.C -> update model (UpdateDrawing DrawingAction.FinishClose)
            | _ -> model
        | KeyUp Keys.LeftCtrl, _ -> { model with draw = false; hoverPosition = None }
        | Move p, true -> { model with hoverPosition = Some (Trafo3d.Translation p) }
        | UpdateDrawing a, true -> 
            {model with drawing = DrawingApp.update model.drawing a}
        | Exit, _ -> { model with hoverPosition = None }
        | _ -> model
                     
let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }

let computeScale (view : IMod<CameraView>)(p:V3d)(size:float) =        
    view 
        |> Mod.map (fun v -> 
            let distV = p - v.Location
            let distF = V3d.Dot(v.Forward, distV)
            distF * size / 800.0
        )

let mkISg color size trafo =         
    Sg.sphere 5 color size 
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.noEvents
        |> Sg.trafo(trafo) 
        
let canvas =  
    let box1 = new Box3d(V3d(-2.0,-0.5,-2.0), V3d(2.0,0.5,2.0)) 
    let box2 = new Box3d(V3d(-2.1,-2.0,-0.4), V3d(2.2,2.0,0.6))

    let b1 = Sg.box (Mod.constant(C4b(241,238,246))) (Mod.constant box1)
    let b2 = Sg.box (Mod.constant(C4b(241,200,200))) (Mod.constant box2)
    
    [b1; b2] 
        |> Sg.ofList  
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

        |> Sg.requirePicking
        |> Sg.noEvents 
            |> Sg.withEvents [
                Sg.onMouseMove (fun p -> Move p)
                Sg.onClick(fun p -> UpdateDrawing (DrawingAction.AddPoint (p, None)))   // TODO hitFunction
                Sg.onLeave (fun _ -> Exit)
            ]    

let frustum =
    Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

let scene3D (model : MSimpleDrawingModel) =
    let cam =
        model.camera.view 
                                           
    let trafo = 
        model.hoverPosition 
            |> Mod.map (function o -> match o with 
                                        | Some t-> t
                                        | None -> Trafo3d.Scale(V3d.Zero))

    let brush = mkISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
        
    let drawingApp = DrawingApp.view model.drawing
        
    [canvas; brush; drawingApp]
        |> Sg.ofList
        |> Sg.fillMode (Mod.constant(FillMode.Fill))
        |> Sg.cullMode (Mod.constant(CullMode.None))

let view (model : MSimpleDrawingModel) =            
    require (Html.semui) (
        div [clazz "ui"; style "background: #1B1C1E"] [
            ArcBallController.controlledControl model.camera CameraMessage frustum
                (AttributeMap.ofList [
                        onKeyDown KeyDown; onKeyUp KeyUp; attribute "style" "width:65%; height: 100%; float: left;"; attribute "data-samples" "8"
                    ]
                )
                (scene3D model)

            div [style "width:35%; height: 100%; float:right; background: #1B1C1E;"] [
                    Html.SemUi.accordion "Rendering" "configure" true [
                        p[][Incremental.text (model.drawing.status |> Mod.map (fun x -> 
                            let status = 
                                match x with
                                | PrimitiveStatus.InProgress -> "InProgress"
                                | PrimitiveStatus.Empty -> "Empty"
                                | PrimitiveStatus.Point -> "Point"
                                | PrimitiveStatus.Line -> "Line"
                                | PrimitiveStatus.PolyLine -> "PolyLine"
                                | PrimitiveStatus.Polygon -> "Polygon"
                            sprintf "Status: %s" status    
                            ))]
                    ]
                ]
            ]
    )

let initial =
    {
        camera        = { ArcBallController.initial with view = CameraView.lookAt (6.0 * V3d.OIO) V3d.Zero V3d.OOI}
        hoverPosition = None
        draw = false
        drawing = DrawingModel.inital
    }

let app =
    {
        unpersist = Unpersist.instance
        threads = fun model -> ArcBallController.threads model.camera |> ThreadPool.map CameraMessage
        initial = initial
        update = update
        view = view
    }

let start () = App.start app
