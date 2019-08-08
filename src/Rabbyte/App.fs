﻿module App

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Geometry

open SimpleDrawingModel
open Rabbyte.Drawing
open Rabbyte.Annotation

type Action =
    | CameraMessage of ArcBallController.Message
    | Move          of V3d
    | KeyDown       of key : Keys
    | KeyUp         of key : Keys
    | Exit  
    | UpdateDrawing of DrawingAction
    | UpdateAnnotation of AnnotationAction

let update (model : SimpleDrawingModel) (act : Action) =

    let drawingUpdate (model : SimpleDrawingModel) (act : DrawingAction) = 
       { model with drawing = DrawingApp.update model.drawing act }

    match act, model.drawingEnabled with
        | CameraMessage m, false -> 
            { model with camera = ArcBallController.update model.camera m }
        | KeyUp Keys.LeftCtrl, _ -> 
            { model with drawingEnabled = false; hoverPosition = None }
        | KeyDown k, _ -> 
            match k with 
            | Keys.LeftCtrl -> { model with drawingEnabled = true }
            | Keys.Z -> drawingUpdate model DrawingAction.Undo
            | Keys.Y -> drawingUpdate model DrawingAction.Redo
            | Keys.Back -> drawingUpdate model DrawingAction.RemoveLastPoint
            | Keys.Delete -> drawingUpdate model DrawingAction.Clear
            | Keys.F -> 
                let finished = drawingUpdate model (DrawingAction.FinishClose None) // TODO add dummy-hitF
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation finished.drawing)
                { finished with annotations = newAnnotation; drawing = DrawingModel.initial} // clear drawingApp
            | Keys.Enter -> 
                let finished = drawingUpdate model DrawingAction.Finish
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation finished.drawing)
                { finished with annotations = newAnnotation; drawing = DrawingModel.initial} // clear drawingApp
            | _ -> model
        | Move p, true -> { model with hoverPosition = Some (Trafo3d.Translation p) }
        | UpdateDrawing a, _ -> 
            match a with
            | DrawingAction.AddPoint _ -> if model.drawingEnabled then drawingUpdate model a else model
            | _ -> drawingUpdate model a
        | UpdateAnnotation a, _ ->
            { model with annotations = AnnotationApp.update model.annotations a }
        | Exit, _ -> { model with hoverPosition = None }
        | _ -> model
  
let testScene =  
    let box1 = PolyMeshPrimitives.Box(new Box3d(V3d(-2.0,-0.5,-2.0), V3d(2.0,0.5,2.0)), C4b(241,238,246))
    let box2 = PolyMeshPrimitives.Box(new Box3d(V3d(-2.1,-2.0,-0.4), V3d(2.2,2.0,0.6)), C4b(241,200,200)) 

    let p1 = KdIntersectionTree(box1).ToConcreteKdIntersectionTree()
    let p2 = KdIntersectionTree(box2).ToConcreteKdIntersectionTree()
    let kdTree = KdIntersectionTree(KdTreeSet([p1; p2])).ToConcreteKdIntersectionTree()

    let hitFunc p = 
        let ray = FastRay3d(p + V3d.OOI * 20.0, -V3d.OOI)
        [
            box1
            box2
        ]
            |> List.choose (fun v -> OpcViewer.Base.Picking.Intersect.single ray kdTree)
            |> List.sort 
            |> List.map (ray.Ray.GetPointOnRay)|> List.tryHead

    [
        box1.GetIndexedGeometry().Sg |> Sg.noEvents
        box2.GetIndexedGeometry().Sg |> Sg.noEvents
    ]
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
                Sg.onClick(fun p -> 
                    let hitF = Some hitFunc
                    UpdateDrawing (DrawingAction.AddPoint (p, hitF)))
                Sg.onLeave (fun _ -> Exit)
            ]    

let frustum =
    Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

let scene3D (model : MSimpleDrawingModel) =
                                 
    let cursorTrafo = 
        model.hoverPosition 
            |> Mod.map (Option.defaultValue(Trafo3d.Scale(V3d.Zero)))

    let cursorSg color size trafo =         
        Sg.sphere 5 (Mod.constant color) (Mod.constant size)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.noEvents
            |> Sg.trafo (trafo)

    let cursor = cursorSg C4b.Red 0.05 cursorTrafo
        
    let drawingApp = DrawingApp.view model.drawing
    let annotationApp = AnnotationApp.viewGrouped model.annotations
        
    [testScene; cursor; drawingApp; annotationApp]
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
                    DrawingApp.viewGui model.drawing |> UI.map UpdateDrawing
                ]
            ]
        ]
    )

let initial =
    {
        camera        = { ArcBallController.initial with view = CameraView.lookAt (6.0 * V3d.OIO) V3d.Zero V3d.OOI}
        hoverPosition = None
        drawingEnabled = false
        drawing = DrawingModel.initial
        annotations = AnnotationModel.initial
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