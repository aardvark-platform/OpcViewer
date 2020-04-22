namespace OpcSelectionViewer

open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open FShade
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes
open Rabbyte.Drawing
open Rabbyte.Annotation

open Aardvark.Application
open Aardvark.VRVis.Opc
open CrackDetection

module App =

    let updateFreeFlyConfig (incr : float) (cam : CameraControllerState) =
        let s' = cam.freeFlyConfig.moveSensitivity + incr
        Log.line "[App] sensitivity: %A" s'
        let config =
            {
                cam.freeFlyConfig with
                    panMouseSensitivity       = exp(s') * 0.0025
                    dollyMouseSensitivity     = exp(s') * 0.0025
                    zoomMouseWheelSensitivity = exp(s') * 0.1
                    moveSensitivity           = s'
            }

        { cam with freeFlyConfig = config }

    let toCameraStateLean (view : CameraView) : CameraStateLean =
        {
            location = view.Location
            forward  = view.Forward
            sky      = view.Sky
        }

    let fromCameraStateLean (c : CameraStateLean) : CameraView =
        CameraView.lookAt c.location (c.location + c.forward) c.sky

    let rec update (model : Model) (msg : Message) =
        match msg with
        | Camera m when model.pickingActive = false ->
          { model with cameraState = FreeFlyController.update model.cameraState m; }
        | Message.KeyDown m ->
            match m with
            | Keys.LeftCtrl ->
                { model with pickingActive = true }
            | Keys.F1 ->
                Log.line ""
                { model with picking = { model.picking with interaction = Interactions.DrawAnnotation }}
            | Keys.F2 ->
                Log.line ""
                { model with picking = { model.picking with interaction = Interactions.PickCrackDetection }}
            | _ ->
                model
        | Message.KeyUp m ->
            match m with
            | Keys.LeftCtrl ->
                { model with pickingActive = false }
            | Keys.Delete ->
                let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.Clear)
                { model with drawing = updatedDrawing }
            | Keys.Back ->
                let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.RemoveLastPoint)
                { model with drawing = updatedDrawing }
            | Keys.PageUp ->
                { model with cameraState = model.cameraState |>  updateFreeFlyConfig +0.5 }
            | Keys.PageDown ->
                { model with cameraState = model.cameraState |>  updateFreeFlyConfig -0.5 }
            | Keys.Space ->
                Log.line "[App] saving camstate, saving crack detection state"
                model.cameraState.view |> toCameraStateLean |> Serialization.save ".\camstate" |> ignore

                //model.crackDetection |> Serialization.save ".\crackdetectionState" |> ignore
                model
            | Keys.Escape ->
                { model with crackDetection = CrackDetectionApp.initModel }
            | Keys.Enter ->
              //let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
              //let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) // TODO...add hitFunc
              //let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
              //{ model with axis = axis; drawing = updatedDrawing }

                match model.picking.interaction with
                | Interactions.DrawAnnotation ->
                    let finished = { model with drawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) } // TODO add dummy-hitF
                    let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, None))
                    { finished with annotations = newAnnotation; drawing = DrawingModel.reset model.drawing} // reset drawingApp, but keep brush-style
                | Interactions.PickCrackDetection ->
                    let crackd =
                        let h = model.opcInfos |> HMap.toSeq |> Seq.map (fun (_, opc) -> opc.patchHierarchy)
                        model.crackDetection |> CrackDetectionApp.update (FinishCrack h)

                    let points =
                        crackd.outputPoints

                    let newAnnotation = AnnotationApp.update model.annotations (AnnotationAction.AddCrack points)
                    { model with crackDetection = crackd; annotations = newAnnotation; drawing = DrawingModel.reset model.drawing }

                | _-> model

            //| Keys.T ->
            //  let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
            //  //let updatedPicking = PickingApp.update model.picking (PickingAction.AddTestBrushes pointsOnAxisFunc)
            //  let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.Finish) // TEST
            //  let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
            //  { model with axis = axis; drawing = updatedDrawing }
            | _ -> model
        | PickingAction msg ->
          // TODO...refactor this!
          //let pickingModel, drawingModel =

            match msg with
            | HitSurface (box, hit) | HitSurfaceWithIndex (box, hit) ->
              //match model.axis with
              //| Some axis ->
              //  let axisNearstFunc = fun p -> (fst (AxisFunctions.getNearestPointOnAxis' p axis)).position
              //  PickingApp.update model.picking (HitSurface (a,b, axisNearstFunc))
              //| None -> PickingApp.update model.picking msg

                let hitF (queryPoint: V3d) =
                    let fray = FastRay3d(V3d.Zero, (queryPoint).Normalized)
                    model.picking.pickingInfos
                    |> HMap.tryFind model.boundingBox   // WRONG use local boundingbox
                    |> Option.bind (fun kk ->
                        OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some kk.kdTree) fray
                        |> Option.map (fun closest ->
                            fray.Ray.GetPointOnRay closest
                        )
                    )

                let picking =
                    (box, hit)
                    |> HitSurfaceWithIndex
                    |> PickingApp.update model.picking

                match model.picking.interaction with
                | Interactions.DrawAnnotation ->

                    let drawing =
                        match picking.lastHit with
                        | Some hit ->
                            (hit.position, Some hitF)
                            |> DrawingAction.AddPoint
                            |> DrawingApp.update model.drawing
                        | None ->
                            model.drawing
                    { model with picking = picking; drawing = drawing }

                | Interactions.PickCrackDetection ->

                    let crackDetection =
                        match picking.lastHit with
                        | Some hit ->
                            model.crackDetection |> CrackDetectionApp.update (AddCrackPoint hit)
                        | _ ->
                            model.crackDetection

                    { model with picking = picking; crackDetection = crackDetection }

                | _->
                    Log.error "[App] Unknown interaction mode %A" model.picking.interaction
                    model

            | _ -> { model with picking = (PickingApp.update model.picking msg) }

        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }
        | AttributeAction msg ->
            { model with opcAttributes = SurfaceAttributes.update model.opcAttributes msg }
        | DrawingAction msg ->
            { model with drawing = DrawingApp.update model.drawing msg }
        | AnnotationAction msg ->
            { model with annotations = AnnotationApp.update model.annotations msg }
        | _ -> model

    let view (m : MModel) =
        let box =
            m.patchHierarchies
            |> List.map(fun x -> x.tree |> QTree.getRoot)
            |> List.map(fun x -> x.info.LocalBoundingBox)
            |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid

        let interaction = m.picking.interaction

        let colors =
            [|
                C4b.Red; C4b.Green; C4b.Blue; C4b.Yellow; C4b.Magenta; C4b.Cyan;
                C4b.DarkRed; C4b.DarkGreen; C4b.DarkBlue; C4b.DarkYellow; C4b.DarkMagenta; C4b.DarkCyan; C4b.VRVisGreen
            |]
            |> Array.map (C3d >> V3d)

        let opcs =
            m.opcInfos
            |> AMap.toASet
            |> ASet.toAList
            |> AList.toMod
            |> Mod.map (fun list ->
                list
                |> PList.toArray
                |> Array.mapi (fun index info ->
                    Sg.createSingleOpcSg m.opcAttributes.selectedScalar m.opcAttributes.selectedTexture m.pickingActive interaction m.cameraState.view info
                    |> Sg.uniform "DebugColor" (Mod.constant colors.[index % colors.Length])
                )
                |> Sg.ofArray
            )
            |> Sg.dynamic
            |> Sg.effect [
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.diffuseTexture
                toEffect Shader.AttributeShader.falseColorLegend //falseColorLegendGray
                toEffect Shader.AttributeShader.markPatchBorders
                Shader.MultipliedDebugColor.Effect
            ]

        let boundingBoxSg =
            m.opcInfos
            |> AMap.toASet
            |> ASet.map Sg.createBoundingBoxSg
            |> Sg.set
            |> Sg.effect [
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.vertexColor
            ]

        let near = m.mainFrustum |> Mod.map(fun x -> x.near)
        let far = m.mainFrustum |> Mod.map(fun x -> x.far)


        let filledPolygonSg, afterFilledPolygonRenderPass =
            m.annotations
            |> AnnotationApp.viewGrouped near far (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)

        let afterFilledPolygonSg =
            [
                m.drawing |> DrawingApp.view near far
                m |> AxisSg.axisSgs
            ]
            |> Sg.ofList
            |> Sg.pass afterFilledPolygonRenderPass

        let crackBrush =
            m.crackDetection
            |> CrackDetectionApp.viewBrush near far
            |> Sg.pass afterFilledPolygonRenderPass

        //let cracks = CrackDetection.drawCracks m.crackDetection near far (Mod.constant 10.0) (Mod.constant 0.1)

        let scene =
            [
                opcs
                //boundingBoxSg
                filledPolygonSg
                afterFilledPolygonSg
                crackBrush
                //cracks
            ]
            |> Sg.ofList

        let textOverlays (model : MModel) =
            div [js "oncontextmenu" "event.preventDefault();"] [
                let style' = "color: white; font-family:Consolas;"

                yield div [clazz "ui"; style "position: absolute; top: 15px; left: 15px; float:left" ] [
                    yield table [style style'] [
                        tr[][
                            td[][
                                Incremental.text(model.cameraState.view |> Mod.map(fun x -> x.Location.ToString("0.00")))
                            ]
                        ]
                        tr[][
                            td[][
                                Incremental.text(model.picking.interaction |> Mod.map string)
                            ]
                        ]
                    ]
                ]
            ]

        let renderControl =
            FreeFlyController.controlledControl m.cameraState Camera m.mainFrustum
                (AttributeMap.ofList [
                    style "width: 100%; height:100%";
                    attribute "showFPS" "true";       // optional, default is false
                    attribute "useMapping" "true"
                    attribute "data-renderalways" "false"
                    attribute "data-samples" "4"
                    onKeyDown (Message.KeyDown)
                    onKeyUp (Message.KeyUp)
                    //onBlur (fun _ -> Camera FreeFlyController.Message.Blur)
                ])
                (scene |> Sg.map PickingAction)

        //let frustum = Frustum.perspective 60.0 0.1 50000.0 1.0 |> Mod.constant
        //let cam = Mod.map2 Camera.create m.cameraState.view frustum

        let semui =
            [
                { kind = Stylesheet; name = "semantic.css";    url = "semantic.css" }
                { kind = Script;     name = "semantic.js";     url = "semantic.js" }
                { kind = Script;     name = "essential";       url = "essentialstuff.js" }
                { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
                { kind = Script;     name = "spectrum.js";     url = "spectrum.js" }
                { kind = Stylesheet; name = "spectrum.css";    url = "spectrum.css" }
            ]

        page (fun request ->
            match Map.tryFind "page" request.queryParams with
            | Some "render" ->
                require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
                    div [clazz "ui"; style "background: #1B1C1E"] [
                        renderControl
                        textOverlays m
                        SurfaceAttributes.scalarsColorLegend m.opcAttributes
                    ]
                )
            | Some "controls" ->
                require Html.semui (
                    body [style "width: 100%; height:100%; background: transparent";] [
                        div[style "color:white; margin: 5px 15px 5px 5px"][
                            h3[][text "NIOBE"]
                            p[][text "Hold Ctrl-Left to add Point"]
                            p[][text "Press Enter to close Polygon"]
                            //p[][div[][text "VolumeGeneration: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.picking.volumeGenerationOptions |> AMap.map (fun k v -> text v)) m.picking.volumeGeneration PickingAction.SetVolumeGeneration ]] |> UI.map PickingAction
                            //p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.debugShadowVolume PickingAction.ShowDebugVis "Show Debug Vis"] |> UI.map PickingAction
                            //p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.useGrouping PickingAction.UseGrouping "Use Grouping"] |> UI.map PickingAction
                            //p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.showOutline PickingAction.ShowOutline "Show Outline"] |> UI.map PickingAction
                            //p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.showDetailOutline PickingAction.ShowOutlineDetail "Show Outline Detail"] |> UI.map PickingAction
                            //p[][div[][text "Alpha: "; slider { min = 0.0; max = 1.0; step = 0.05 } [clazz "ui inverted blue slider"] m.picking.alpha PickingAction.SetAlpha]] |> UI.map PickingAction
                            //p[][div[][text "Extrusion: "; slider { min = 0.05; max = 500.0; step = 5.0 } [clazz "ui inverted blue slider"] m.picking.extrusionOffset PickingAction.SetExtrusionOffset]] |> UI.map PickingAction
                        ]
                        div[style "color:white; margin: 5px 15px 5px 5px"][
                            DrawingApp.viewGui m.drawing |> UI.map DrawingAction
                        ]
                    ]
                )
            | Some "falseColors" ->
                require semui (
                    SurfaceAttributes.view m.opcAttributes |> UI.map AttributeAction
                )
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ]
            | None ->
                m.dockConfig
                |> docking [
                    style "width:100%; height:100%; background:#F00"
                    onLayoutChanged UpdateDockConfig
                ]
            )

    let app dir axisFile (rotate : bool) =
        Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

        let phDirs =
            Directory.GetDirectories(dir)

        let axis =
            axisFile
            |> Option.map(fun fileName -> AxisFunctions.loadAxis fileName)
            |> Option.defaultValue None

        let patchHierarchies =
            [
                for h in phDirs do
                    yield PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle (h |> OpcPaths)
            ]

        let box =
            patchHierarchies
            |> List.map(fun x -> x.tree |> QTree.getRoot)
            |> List.map(fun x -> x.info.GlobalBoundingBox)
            |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid

        let opcInfos =
            [
                for h in patchHierarchies do

                    let rootTree = h.tree |> QTree.getRoot

                    yield {
                        patchHierarchy = h
                        kdTree         = Aardvark.VRVis.Opc.KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath (KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ Serialization.binarySerializer)
                        localBB        = rootTree.info.LocalBoundingBox
                        globalBB       = rootTree.info.GlobalBoundingBox
                        neighborMap    = HMap.empty
                    }
            ]
            |> List.map (fun info -> info.globalBB, info)
            |> HMap.ofList

        let up = if rotate then (box.Center.Normalized) else V3d.OOI

        let restoreCamState : CameraControllerState =
            if File.Exists ".\camstate" then
                Log.line "[App] restoring camstate"
                let csLight : CameraStateLean = Serialization.loadAs ".\camstate"
                { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
            else
                { FreeFlyController.initial with view = CameraView.lookAt (box.Max) box.Center up; }

        let camState = restoreCamState

        let crackDetection : CrackDetectionModel =
            if File.Exists ".\crackdetectionState" then
                Serialization.loadAs ".\crackdetectionState"
            else
                CrackDetectionApp.initModel

        let ffConfig = {
            camState.freeFlyConfig with
                lookAtMouseSensitivity = 0.004;
                lookAtDamping = 50.0;
                moveSensitivity = 0.0
            }

        let camState =
            camState
            |> Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

        let initialDockConfig =
            config {
                content (
                    horizontal 1.0 [
                        element { id "render"; title "Render View"; weight 0.7 }
                        vertical 0.3 [
                            element { id "controls"; title "Controls"; weight 0.6 }
                            element { id "falseColors"; title "FalseColors"; weight 0.4 }
                        ]
                    ]
                )
                appName "OpcSelectionViewer"
                useCachedConfig true
            }

        let initialModel : Model =
            {
                cameraState        = camState
                mainFrustum        = Frustum.perspective 60.0 0.01 1000.0 1.0
                fillMode           = FillMode.Fill
                patchHierarchies   = patchHierarchies
                boundingBox        = box
                axis               = axis

                threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
                boxes              = List.empty //kdTrees |> HMap.toList |> List.map fst

                pickingActive      = false
                opcInfos           = opcInfos
                picking            = { PickingModel.initial with pickingInfos = opcInfos }
                dockConfig         = initialDockConfig

                //interaction        = Interactions.DrawAnnotation
                opcAttributes      = SurfaceAttributes.initModel dir
                drawing            = DrawingModel.initial
                annotations        = AnnotationModel.initial
                crackDetection     = crackDetection
            }

        {
            initial = initialModel
            update = update
            view   = view
            threads = fun m -> m.threads
            unpersist = Unpersist.instance<Model, MModel>
        }
