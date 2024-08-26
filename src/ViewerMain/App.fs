namespace OpcSelectionViewer

open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Data.Opc
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

open Adaptify.FSharp.Core

open Aether
open Aether.Operators

open Aardvark.Application
open OpcViewer.Base.KdTrees

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
            | _ -> model
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
                Log.line "[App] saving camstate"
                model.cameraState.view |> toCameraStateLean |> Serialization.save ".\camstate" |> ignore
                model
            | Keys.Enter ->
                //let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
                //let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) // TODO...add hitFunc
                //let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
                //{ model with axis = axis; drawing = updatedDrawing }
    
                let finished = { model with drawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) } // TODO add dummy-hitF
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, None))
                { finished with annotations = newAnnotation; drawing = DrawingModel.reset model.drawing} // reset drawingApp, but keep brush-style
            //| Keys.T ->
            //  let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
            //  //let updatedPicking = PickingApp.update model.picking (PickingAction.AddTestBrushes pointsOnAxisFunc)
            //  let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.Finish) // TEST
            //  let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
            //  { model with axis = axis; drawing = updatedDrawing }
            | _ -> model
        | PickingAction msg -> 
          // TODO...refactor this!
            let pickingModel, drawingModel =
                match msg with
                | HitSurface (a,b) -> //,_) -> 
                    //match model.axis with
                    //| Some axis -> 
                    //  let axisNearstFunc = fun p -> (fst (AxisFunctions.getNearestPointOnAxis' p axis)).position
                    //  PickingApp.update model.picking (HitSurface (a,b, axisNearstFunc))
                    //| None -> PickingApp.update model.picking msg
    
                    let hitF (queryPoint: V3d) = 
                        let fray = FastRay3d(V3d.Zero, (queryPoint).Normalized)
                        model.picking.pickingInfos 
                        |> HashMap.tryFind model.boundingBox   // WRONG use local boundingbox
                        |> Option.bind (fun kk -> 
                            OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some kk.kdTree) fray 
                            |> Option.map (fun closest ->
                                fray.Ray.GetPointOnRay closest
                            )
                        )
    
                    let updatePickM = PickingApp.update model.picking (HitSurface (a,b))
                    let lastPick = updatePickM.intersectionPoints |> IndexList.tryFirst
                    let updatedDrawM =
                        match lastPick with
                        | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, Some hitF))
                        | None -> model.drawing
                    updatePickM, updatedDrawM
                | _ -> PickingApp.update model.picking msg, model.drawing
            { model with picking = pickingModel; drawing = drawingModel }
        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }
        | AttributeAction msg ->
            { model with opcAttributes = SurfaceAttributes.update model.opcAttributes msg }
        | DrawingAction msg -> 
            { model with drawing = DrawingApp.update model.drawing msg }
        | AnnotationAction msg -> 
            { model with annotations = AnnotationApp.update model.annotations msg }
        | _ -> 
            model
                      
    let view (m : AdaptiveModel) =
                                               
        let box = 
            m.patchHierarchies
            |> List.map(fun x -> x.tree |> QTree.getRoot) 
            |> List.map(fun x -> x.info.LocalBoundingBox)
            |> List.fold (fun a b -> Box3d(a, b)) Box3d.Invalid
        
        let scalar = 
            m.opcAttributes.selectedScalar 
            |> AVal.map (function AdaptiveNone -> None | AdaptiveSome s -> Some s)
        
        let opcs = 
            m.opcInfos
            |> AMap.toASet
            |> ASet.map(fun info -> Sg.createSingleOpcSg scalar m.pickingActive m.cameraState.view info)
            |> Sg.set
            |> Sg.effect [ 
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.diffuseTexture  
                toEffect Shader.AttributeShader.falseColorLegend //falseColorLegendGray
            ]
    
        let near = m.mainFrustum |> AVal.map(fun x -> x.near)
        let far = m.mainFrustum |> AVal.map(fun x -> x.far)
        
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
    
        let scene = 
            [
                opcs
                filledPolygonSg
                afterFilledPolygonSg
            ]
            |> Sg.ofList
    
        let textOverlays (cv : aval<CameraView>) = 
            div [js "oncontextmenu" "event.preventDefault();"] [ 
                let style' = "color: white; font-family:Consolas;"
                
                yield div [clazz "ui"; style "position: absolute; top: 15px; left: 15px; float:left" ] [          
                    yield table [] [
                        tr[][
                            td[style style'][
                                Incremental.text(cv |> AVal.map(fun x -> x.Location.ToString("0.00")))
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
              
        //let frustum = Frustum.perspective 60.0 0.1 50000.0 1.0 |> AVal.constant          
        //let cam = AVal.map2 Camera.create m.cameraState.view frustum 
    
        let semui = 
            [ 
                { kind = Stylesheet; name = "semantic.css"; url = "semantic.css" }
                { kind = Script; name = "semantic.js"; url = "semantic.js" }
                { kind = Script; name = "essential"; url = "essentialstuff.js" }
                { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
                { kind = Script; name = "spectrum.js";  url = "spectrum.js" }
                { kind = Stylesheet; name = "spectrum.css";  url = "spectrum.css"}
            ]
    
        page (fun request -> 
            match Map.tryFind "page" request.queryParams with
            | Some "render" ->
                require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
                    div [clazz "ui"; style "background: #1B1C1E"] [
                      renderControl; 
                      textOverlays (m.cameraState.view);
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
    
        let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton
    
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
            |> List.fold (fun a b -> Box3d(a, b)) Box3d.Invalid
        
        let opcInfos = 
            [
                for h in patchHierarchies do
                  
                    let rootTree = h.tree |> QTree.getRoot
                    let kd = KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ Serialization.binarySerializer false false (fun _ _ -> failwith "no function for creating triangle sets") false false KdTreeParameters.legacyDefault

                    yield {
                        patchHierarchy = h
                        kdTree         = KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath kd
                        localBB        = rootTree.info.LocalBoundingBox 
                        globalBB       = rootTree.info.GlobalBoundingBox
                        neighborMap    = HashMap.empty
                    }
            ]
            |> List.map (fun info -> info.globalBB, info)
            |> HashMap.ofList      
                        
        let up = if rotate then (box.Center.Normalized) else V3d.OOI
    
        let restoreCamState : CameraControllerState =
            if File.Exists ".\camstate" then          
                Log.line "[App] restoring camstate"
                let csLight : CameraStateLean = Serialization.loadAs ".\camstate"
                { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
            else 
                { FreeFlyController.initial with view = CameraView.lookAt (box.Max) box.Center up; }                    
    
        let camState = restoreCamState
    
        let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
        let camState = camState |> ffConfig ^= CameraControllerState.freeFlyConfig_  
    
        let initialDockConfig = 
            config {
                content (
                    horizontal 1.0 [
                        stack 0.7 (Some "render") [
                            { id = "render"; title = Some " Main View "; weight = 1.0; deleteInvisible = None; isCloseable = None }
                        ]
                        stack 0.3 (Some "falseColors") [
                            { id = "falseColors"; title = Some " Attributes "; weight = 1.0; deleteInvisible = None; isCloseable = None }
                            { id = "controls"; title = Some " Rabbyte "; weight = 1.0; deleteInvisible = None; isCloseable = None }
                        ]
                        //element { id "render"; title "Render View"; weight 0.7 }
                        //element { id "controls"; title "Controls"; weight 0.3 } 
                        //element { id "falseColors"; title "FalseColors"; weight 0.3 }  
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
                boxes              = List.empty //kdTrees |> HashMap.toList |> List.map fst
                
                pickingActive      = false
                opcInfos           = opcInfos
                picking            = { PickingModel.initial with pickingInfos = opcInfos }
                dockConfig         = initialDockConfig       
                
                opcAttributes      = SurfaceAttributes.initModel dir
                drawing            = DrawingModel.initial
                annotations        = AnnotationModel.initial
            }
    
        {
            initial = initialModel             
            update = update
            view   = view          
            threads = fun m -> m.threads
            unpersist = Unpersist.instance<Model, AdaptiveModel>
        }
       