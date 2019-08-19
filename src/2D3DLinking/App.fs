namespace LinkingView

open System.IO

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

open Aardvark.Rendering.Text

open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open FShade

open Aardvark.Application
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos

open OpcViewer.Base
open OpcViewer.Base.Picking
open PRo3D.Minerva
open Aardvark.VRVis.Opc
open Rabbyte.Annotation
open Rabbyte.Drawing
open Linking


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
    
    //---saving and restoring camera state
    let toCameraStateLean (view : CameraView) : CameraStateLean = 
        {
        location = view.Location
        forward  = view.Forward
        sky      = view.Sky
        }

    let fromCameraStateLean (c : CameraStateLean) : CameraView = 
        CameraView.lookAt c.location (c.location + c.forward) c.sky    
    //---


    //---saving and restoring plane state
    let toPlaneCoords (coords : plist<V3d>): PlaneCoordinates =
        {
        points = coords
        }

    let fromPlaneCoords (c : PlaneCoordinates) : plist<V3d> =
        c.points
    //---

    //---UPDATE
    let update (model : Model) (msg : Action) =   
        match msg with
        | Camera m when model.pickingActive = false -> 
            { model with cameraState = FreeFlyController.update model.cameraState m; }
        
        //| PickPoint pos -> 
        //    { model with pickedPoint = pos}

        | Action.KeyDown m ->
         match m with
          | Keys.LeftCtrl -> 
            { model with pickingActive = true }
          | _ -> model
        | Action.KeyUp m ->
            match m with
            | Keys.LeftCtrl -> 
                { model with pickingActive = false }
            | Keys.Delete ->            
                { model with pickingModel = PickingApp.update model.pickingModel (PickingAction.ClearPoints) }
            | Keys.Back ->
                { model with pickingModel = PickingApp.update model.pickingModel (PickingAction.RemoveLastPoint) }
            | Keys.PageUp ->             
                { model with cameraState = model.cameraState |>  updateFreeFlyConfig +0.5 }
            | Keys.PageDown ->             
                { model with cameraState = model.cameraState |>  updateFreeFlyConfig -0.5 }
            | Keys.Space ->    
                Log.line "[App] saving camstate"
                model.cameraState.view |> toCameraStateLean |> OpcSelectionViewer.Serialization.save ".\camerastate" |> ignore
                model
            | Keys.Enter ->
                let finished = { model with drawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) } // TODO add dummy-hitF
                let dir = Direction (model.drawing.points |> PList.toSeq |> fun x -> PlaneFitting.planeFit x).Normal
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, Some dir))
                { finished with annotations = newAnnotation; drawing = DrawingModel.initial} // clear drawingApp
                
            | _ -> model


        | PickingAction msg ->
            // TODO...refactor this!
            let pickingModel, drawingModel, linkingModel =
              match msg with
              | HitSurface (a,b) -> //,_) -> 
                //match model.axis with
                //| Some axis -> 
                //  let axisNearstFunc = fun p -> (fst (AxisFunctions.getNearestPointOnAxis' p axis)).position
                //  PickingApp.update model.picking (HitSurface (a,b, axisNearstFunc))
                //| None -> PickingApp.update model.picking msg
                let updatePickM = PickingApp.update model.pickingModel (HitSurface (a,b))
                let lastPick = updatePickM.intersectionPoints |> PList.tryFirst

                let updatedDrawM = model.drawing // DISABLING DRAWING
                //let updatedDrawM =
                //    match lastPick with
                //    | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                //    | None -> model.drawing

                let updatedLinkingM = 
                    match lastPick with
                    | Some p -> LinkingApp.update model.cameraState.view model.linkingModel (LinkingAction.CheckPoint(p))
                    | None -> model.linkingModel

                updatePickM, updatedDrawM, updatedLinkingM
              | _ -> PickingApp.update model.pickingModel msg, model.drawing, model.linkingModel
            { model with pickingModel = pickingModel; drawing = drawingModel; linkingModel = linkingModel }
        
        | DrawingAction msg ->
            { model with drawing = DrawingApp.update model.drawing msg }

        | AnnotationAction msg ->
            { model with annotations = AnnotationApp.update model.annotations msg }

        | LinkingAction msg ->
            //{ model with minervaModel = MinervaApp.update model.cameraState.view model.minervaModel msg }
            { model with linkingModel = LinkingApp.update model.cameraState.view model.linkingModel msg }

        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }
        | _ -> model
    
    //---

    //---VIEW
    //let viewFeaturesSg (model : MMinervaModel) =
    //    let pointSize = Mod.constant 10.0 //model.featureProperties.pointSize.value

    //    Sg.ofList [
    //        // Drawing.featureMousePick model.kdTreeBounds
    //        Drawing.drawFeaturePoints model.sgFeatures pointSize
    //        Drawing.drawSelectedFeaturePoints model.selectedSgFeatures pointSize
    //        Drawing.drawHoveredFeaturePoint model.hoveredProduct pointSize model.sgFeatures.trafo
    //    ]

    let view (m : MModel) =
                               
        let opcs = 
            m.opcInfos
                |> AMap.toASet
                |> ASet.map(fun info -> Sg.createSingleOpcSg (Mod.constant None) m.pickingActive m.cameraState.view info)
                |> Sg.set
                |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.diffuseTexture       
                ]

        let pos = V3d [|-2486735.62;2289118.43;-276194.91|]
        let trafo = Mod.constant (Trafo3d.Translation pos)
        let radius = Mod.constant 0.1
        let debug = 
            Sg.sphere 4 (Mod.constant C4b.Cyan) radius
            |> Sg.trafo trafo
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            //|> Sg.uniform "Size" radius // 5.0
            |> Sg.effect [
                //Shader.ScreenSpaceScale.Effect
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
            ]

        let near = m.mainFrustum |> Mod.map(fun x -> x.near)
        let far = m.mainFrustum |> Mod.map(fun x -> x.far)


        let filledPolygonSg, afterFilledPolygonRenderPass = 
          m.annotations 
          |> AnnotationApp.viewGrouped near far (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)

        let afterFilledPolygonSg = 
            [
                LinkingApp.view m.linkingModel |> Sg.map LinkingAction
                DrawingApp.view near far (* whereever you are ♫ *) m.drawing |> Sg.map DrawingAction
            ] 
            |> Sg.ofList
            |> Sg.pass afterFilledPolygonRenderPass

        let scene = 
          [
              opcs |> Sg.map PickingAction
              filledPolygonSg |> Sg.map AnnotationAction
              afterFilledPolygonSg
          ]
          |> Sg.ofList

        let textOverlays (cv : IMod<CameraView>) = 
            div [js "oncontextmenu" "event.preventDefault();"] [ 
                let style' = "color: white; font-family:Consolas;"
    
                yield div [clazz "ui"; style "position: absolute; top: 15px; left: 15px; float:left" ] [          
                    yield table [] [
                    tr[][
                        td[style style'][Incremental.text(cv |> Mod.map(fun x -> x.Location.ToString("0.00")))]
                    ]
                    //tr[][ 
                    //    td[style style'][Incremental.text(points |> Mod.map(fun x -> x |> Array.map (fun e -> e.ToString("0.00")) |> String.concat ", "))]
                    //]
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
                    onKeyDown (Action.KeyDown)
                    onKeyUp (Action.KeyUp)
                ]) 
                (scene) 
            //(scene |> Sg.map PickingAction) 
            
 
        page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
            require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
                div [clazz "ui"; style "background: #1B1C1E"] [renderControl; textOverlays (m.cameraState.view)]
            )
        | Some "controls" -> 
            require Html.semui (
                body [style "width: 100%; height:100%; background: transparent";] [
                    div[style "color:white; margin: 5px 15px 5px 5px"][
                    h3[][text "2D/3D Linking"]
                    p[][text "Hold Ctrl-Left to add Point"]
                    p[][text "Press Enter to close Polygon"]

                    LinkingApp.guiView m.linkingModel |> UI.map LinkingAction

                    //p[][div[][text "VolumeGeneration: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.pickingModel.volumeGenerationOptions |> AMap.map (fun k v -> text v)) m.pickingModel.volumeGeneration PickingAction.SetVolumeGeneration ]] |> UI.map PickingAction
                    //p[][checkbox [clazz "ui inverted toggle checkbox"] m.pickingModel.debugShadowVolume PickingAction.ShowDebugVis "Show Debug Vis"] |> UI.map PickingAction
                    //p[][checkbox [clazz "ui inverted toggle checkbox"] m.pickingModel.useGrouping PickingAction.UseGrouping "Use Grouping"] |> UI.map PickingAction
                    //p[][checkbox [clazz "ui inverted toggle checkbox"] m.pickingModel.showOutline PickingAction.ShowOutline "Show Outline"] |> UI.map PickingAction
                    //p[][checkbox [clazz "ui inverted toggle checkbox"] m.pickingModel.showDetailOutline PickingAction.ShowOutlineDetail "Show Outline Detail"] |> UI.map PickingAction
                    //p[][div[][text "Alpha: "; slider { min = 0.0; max = 1.0; step = 0.05 } [clazz "ui inverted blue slider"] m.pickingModel.alpha PickingAction.SetAlpha]] |> UI.map PickingAction
                    //p[][div[][text "Extrusion: "; slider { min = 0.05; max = 500.0; step = 5.0 } [clazz "ui inverted blue slider"] m.pickingModel.extrusionOffset PickingAction.SetExtrusionOffset]] |> UI.map PickingAction
                    ]
                ]
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
                onLayoutChanged UpdateDockConfig ]
        )


    //---


    let app dir (rotate : bool) dumpFile cacheFile =
      OpcSelectionViewer.Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

      let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton


      let patchHierarchies =
        [ 
          for h in phDirs do
            yield PatchHierarchy.load 
              OpcSelectionViewer.Serialization.binarySerializer.Pickle 
              OpcSelectionViewer.Serialization.binarySerializer.UnPickle 
              (h |> OpcPaths)
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
              kdTree         = Aardvark.VRVis.Opc.KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath (KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ OpcSelectionViewer.Serialization.binarySerializer)
              localBB        = rootTree.info.LocalBoundingBox 
              globalBB       = rootTree.info.GlobalBoundingBox
              neighborMap    = HMap.empty
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HMap.ofList      
                      
      let up = if rotate then (box.Center.Normalized) else V3d.OOI

      let restoreCamState : CameraControllerState =
        if File.Exists ".\camerastate" then          
          Log.line "[App] restoring camstate"
          let csLight : CameraStateLean = OpcSelectionViewer.Serialization.loadAs ".\camerastate"
          { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
        else 
          { FreeFlyController.initial with view = CameraView.lookAt (box.Max) box.Center up; }                    

      let camState = restoreCamState

      let restorePlane =
        if File.Exists ".\planestate" then
            Log.line "[App] restoring planestate"
            let p : PlaneCoordinates = OpcSelectionViewer.Serialization.loadAs ".\planestate"
            p |> fromPlaneCoords
        else
        PList.empty

      let planeState = restorePlane

      let setPlaneForPicking =
        match planeState.IsEmpty() with
            | true -> None
            | false -> Some planeState


      let roverinitialCamera = CameraView.lookAt box.Max box.Center box.Center.Normalized


      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> OpcSelectionViewer.Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

      let initialDockConfig = 
        config {
          content (
              horizontal 10.0 [
                  element { id "render"; title "Render View"; weight 7.0 }
                  element { id "controls"; title "Controls"; weight 3.0 }                         
              ]
          )
          appName "ViewPlanner"
          useCachedConfig true
        }
      
      let initialModel : Model = 
        { 
          cameraState        = camState
          mainFrustum        = Frustum.perspective 60.0 0.01 1000.0 1.0
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies          
          
          threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
          boxes              = List.empty 
      
          pickingActive      = false
          opcInfos           = opcInfos
          pickingModel       = { PickingModel.initial with pickingInfos = opcInfos }
          annotations        = AnnotationModel.initial
          drawing            = DrawingModel.initial
          pickedPoint        = None
          planePoints        = setPlaneForPicking
          dockConfig         = initialDockConfig   
          linkingModel       = LinkingApp.update camState.view LinkingModel.initial (MinervaAction(LoadProducts(dumpFile, cacheFile)))
        }

      

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }


