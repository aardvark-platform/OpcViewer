namespace ViewPlanner

open System.IO

open Aardvark.Base
open FSharp.Data.Adaptive
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

open ViewPlanner.Rover

open Aardvark.VRVis.Opc
open Rabbyte.Drawing
open Rabbyte.Annotation

open Adaptify.FSharp.Core

open Aether.Operators

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
    let toPlaneCoords (coords : IndexList<V3d>): PlaneCoordinates =
        {
        points = coords
        }

    let fromPlaneCoords (c : PlaneCoordinates) : IndexList<V3d> =
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

            | Keys.V ->
                Log.line "[App] saving plane points"
                model.pickingModel.intersectionPoints |> toPlaneCoords |> OpcSelectionViewer.Serialization.save ".\planestate" |> ignore
                model

            | Keys.L -> //if R is pressed then picked point on plane is new rover target
                let picked = model.pickedPoint
                let roverModel = 
                    match picked with
                        | Some p -> 
                            let forward = p-model.rover.position
                            let cam = CameraView.look model.rover.position forward.Normalized model.rover.up
                            { model.rover with target = p; camera = cam}
                        
                        | None -> model.rover
                { model with rover = roverModel}

            | Keys.Enter ->
                let finished = { model with drawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) } // TODO add dummy-hitF
                let dir = Direction (model.drawing.points |> IndexList.toSeq |> fun x -> PlaneFitting.planeFit x).Normal
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, Some dir))
                { finished with annotations = newAnnotation; drawing = DrawingModel.reset model.drawing} // reset drawingApp, but keep brush-style
                
                //let pointsOnAxisFunc = OpcSelectionViewer.AxisFunctions.pointsOnAxis None
                //let updatedPicking = PickingApp.update model.pickingModel (PickingAction.AddBrush pointsOnAxisFunc)
                //let updatedDrawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) // TODO...add hitFunc
                //{ model with drawing = updatedDrawing }
            | _ -> model

        | PickingAction msg -> 
            let pickingModel, drawingModel =
                match msg with
                    | HitSurface (a,b) -> //,_) -> 
                        let updatePickM = PickingApp.update model.pickingModel (HitSurface (a,b))
                        let lastPick = updatePickM.intersectionPoints |> IndexList.tryFirst
                        let updatedDrawM =
                            match lastPick with
                            | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                            | None -> model.drawing
                        updatePickM, updatedDrawM
                    | _ -> PickingApp.update model.pickingModel msg, model.drawing
            { model with pickingModel = pickingModel; drawing = drawingModel }

        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }

        | RoverAction msg -> 
            match msg with
                  | ChangePosition pos -> 
                        let r = RoverApp.update model.rover (ChangePosition pos)
                        {model with rover = r}
                //| ChangePan p -> 
                //    let r = RoverApp.update model.rover (ChangePan p)
                //    {model with rover = r}
                    
                //| ChangeTilt t -> 
                //    let r = RoverApp.update model.rover (ChangeTilt t)
                //    {model with rover = r}


        | _ -> model
    
    //---

    //---VIEW
    let view (m : AdaptiveModel) =
                                                 
      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg (AVal.constant None) m.pickingActive m.cameraState.view info)
          |> Sg.set
          |> Sg.effect [ 
            toEffect Shader.stableTrafo
            toEffect DefaultSurfaces.diffuseTexture       
            ]

      let myPlane = 
        m.planePoints
            |> AVal.map (fun n ->
                match n with
                    | AdaptiveNone -> Sg.empty
                    | AdaptiveSome points -> 
                        points 
                            |> AList.toAVal
                            |> AVal.map (fun p ->
                                p
                                    |> IndexList.toSeq
                                    |> PlaneFitting.planeFit
                                    |> fun t ->
                                         let box = Aardvark.SceneGraph.SgPrimitives.Sg.box' C4b.Cyan (Box3d(V3d.NNN, V3d.III))
                                         let scaleT = Trafo3d.Scale(10.0, 20.0, 0.2)
                                         let sum = p.Sum()
                                         let c = p |> IndexList.count
                                         let average = sum / (float c)
                                         let pos = V3d(average.X, average.Y, average.Z)
                            
                                         let trafo = scaleT * Trafo3d.RotateInto(V3d.OOI, t.Normal) * Trafo3d.Translation(pos)
                                         box
                                            |> Aardvark.SceneGraph.``Sg Picking Extensions``.Sg.requirePicking
                                            |> Sg.noEvents
                                            |> Sg.withEvents [
                                                SceneEventKind.DoubleClick, (fun sh -> 
                                                true, Seq.ofList [(RoverAction.ChangePosition (sh.globalPosition))]
                                                                            )
                                                              ]
                                            |> Sg.trafo (AVal.constant(trafo)) 
                                    |> Sg.effect [
                                            toEffect DefaultSurfaces.stableTrafo
                                            toEffect (DefaultSurfaces.constantColor C4f.DarkRed)
                                                 ]  
                                           )
                            |> Sg.dynamic
                      )





      let near = m.mainFrustum |> AVal.map(fun x -> x.near)
      let far = m.mainFrustum |> AVal.map(fun x -> x.far)

      let filledPolygonSg, afterFilledPolygonRenderPass = 
        m.annotations 
        |> AnnotationApp.viewGrouped near far (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)

      let afterFilledPolygonSg = 
        [
          m.drawing |> DrawingApp.view near far
          // myPlane
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
                    td[style style'][Incremental.text(cv |> AVal.map(fun x -> x.Location.ToString("0.00")))]
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
           onKeyDown (Action.KeyDown)
           onKeyUp (Action.KeyUp)
         ]) 
         (scene |> Sg.map PickingAction) 
            
 
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
                h3[][text "ROVER CONTROL"]
                p[][text "Press R to place rover at picked point"]
                p[][text "Press L to select picked point as rover target"]
                p[][Incremental.text (m.rover.position |> AVal.map (fun f -> f.ToString())) ]
                    //    match f with
                    //        |Some point -> "picked position:" + point.ToString()
                    //        |None -> "Double-click on plane to pick position"
                    //))]


                h3[][text "NIOBE"]
                p[][text "Hold Ctrl-Left to add Point"]
                p[][text "Press Enter to close Polygon"]
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


    let app dir (rotate : bool) =
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
              neighborMap    = HashMap.empty
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HashMap.ofList      
                      
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
        IndexList.empty

      let planeState = restorePlane

      let setPlaneForPicking =
        match planeState.IsEmpty with
            | true -> None
            | false -> Some planeState


      let roverinitialCamera = CameraView.lookAt box.Max box.Center box.Center.Normalized


      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> ffConfig ^= CameraControllerState.freeFlyConfig_ 

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
          drawing            = DrawingModel.initial
          annotations        = AnnotationModel.initial
          pickedPoint        = None
          planePoints        = setPlaneForPicking
          rover              = { RoverModel.initial with up = box.Center.Normalized; camera = roverinitialCamera; position = box.Center}
          dockConfig         = initialDockConfig            
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, AdaptiveModel>
      }


