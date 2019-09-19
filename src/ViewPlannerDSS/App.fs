namespace ViewPlanner

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

open ViewPlanner.Rover

open Aardvark.VRVis.Opc
open Rabbyte.Drawing
open Rabbyte.Annotation

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

    //---saving and restoring rover position and target
    let toRoverCoords(coords: plist<V3d>): initialRoverCoords = 
        {
        coordinates = coords
        }
    
    let fromRoverCoords(c:initialRoverCoords) : plist<V3d> = 
        c.coordinates






    //---UPDATE
    let update (model : Model) (msg : Action) =   
        match msg with
        | Camera m when model.pickingActive = false -> 
            { model with cameraState = FreeFlyController.update model.cameraState m; }

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
            

            | Keys.L -> 
                let intersect = model.pickingModel.intersectionPoints
                let l = intersect |> PList.toList
                let t = 
                    match l.IsEmpty with
                        | true -> V3d.OOO
                        | false -> l.Head
                let p = PickingApp.update model.pickingModel (PickingAction.RemoveLastPoint)
                let d = DrawingApp.update model.drawing (DrawingAction.RemoveLastPoint)
                let forward = t-model.rover.position
                let cam = CameraView.look model.rover.position forward.Normalized model.rover.up
                let r = { model.rover with target = t; HighResCam = { model.rover.HighResCam with cam = { model.rover.HighResCam.cam with camera = {model.rover.HighResCam.cam.camera with view = cam}}}}
                { model with rover = r; pickingModel = p; drawing = d}


            | Keys.R -> 
                let intersect = model.pickingModel.intersectionPoints
                let n = model.rover.up
                let l = intersect |> PList.toList
                let m = 
                    match l.IsEmpty with
                        | true -> V3d.OOO
                        | false -> l.Head
                let p = PickingApp.update model.pickingModel (PickingAction.RemoveLastPoint)
                let d = DrawingApp.update model.drawing (DrawingAction.RemoveLastPoint)
                let pos = m+n
                let r = { model.rover with position = pos; projsphere = {model.rover.projsphere with position = pos}; HighResCam = {model.rover.HighResCam with cam = {model.rover.HighResCam.cam with position = pos}}} 
                { model with rover = r; pickingModel = p; drawing = d}
                        

            | Keys.Enter ->
                let points = model.pickingModel.intersectionPoints
                let rover = { model.rover with reg = Some model.pickingModel.intersectionPoints }
                let finished = { model with drawing = DrawingApp.update model.drawing (DrawingAction.FinishClose None) } // TODO add dummy-hitF
                let dir = Direction (model.drawing.points |> PList.toSeq |> fun x -> PlaneFitting.planeFit x).Normal
                let newAnnotation = AnnotationApp.update finished.annotations (AnnotationAction.AddAnnotation (finished.drawing, Some dir))
                { finished with annotations = newAnnotation; drawing = DrawingModel.reset model.drawing; region = Some points; rover = rover} // reset drawingApp, but keep brush-style
                

            | Keys.F1 -> {model with roiBboxFull = not model.roiBboxFull}

            | _ -> model

        | PickingAction msg -> 
            match msg with
            | HitSurface (a,b) ->
                //check if roverplacement is active
                let mode = model.currentModeOption
                let placement = model.roverPlacement
                let active = placement.active
                let counterMax = placement.counterToMax
                let counter = placement.counter
                let max = placement.max
                let reachedMax = (counterMax = max)
                match mode,reachedMax with
                | Some RoverPlacementMode, false -> //placement is active, new rovers can be placed
                    
                    match counter with 
                    | 0 -> //position
                       let pm = PickingApp.update model.pickingModel (HitSurface (a,b))
                       let lastPick = pm.intersectionPoints |> PList.tryFirst
                       
                       match lastPick with
                       | Some pick -> 
                           let up = model.rover.up
                           let rover = {model.rover with position = (pick+up)}
                           let updatedRp = {model.roverPlacement with counter = (counter + 1)}
                           {model with rover = rover; roverPlacement = updatedRp; pickingModel = pm}

                       | None -> model

                    | 1 -> //target
                        let pm = PickingApp.update model.pickingModel (HitSurface (a,b))
                        let intersecPoints = pm.intersectionPoints |> PList.toList
                        let pos = intersecPoints.Item(1) + model.rover.up
                        let target = intersecPoints.Item(0)
                        let newPlacement = 
                            { 
                            id = counterMax + 1
                            position = pos
                            target = target
                            }
                        let newList = model.rover.positionsList.Append newPlacement
                        
                        let rover = {model.rover with position = pos; target = target; positionsList = newList}
                        let updatedRp = {model.roverPlacement with counter = 0; counterToMax = (counterMax + 1)}
                        
                        let pmReset = PickingApp.update pm ClearPoints
                        { model with rover = rover; roverPlacement = updatedRp; pickingModel = pmReset;}

                | Some RoverPlacementMode, true -> //placement is active, maximum number of rovers has been reached
                    let updatedRp = {model.roverPlacement with active = false}
                    { model with pickingModel = model.pickingModel; drawing = model.drawing; roverPlacement = updatedRp}

                | _, _  -> 
                            let updatePickM = PickingApp.update model.pickingModel (HitSurface (a,b))
                            let lastPick = updatePickM.intersectionPoints |> PList.tryFirst
                            let updatedDrawM =
                                match lastPick with
                                | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                                | None -> model.drawing
                            { model with pickingModel = updatePickM; drawing = updatedDrawM }

            | PickPointOnPlane p ->
                        let pm = PickingApp.update model.pickingModel (PickPointOnPlane p) 
                        { model with pickingModel = pm; drawing = model.drawing }

            | _ -> { model with pickingModel = model.pickingModel; drawing = model.drawing }

            
        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }

        | RoverAction msg -> 
            let r = RoverApp.update model.rover msg
            {model with rover = r}
                    
        | Configs msg ->
            //let mode = 
            //    msg 
            //    |> Option.map (fun x -> 
            //        match x with
            //        | ModeOption.ViewPlanMode -> model.viewPlanModeConfig
            //        | _ -> model.standardConfig
            //        )
            //    |> Option.defaultValue model.standardConfig

            { model with currentModeOption = msg}//; dockConfig = mode }
            
            //| Some SaveCameraState -> 
            //    Log.line "[App] saving camstate"
            //    model.cameraState.view |> toCameraStateLean |> OpcSelectionViewer.Serialization.save ".\camerastate" |> ignore 
            //    model
            //| Some SavePlaneState ->
            //    Log.line "[App] saving plane points"
            //    model.pickingModel.intersectionPoints |> toPlaneCoords |> OpcSelectionViewer.Serialization.save ".\planestate" |> ignore
            //    model
            //| Some SaveRoverState ->
            //    let intersect = model.pickingModel.intersectionPoints |> PList.toList
            //    let r =
            //        if intersect |> List.isEmpty then model
            //        elif intersect.Length < 2 then model
            //        else
            //            Log.line "[App] saving rover position and target"
            //            let n = model.rover.up
            //            let m = (intersect |> List.item (1)) + n
            //            let t = intersect |> List.item (0)
            //            let adaptedList = [m;t] |> PList.ofList
            //            adaptedList |> toRoverCoords |> OpcSelectionViewer.Serialization.save ".\Roverstate" |> ignore
                     
            //            let forward = t-m
            //            let cam = CameraView.look model.rover.position forward.Normalized model.rover.up

            //            let p = PickingApp.update model.pickingModel (PickingAction.ClearPoints)
            //            let d = DrawingApp.update model.drawing (DrawingAction.Clear) 

            //            let hrcam = {model.rover.HighResCam with cam = { model.rover.HighResCam.cam with position = m; camera = {model.rover.HighResCam.cam.camera with view = cam}} }
            //            let r = { model.rover with position = m; target = t; HighResCam = hrcam; projsphere = {model.rover.projsphere with position = m}} 
            //            { model with rover = r; pickingModel = p; drawing = d}

            //    r
          
        | _ -> model
    
    //---

    //---VIEW
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

       //projection points on sphere
      let ps = Sg.projectionPoints m.rover.projPoints
   
     
      //views

      let renderViewSg = Sg.setRenderViewScene m.currentModeOption m

      let rovercamScene = 
       [
          m.drawing |> DrawingApp.view
          //ps
       ] |> Sg.ofList

      let fullSceneRenderView = 
        m.annotations |> AnnotationApp.viewGrouped opcs RenderPass.main renderViewSg
    
      let camSceneRenderView = 
         m.annotations |> AnnotationApp.viewGrouped opcs RenderPass.main rovercamScene
   
     
      //show sample button or not
      //let criteria = 
      //      adaptive {
      //          let! pos = m.rover.selectedPosition
      //          let! reg = m.rover.reg
      //          let crit = 
      //             match pos,reg with
      //             | Some p, Some r -> true
      //             | _ -> false
      //          return crit
      //      }
    
      //let roverViews = Sg.createView (camSceneRenderView |> Sg.map PickingAction) m.rover.camera m.rover
      //let viewLeft = fst roverViews 
      //let viewRight = snd roverViews

      let camScene = (camSceneRenderView |> Sg.map PickingAction)
     

      let renderControl =
       FreeFlyController.controlledControl m.cameraState Camera (Frustum.perspective 60.0 0.01 1000.0 1.0 |> Mod.constant) 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "true";       
           attribute "useMapping" "true"
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
           onKeyDown (Action.KeyDown)
           onKeyUp (Action.KeyUp)
         ]) 
         (fullSceneRenderView |> Sg.map PickingAction)
      
     
      let dependencies =   
        Html.semui @ [        
          { name = "spectrum.js";  url = "spectrum.js";  kind = Script     }
          { name = "spectrum.css";  url = "spectrum.css";  kind = Stylesheet     }
          ]

     
      let mode = ViewUtilities.selectMode m.currentModeOption m

      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
 
          require Html.semui ( 
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl] 
          )
        
        | Some "leftCam" ->
            require Html.semui (

              body [style "width: 100%; height:100%; background: transparent";] [
                ViewUtilities.selectView m.currentModeOption camScene "left" m
                   ]

               
          )
        
         | Some "rightCam" ->
            require Html.semui (
                
                body [style "width: 100%; height:100%; background: transparent";] [
                 ViewUtilities.selectView m.currentModeOption camScene "right" m
                ]


               
            )

        | Some "controls" -> 

            require dependencies (
                body [style "width: 100%; height:100%; background: transparent";] [
                    
                    div[style "color:white; margin: 5px 15px 5px 5px"][

                        h4[][text "Mode Options"]
                        div [ clazz "item" ] [ 
                        dropdown { placeholder = ""; allowEmpty = false } [ clazz "ui inverted selection dropdown" ] (m.modeOptions |> AMap.map (fun k v -> text v)) m.currentModeOption Action.Configs 
                        ]
                    ]
                    br []
                    
                    mode
            ]
          )
            
 
        | Some other -> 
          let msg = sprintf "Unknown page: %A" other
          body [] [
              div [style "color: white; font-size: large; background-color: black; width: 100%; height: 100%"] [text msg]
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
        

      let restoreRoverCoords = 
         if File.Exists ".\Roverstate" then
            Log.line "[App] restoring roverstate"
            let p : initialRoverCoords = OpcSelectionViewer.Serialization.loadAs ".\Roverstate"
            p |> fromRoverCoords
         else
         PList.empty

      let roverState = restoreRoverCoords

      let initialRoverPos = 
         match roverState.IsEmpty() with
            | true -> V3d.OOO
            | false -> roverState |> PList.toList |> List.item (0)
      
      let initialRoverTarget = 
         match roverState.IsEmpty() with
            | true -> V3d.OOI
            | false -> roverState |> PList.toList |> List.item (1)
      
      
      let forward = initialRoverTarget - initialRoverPos

      let roverinitialCamera = {
        
        FreeFlyController.initial with view = CameraView.look initialRoverPos forward.Normalized box.Center.Normalized
      }


      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> OpcSelectionViewer.Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

      let viewPlanDockConfig = 
        config {
          content (

              horizontal 23.0 [
                
                vertical 14.0 [
                element {id "render"; title "Main View"; weight 8.0}
                horizontal 6.0[
                element {id "leftCam"; title "HR-Cam / WACL"; weight 3.0}
                element {id "rightCam"; title "WACR"; weight 3.0}
                ]
                
                ]
                element {id "controls"; title "Controls"; weight 9.0}
                
              
              ]

          )
          appName "ViewPlanner"
          useCachedConfig true
        }
    
      let standardDockConfig = 
        config {
          content (

              horizontal 23.0 [
                
                vertical 14.0 [
                element {id "render"; title "Main View"; weight 14.0}
                horizontal 0.0[
                element {id "leftCam"; title "HR-Cam / WACL"; weight 0.0}
                element {id "rightCam"; title "WACR"; weight 0.0}
                ]
                
                ]
                element {id "controls"; title "Controls"; weight 9.0}
                
              
              ]

          )
          appName "ViewPlanner"
          useCachedConfig true
        }
          
      //high resolution camera
      let hrcam = 
        let camState = {RoverModel.initial.HighResCam.cam.camera with view = roverinitialCamera.view}
        let camVar = {RoverModel.initial.HighResCam.cam with camera = camState; position = initialRoverPos}
        {RoverModel.initial.HighResCam with cam = camVar}
      
     
      //stereo camera
      let rightV = (forward.Normalized).Cross(box.Center.Normalized)
      let shift = rightV * 0.3
      let positionCamL = initialRoverPos - shift
      let positionCamR = initialRoverPos + shift
      let forwardL = forward - shift
      let forwardR = forward + shift
      let camViewL = CameraView.look positionCamL forwardL.Normalized box.Center.Normalized
      let camViewR = CameraView.look positionCamR forwardR.Normalized box.Center.Normalized
      let stcam =   
        let camStateL = {RoverModel.initial.WACLR.camL.camera with view = camViewL}
        let camVarL = {RoverModel.initial.WACLR.camL with camera = camStateL; position = positionCamL }

        let camStateR = {RoverModel.initial.WACLR.camR.camera with view = camViewR}
        let camVarR = {RoverModel.initial.WACLR.camR with camera = camStateR; position = positionCamR }
       
        {RoverModel.initial.WACLR with camL = camVarL; camR = camVarR }

      

      let initialModel : Model = 
        { 
          cameraState        = camState
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
          rover              = { RoverModel.initial with up = box.Center.Normalized; HighResCam = hrcam; WACLR = stcam; position = initialRoverPos; target = initialRoverTarget; projsphere = {RoverModel.initial.projsphere with position = initialRoverPos}}
          dockConfig         = viewPlanDockConfig//standardDockConfig    
          standardConfig     = standardDockConfig
          viewPlanModeConfig = viewPlanDockConfig
          region             = None
          roiBboxFull        = false
          roverPlacement     = 
            {
            active = false
            counter = 0
            counterToMax = 0
            max = 4
            }
          //menuOptions        = HMap.ofList [SaveCameraState, "Save camera state"; SaveRoverState, "Save plane state"; RoverPlacementMode, "rover placement mode"; StandardMode, "standard mode"]
          modeOptions        = HMap.ofList [StandardMode, "standard mode"; RoverPlacementMode, "rover placement mode"; SampleMode, "sample mode" ; ViewPlanMode, "view plan mode"] 
          currentModeOption  = Some StandardMode
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }


