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
                let picked = model.pickingModel.pickedPointOnPlane
                let roverModel = 
                    match picked with
                        | Some p -> 
                            let forward = p-model.rover.position
                            let cam = CameraView.look model.rover.position forward.Normalized model.rover.up
                            { model.rover with target = p; camera = { model.rover.camera with view = cam }}
                        
                        | None -> model.rover
                { model with rover = roverModel}

            | Keys.R -> //if R is pressed then picked point on plane is new rover position
                let picked = model.pickingModel.pickedPointOnPlane
                let n = model.rover.up
                let roverModel = 
                    match picked with
                        | Some p -> { model.rover with position = (p+n) }
                        | None -> model.rover
                { model with rover = roverModel}



            | Keys.Enter ->
                let pointsOnAxisFunc = OpcSelectionViewer.AxisFunctions.pointsOnAxis None
                let updatedPicking = PickingApp.update model.pickingModel (PickingAction.AddBrush pointsOnAxisFunc)
            
                { model with pickingModel = updatedPicking}
            | Keys.T ->
                let pointsOnAxisFunc = OpcSelectionViewer.AxisFunctions.pointsOnAxis None
                let updatedPicking = PickingApp.update model.pickingModel (PickingAction.AddTestBrushes pointsOnAxisFunc)
            
                { model with pickingModel = updatedPicking; }
            | _ -> model

        | PickingAction msg -> 
            let pickingModel =
                match msg with
                    | HitSurface (a,b,_) -> 
                        let axisNearstFunc = fun p -> p
                        PickingApp.update model.pickingModel (HitSurface (a,b, axisNearstFunc))

                    | PickPointOnPlane p ->
                       PickingApp.update model.pickingModel (PickPointOnPlane p)
                       
                    | _ -> PickingApp.update model.pickingModel msg
            { model with pickingModel = pickingModel }

        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }

        | RoverAction msg -> 
            match msg with
                  | ChangePosition pos -> 
                        let r = RoverApp.update model.rover (ChangePosition pos)
                        {model with rover = r}
                  | ChangePan p -> 
                    let r = RoverApp.update model.rover (ChangePan p)
                    {model with rover = r}
                    
                  | ChangeTilt t -> 
                    let r = RoverApp.update model.rover (ChangeTilt t)
                    {model with rover = r}
                
                  | MoveToRegion re ->
                    let r = RoverApp.update model.rover (MoveToRegion re)
                    {model with rover = r}


        | _ -> model
    
    //---

    //---VIEW
    let view (m : MModel) =
                                                 
      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg (Mod.constant None) m.pickingActive m.cameraState.view info)
          |> Sg.set
          //|> Sg.effect [ 
          //  toEffect Shader.stableTrafo
          //  toEffect DefaultSurfaces.diffuseTexture       
          //  ]
        
        
      let rovertrafo = m.rover.position |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let rov = 
           Sg.sphere 5 (Mod.constant C4b.Cyan) (Mod.constant 0.2)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo rovertrafo


      let targettrafo = m.rover.target |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let target = 
           Sg.sphere 5 (Mod.constant C4b.DarkMagenta) (Mod.constant 0.2)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo targettrafo


      let vp = (RoverModel.getViewProj m.rover.camera.view m.rover.frustum)    
      
      let frustumBox =
        Sg.wireBox' C4b.White (Box3d(V3d.NNN,V3d.III))
        |> Sg.noEvents
        |> Sg.trafo (vp |> Mod.map ( fun vp -> vp.Inverse))
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.vertexColor
        }
       
      //highlights the area of the model which is inside the rover's view frustum
      let shading = 
        opcs
            |> Sg.cullMode (Mod.constant CullMode.Back)
            |> Sg.depthTest (Mod.constant DepthTestMode.Less)
            |> Sg.uniform "FootprintMVP" vp
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! Shading.vert
                do! Shading.frag
            }


      let myPlane = 
        m.planePoints
            |> Mod.map (fun n ->
                match n with
                    | None -> Sg.empty
                    | Some points -> 
                        points 
                            |> AList.toMod
                            |> Mod.map (fun p ->
                                p
                                    |> PList.toSeq
                                    |> Sg.planeFit
                                    |> fun t ->
                                         let box = Aardvark.SceneGraph.SgPrimitives.Sg.box' C4b.Cyan (Box3d(V3d.NNN, V3d.III))
                                         let scaleT = Trafo3d.Scale(10.0, 20.0, 0.2)
                                         let sum = p.Sum()
                                         let c = p |> PList.count
                                         let average = sum / (float c)
                                         let pos = V3d(average.X, average.Y-0.5, average.Z)
                            
                                         let trafo = scaleT * Trafo3d.RotateInto(V3d.OOI, t.Normal) * Trafo3d.Translation(pos)
                                         box
                                            |> Aardvark.SceneGraph.``Sg Picking Extensions``.Sg.requirePicking
                                            |> Sg.noEvents
                                            |> Sg.withEvents [
                                                SceneEventKind.DoubleClick, (fun sh -> 
                                                true, Seq.ofList [(PickingAction.PickPointOnPlane (Some sh.globalPosition))]
                                                                            )
                                                              ]
                                            |> Sg.trafo (Mod.constant(trafo)) 
                                    |> Sg.effect [
                                            toEffect DefaultSurfaces.stableTrafo
                                            toEffect (DefaultSurfaces.constantColor C4f.DarkRed)
                                                 ]  
                                           )
                            |> Sg.dynamic
                      )



      let drawPlane = myPlane |> Sg.dynamic

      let fullScene = 
        [
          PickingApp.view m.pickingModel
          drawPlane
          rov
          target
          frustumBox
          shading
        ] |> Sg.ofList
    
      let roverCamScene = 
       [
          PickingApp.view m.pickingModel
          drawPlane
          target
          //frustumBox
          shading
        ] |> Sg.ofList
        

      let textOverlays (cv : IMod<CameraView>) = 
        div [js "oncontextmenu" "event.preventDefault();"] [ 
           let style' = "color: white; font-family:Consolas;"
    
           yield div [clazz "ui"; style "position: absolute; top: 15px; left: 15px; float:left" ] [          
              yield table [] [
                tr[][
                    td[style style'][Incremental.text(cv |> Mod.map(fun x -> x.Location.ToString("0.00")))]
                ]
              ]
           ]
        ]

       
      let roverCamControl = 
       
        FreeFlyController.controlledControl  m.rover.camera Camera m.rover.frustum 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "false";      
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
         ]) 
            
         (roverCamScene |> Sg.map PickingAction)
      




      let renderControl =
       FreeFlyController.controlledControl m.cameraState Camera (Frustum.perspective 60.0 0.01 1000.0 1.0 |> Mod.constant) 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "true";       // optional, default is false
           attribute "useMapping" "true"
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
           onKeyDown (Action.KeyDown)
           onKeyUp (Action.KeyUp)
         ]) 
         (fullScene |> Sg.map PickingAction) 
            
   
      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl; textOverlays (m.cameraState.view)]
          )
        
        | Some "roverCam" ->
            require Html.semui (
              div [clazz "ui"; style "background: #1B1C1E"] [roverCamControl]
          )

        | Some "controls" -> 
          require Html.semui (
            body [style "width: 100%; height:100%; background: transparent";] [
              div[style "color:white; margin: 5px 15px 5px 5px"][
                h3[][text "ROVER CONTROL"]
                p[][text "Press R to place rover at picked point"]
                p[][text "Press L to select picked point as rover target"]
                p[][Incremental.text (m.rover.position |> Mod.map (fun f -> f.ToString())) ]
                p[][div[][Incremental.text (m.rover.pan.current |>Mod.map (fun f -> "Panning - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.pan.current RoverAction.ChangePan]] |> UI.map RoverAction 
                p[][div[][Incremental.text (m.rover.tilt.current |> Mod.map (fun f -> "Tilting - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.tilt.current RoverAction.ChangeTilt]] |> UI.map RoverAction  
                button [onClick (fun _ -> RoverAction.MoveToRegion (m.pickingModel.intersectionPoints |> AList.toPList |> PList.last))] [text "Move to region"] |> UI.map RoverAction





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


      let roverinitialCamera = {
      
        FreeFlyController.initial with view = CameraView.lookAt box.Max box.Center box.Center.Normalized
      }


      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> OpcSelectionViewer.Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

      let initialDockConfig = 
        config {
          content (
              vertical 13.0 [
                  element { id "render"; title "Render View"; weight 7.0 }
                  horizontal 6.0 [
                  element { id "roverCam"; title "Rover view"; weight 3.0 }
                  element { id "controls"; title "Controls"; weight 3.0 }
                  ]
                  
              ]

          )
          appName "ViewPlanner"
          useCachedConfig true
        }
            
      let initialModel : Model = 
        { 
          cameraState        = camState
          //secondCam          = camState
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies          
          
          threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
          boxes              = List.empty 
      
          pickingActive      = false
          opcInfos           = opcInfos
          pickingModel       = { PickingModel.initial with pickingInfos = opcInfos }
          planePoints        = setPlaneForPicking
          rover              = { RoverModel.initial with up = box.Center.Normalized; camera = roverinitialCamera; position = box.Center}
          dockConfig         = initialDockConfig            
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }


