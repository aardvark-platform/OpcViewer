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

open OpcSelectionViewer.Picking

module App =   
  open Aardvark.Application
  open Aardvark.Base.DynamicLinkerTypes  
  open Aardvark.UI
  open System.Numerics

      
     
  
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

 
  let totalTime = System.Diagnostics.Stopwatch.StartNew()
  
  module ThreadPool =
    let unionMany xs = List.fold ThreadPool.union ThreadPool.empty xs

  let threads (m : Model) = 
    let rec time() =
        proclist {
            do! Proc.Sleep 10
            yield Tick (float System.DateTime.Now.Ticks)// totalTime.Elapsed.TotalSeconds
            yield! time()
        }
    // handling of continous camera animations (camera controller)
   // let cameraAnimations = FreeFlyController.threads m.cameraState |> ThreadPool.map CameraMessage
       

    // handling of continous animations
    let animations = 
       //if shouldAnimate m then
            ThreadPool.add "timer" (time()) ThreadPool.empty
        //else
        //    ThreadPool.empty

    animations

  let camera_return_jump_animation (model : Model) (t : float) = 
    if model.cameraAnimEndTime > t then 
        let durationTicks = TimeSpan.FromSeconds 2.0 
        let remaingTicks = model.cameraAnimEndTime - t
        let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

        let cam_to_target = model.targetPosition - model.originalCamPos

        let cam = 
            CameraView(model.cameraState.view.Sky, model.originalCamPos + cam_to_target * percent, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

        let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

        { model with cameraState = newCamState }
     
    elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
        let duration = TimeSpan.FromSeconds 0.4
        let total = DateTime.Now.AddTicks (duration.Ticks)
        { model with camJumpAnimRunning = false; camViewAnimRunning = true; cameraAnimEndTime = float total.Ticks }     
    else 
        model

  let camera_jump_animation (model : Model) (t : float) = 
    if model.cameraAnimEndTime > t then 
        let durationTicks = TimeSpan.FromSeconds 2.0 
        let remaingTicks = model.cameraAnimEndTime - t
        let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

        let target_to_cam = model.originalCamPos - model.targetPosition
        let offset = target_to_cam/10.0
        let target_pos = model.targetPosition + offset
        let cam_to_target = target_pos - model.originalCamPos

        let cam = 
            CameraView(model.cameraState.view.Sky, model.originalCamPos + cam_to_target * percent, model.cameraState.view.Forward, model.cameraState.view.Up, model.cameraState.view.Right)

        let newCamState : CameraControllerState =
                      { model.cameraState with view = cam }

        { model with cameraState = newCamState }
     
    elif model.camJumpAnimRunning && model.cameraAnimEndTime < t then
        { model with camJumpAnimRunning = false; camCompAnimRunning = false }     
    else 
        model

  let ortho_persp_animation (model : Model) (t: float) = 
      if model.cameraAnimEndTime > t then
         if model.perspectiveView then
            let durationTicks = TimeSpan.FromSeconds 0.4 
            let remaingTicks = model.cameraAnimEndTime - t

            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            { model with pers_to_ortho = percent }

         else
            let durationTicks = TimeSpan.FromSeconds 0.4 
            let remaingTicks = model.cameraAnimEndTime - t

            let percent = 1.0 - (remaingTicks / float durationTicks.Ticks)

            { model with pers_to_ortho = 1.0 - percent }


      elif model.camViewAnimRunning && model.cameraAnimEndTime < t then
        if model.perspectiveView then
            { model with perspectiveView = false; pers_to_ortho = 1.0; camViewAnimRunning = false }
        else
            let duration = TimeSpan.FromSeconds 2.0
            let total = DateTime.Now.AddTicks (duration.Ticks)
            { model with perspectiveView = true; pers_to_ortho = 0.0; camViewAnimRunning = false; camJumpAnimRunning = model.camCompAnimRunning; camRetAnimRunning = false; cameraAnimEndTime = float total.Ticks; }
      else
        model
  


  let rec update (model : Model) (msg : Message) =   
    match msg with
      | Camera m when model.pickingActive = false -> 
        if model.pers_to_ortho = 0.0 then
            { model with cameraState = FreeFlyController.update model.cameraState m; }
        else
            model       
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
            { model with picking = PickingApp.update model.picking (PickingAction.ClearPoints) }
          | Keys.Back ->
            { model with picking = PickingApp.update model.picking (PickingAction.RemoveLastPoint) }
          | Keys.PageUp ->             
            { model with cameraState = model.cameraState |>  updateFreeFlyConfig +0.5 }
          | Keys.PageDown ->             
            { model with cameraState = model.cameraState |>  updateFreeFlyConfig -0.5 }
          | Keys.Space ->    
            Log.line "[App] saving camstate"
            model.cameraState.view |> toCameraStateLean |> Serialization.save ".\camstate" |> ignore
            model
          | Keys.Enter ->
            let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
            let updatedPicking = PickingApp.update model.picking (PickingAction.AddBrush pointsOnAxisFunc)
            
            let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
            { model with picking = updatedPicking; axis = axis }
          | Keys.T ->
            let pointsOnAxisFunc = AxisFunctions.pointsOnAxis model.axis
            let updatedPicking = PickingApp.update model.picking (PickingAction.AddTestBrushes pointsOnAxisFunc)
            
            let axis = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis
            { model with picking = updatedPicking; axis = axis }
          | Keys.B ->
            update model Message.AnimateCameraComplete
          | Keys.N ->
            update model Message.AnimateCameraReturn
          | Keys.J ->
             update model Message.AnimateCameraJump
          | Keys.O ->
            // let target_pos = V3d(-2487194.25671355, 2289440.21734682, -276259.808310618)
            // let cam_pos = model.cameraState.view.Location
            // let cam_to_target = cam_pos - target_pos
            // let offset = cam_to_target/10.0

            // let cam = CameraView(model.cameraState.view.Sky, target_pos + offset, -target_pos, model.cameraState.view.Up, model.cameraState.view.Right)
             
            // { model with cameraState = FreeFlyController.update model.cameraState (cam |> (fun b -> FreeFlyController.Message.JumpTo b)); animation_running = true }
             update model Message.AnimateCameraViewSwitch

          | _ -> model
      | Message.Down(button,pos) ->   
         match button with
          | MouseButtons.Right -> 
                let model = { model with dragStart = pos }
                { model with zoom = true }
                
          | MouseButtons.Left -> 
                let model = { model with dragStart = pos}
                { model with pan = true } 
    
          | _ -> model
      | Message.Up button ->        
         match button with
          | MouseButtons.Right -> 
                { model with zoom = false }

          | MouseButtons.Left -> 
                { model with pan = false }
               
          | _ -> model
      | Message.Zoom pos ->
        if model.pers_to_ortho = 1.0 then
            if pos.Y > model.dragStart.Y then
               let newv= model.zoomFactor + 2.0 
               { model with zoomFactor = abs newv }
            elif pos.Y < model.dragStart.Y then
               let newv= model.zoomFactor - 2.0 
               { model with zoomFactor = abs newv }
            else 
                model
        else
            model
      | Message.Pan pos ->
         if model.pers_to_ortho = 1.0 then
             let direction = pos - model.dragStart
             let newv= model.panVector + direction/10
             { model with panVector = newv; prevPanVec = pos }
         else
            model   
      | Message.AnimateCameraViewSwitch ->
        let duration = TimeSpan.FromSeconds 0.4
        let total = DateTime.Now.AddTicks (duration.Ticks)
        { model with cameraAnimEndTime = float total.Ticks; camViewAnimRunning = true}
      | Message.AnimateCameraJump ->
        let duration = TimeSpan.FromSeconds 2.0
        let total = DateTime.Now.AddTicks (duration.Ticks)
        let target_pos = V3d(-2487194.25671355, 2289440.21734682, -276259.808310618)

        { model with cameraAnimEndTime = float total.Ticks; camJumpAnimRunning = true; targetPosition = target_pos; originalCamPos = model.cameraState.view.Location }
      | Message.AnimateCameraComplete ->
        let duration = TimeSpan.FromSeconds 0.4
        let total = DateTime.Now.AddTicks (duration.Ticks)
        let target_pos = V3d(-2487194.25671355, 2289440.21734682, -276259.808310618)
        { model with cameraAnimEndTime = float total.Ticks; camCompAnimRunning = true; camViewAnimRunning = true; camJumpAnimRunning = false; targetPosition = target_pos; originalCamPos = model.cameraState.view.Location }
      | Message.AnimateCameraReturn ->
        let duration = TimeSpan.FromSeconds 2.0
        let total = DateTime.Now.AddTicks (duration.Ticks)
        { model with cameraAnimEndTime = float total.Ticks; camRetAnimRunning = true; camViewAnimRunning = false; camJumpAnimRunning = true; targetPosition = model.originalCamPos; originalCamPos = model.cameraState.view.Location }
      | Message.Tick t ->       
        if not model.camCompAnimRunning && not model.camRetAnimRunning && model.camViewAnimRunning then
            ortho_persp_animation model t
        elif not model.camCompAnimRunning && not model.camRetAnimRunning && model.camJumpAnimRunning then
            camera_jump_animation model t
        elif model.camCompAnimRunning && model.camViewAnimRunning then
            ortho_persp_animation model t
        elif model.camCompAnimRunning && model.camJumpAnimRunning then
            camera_jump_animation model t
        elif model.camRetAnimRunning && model.camJumpAnimRunning then
            camera_return_jump_animation model t
        elif model.camRetAnimRunning && model.camViewAnimRunning then
            ortho_persp_animation model t
        else 
            model
      | PickingAction msg -> 
        let pickingModel =
          match msg with
          | HitSurface (a,b,_) -> 
            match model.axis with
            | Some axis -> 
              let axisNearstFunc = fun p -> (fst (AxisFunctions.getNearestPointOnAxis' p axis)).position
              PickingApp.update model.picking (HitSurface (a,b, axisNearstFunc))
            | None -> PickingApp.update model.picking msg
          | _ -> PickingApp.update model.picking msg
        { model with picking = pickingModel }

        ////IntersectionController.intersect model sceneHit box
        //failwith "panike"
        //model 
      | UpdateDockConfig cfg ->
        { model with dockConfig = cfg }
      | SetT t -> 
        { model with pers_to_ortho = t}
      
      | _ -> model
                    
  let view (m : MModel) =
        
                               
      let box = 
        m.patchHierarchies
          |> List.map(fun x -> x.tree |> QTree.getRoot) 
          |> List.map(fun x -> x.info.GlobalBoundingBox)
          |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid
      
      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg m info)
          |> Sg.set
          |> Sg.effect [ 
            toEffect Shader.stableTrafo
            toEffect DefaultSurfaces.diffuseTexture       
            ]

      let axis = 
        m |> Sg.axisSgs

      let shiftToOriginVec = -box.Center

      //let projTrafo : IMod<Trafo3d> = 
      //  m.zoomFactor
      //  |> Mod.map(fun zf -> 
      //        let min = -V3d(10.0,10.0,10.0) * zf
      //        let max = V3d(10.0,10.0,10.0) * zf
      //        Frustum.projTrafo (Frustum.ortho (Box3d(min, max)))  
      //      )

      let panTrafo : IMod<Trafo3d> = 
        m.panVector
        |> Mod.map(fun pv ->        
              Trafo3d.Translation(V3d(double pv.X, 0.0, double pv.Y))   
            )
      
      let projTrafo =
        adaptive {
            
            let! zf = m.zoomFactor
            let! t = m.pers_to_ortho
            let min = -V3d(10.0,10.0,1000.0) * zf
            let max = V3d(10.0,10.0,1000.0) * zf 
            let o=  (Frustum.projTrafo (Frustum.ortho (Box3d(min, max))))


            let p=  (Frustum.projTrafo (Frustum.perspective 60.0 0.01 10000.0 1.0 ))


            let t = t ** (1.0/50.0)
            let trafo = 
                Trafo3d (
                    o.Forward * t + (1.0-t) * p.Forward,
                    o.Backward * t + (1.0 - t) * p.Backward
                )
            return trafo           
        }

    
      
      let scene = 
        [
          opcs
          axis
          PickingApp.view m.picking
        ] |> Sg.ofList
          //|> Sg.trafo (Mod.constant(Trafo3d.Translation(shiftToOriginVec)))
          |> Sg.trafo (panTrafo)
          //|> Sg.trafo (Mod.constant(Trafo3d.Rotation(1.55,1.0,0.0)))
          //|> Sg.viewTrafo (Mod.constant Trafo3d.Identity)
          |> Sg.projTrafo projTrafo
          //|> if true then Sg.projTrafo projTrafo else Sg.trafo (Mod.constant(Trafo3d.Translation(V3d.Zero)))
          //|> Sg.cullMode (Mod.constant CullMode.None)
          //|> Sg.depthTest (Mod.constant DepthTestMode.Always)
          //|> Sg.blendMode (Mod.constant BlendMode.None) 

      let renderControl (state : MModel) (f : Message -> 'msg)=
        
        FreeFlyController.controlledControl m.cameraState Camera (Frustum.perspective 60.0 0.01 10000.0 1.0 |> Mod.constant)     
          
         (AttributeMap.ofListCond [ 
           always (style "width: 100%; height:100%"; )
           always (attribute "showFPS" "true"; )      // optional, default is false
           always (attribute "useMapping" "true")
           always (attribute "data-renderalways" "false")
           always (attribute "data-samples" "4")
           always (onKeyDown (Message.KeyDown))
           always (onKeyUp (Message.KeyUp))
           always (onMouseDown (fun b p -> f (Message.Down(b,p))))
           always (onMouseUp (fun b p -> f (Message.Up(b))))
           onlyWhen state.zoom (onMouseMove (Message.Zoom >> f))
           onlyWhen state.pan (onMouseMove (Message.Pan >> f))
           
           //onBlur (fun _ -> Camera FreeFlyController.Message.Blur)
         ]) 
         (scene) 
      //let frustum = Frustum.perspective 60.0 0.1 50000.0 1.0 |> Mod.constant          
      //let cam = Mod.map2 Camera.create m.cameraState.view frustum 

      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl m id]
          )
        | Some "controls" -> 
          require Html.semui (
            body [style "width: 100%; height:100%; background: transparent";] [
              div[style "color:white; margin: 5px 15px 5px 5px"][
                p[][div[][text "t: "; slider { min = 0.0; max = 1.0; step = 0.01 } [clazz "ui inverted blue slider"] m.pers_to_ortho Message.SetT]]
                h3[][text "NIOBE"]
                p[][text "Hold Ctrl-Left to add Point"]
                p[][text "Press Enter to close Polygon"]
                p[][div[][text "VolumeGeneration: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.picking.volumeGenerationOptions |> AMap.map (fun k v -> text v)) m.picking.volumeGeneration PickingAction.SetVolumeGeneration ]] |> UI.map PickingAction
                p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.debugShadowVolume PickingAction.ShowDebugVis "Show Debug Vis"] |> UI.map PickingAction
                p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.useGrouping PickingAction.UseGrouping "Use Grouping"] |> UI.map PickingAction
                p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.showOutline PickingAction.ShowOutline "Show Outline"] |> UI.map PickingAction
                p[][checkbox [clazz "ui inverted toggle checkbox"] m.picking.showDetailOutline PickingAction.ShowOutlineDetail "Show Outline Detail"] |> UI.map PickingAction
                //p[][checkbox [clazz "ui inverted toggle checkbox"] m.ortho Message.KeyUp "Otho Camera"] 
                p[][div[][text "Alpha: "; slider { min = 0.0; max = 1.0; step = 0.05 } [clazz "ui inverted blue slider"] m.picking.alpha PickingAction.SetAlpha]] |> UI.map PickingAction
                p[][div[][text "Extrusion: "; slider { min = 0.05; max = 500.0; step = 5.0 } [clazz "ui inverted blue slider"] m.picking.extrusionOffset PickingAction.SetExtrusionOffset]] |> UI.map PickingAction
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

  let app dir axisFile =
      Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

      let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton

      let axis = 
        axisFile |> Option.map(fun fileName -> AxisFunctions.loadAxis fileName)
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
                      
      let up = V3d.OOI 
      let sky = box.Center.Normalized
      let r = Trafo3d.RotateInto(V3d.OOI, sky)
      //let height = box.Max.Z - box.Min.Z   
      let camPos = V3d(box.Center.X,box.Center.Y,box.Center.Z)+r.Forward.TransformPos(V3d(0.0,0.0,10.0*2.0*100.0))

      let restoreCamState : CameraControllerState =
        if File.Exists ".\camstate" then          
          Log.line "[App] restoring camstate"
          let csLight : CameraStateLean = Serialization.loadAs ".\camstate"
          { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
        else 
          { FreeFlyController.initial with view = CameraView.lookAt camPos box.Center up; } 

      let camState = restoreCamState

      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 0.0}
      let camState = camState |> Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

      let initialDockConfig = 
        config {
          content (
              horizontal 10.0 [
                  element { id "render"; title "Render View"; weight 7.0 }
                  element { id "controls"; title "Controls"; weight 3.0 }                         
              ]
          )
          appName "OpcSelectionViewer"
          useCachedConfig true
        }
            
      let initialModel : Model = 
        { 
          cameraState        = camState
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies          
          axis               = axis
          
          threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
          boxes              = List.empty //kdTrees |> HMap.toList |> List.map fst
      
          pickingActive      = false
          opcInfos           = opcInfos
          picking            = { PickingModel.initial with pickingInfos = opcInfos }
          dockConfig         = initialDockConfig  
          zoomFactor         = 100.0
          dragStart          = V2i.Zero
          movePos            = V2i.Zero
          zoom               = false
          panVector          = V2i.Zero
          prevPanVec         = V2i.Zero
          pan                = false
          pers_to_ortho      = 1.0
          camViewAnimRunning = false
          cameraAnimEndTime  = 0.0
          targetPosition     = V3d.Zero        
          perspectiveView    = false  
          camJumpAnimRunning = false
          originalCamPos     = V3d.Zero
          camCompAnimRunning = false
          camRetAnimRunning  = false
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = threads 
          unpersist = Unpersist.instance<Model, MModel>
      }
     