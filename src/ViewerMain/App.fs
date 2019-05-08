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

  let update (model : Model) (msg : Message) =   
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
            let updatedPicking = PickingApp.update model.picking (PickingAction.AddBrush)
            { model with picking = updatedPicking 
                         axis    = AxisFunctions.calcDebuggingPosition model.picking.intersectionPoints model.axis}
          | _ -> model
      | PickingAction msg -> 
        { model with picking = PickingApp.update model.picking msg }
        ////IntersectionController.intersect model sceneHit box
        //failwith "panike"
        //model 
      | UpdateDockConfig cfg ->
        { model with dockConfig = cfg }
      | _ -> model
                    
  let view (m : MModel) =
                                                 
      let box = 
        m.patchHierarchies
          |> List.map(fun x -> x.tree |> QTree.getRoot) 
          |> List.map(fun x -> x.info.LocalBoundingBox)
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

      let scene = 
        [
          opcs
          axis
          PickingApp.view m.picking
        ] |> Sg.ofList

      let renderControl =
       FreeFlyController.controlledControl m.cameraState Camera (Frustum.perspective 60.0 0.01 1000.0 1.0 |> Mod.constant) 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "false";       // optional, default is false
           attribute "useMapping" "true"
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
           onKeyDown (Message.KeyDown)
           onKeyUp (Message.KeyUp)
           //onBlur (fun _ -> Camera FreeFlyController.Message.Blur)
         ]) 
         (scene) 
            
      let frustum = Frustum.perspective 60.0 0.1 50000.0 1.0 |> Mod.constant          
        
      let cam = Mod.map2 Camera.create m.cameraState.view frustum 

      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl]
          )
        | Some "controls" -> 
          require Html.semui (
            body [style "width: 100%; height:100%; background: transparent";] [
              div[style "color:white; margin: 5px 15px 5px 5px"][
                h3[][text "NIOBE"]
                p[][text "Hold Ctrl-Left to add Point"]
                p[][text "Press Enter to close Polygon"]
                p[][text "Debug Volume "; Html.SemUi.toggleBox m.picking.debugShadowVolume PickingAction.ShowDebugVis |> UI.map PickingAction]
                p[][text "Alpha"; Numeric.numericField (PickingAction.SetAlpha >> Seq.singleton) AttributeMap.empty m.picking.alpha Slider |> UI.map PickingAction]
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
                      
      let up = V3d.OOI // if true then (box.Center.Normalized) else V3d.OOI

      let restoreCamState : CameraControllerState =
        if File.Exists ".\camstate" then          
          Log.line "[App] restoring camstate"
          let csLight : CameraStateLean = Serialization.loadAs ".\camstate"
          { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
        else 
          { FreeFlyController.initial with view = CameraView.lookAt (box.Max) box.Center up; }                    

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
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }
       