namespace Shadows

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
open FShade.Imperative
open FShade.``Reflection Helpers``
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes
open Rabbyte.Drawing
open Rabbyte.Annotation

module App =   
  open Aardvark.Application
  open Aardvark.VRVis.Opc
  open Aardvark.Application.Slim
  
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

  let updateCamSpeed (c : CameraControllerState) (s: float) =
    let cState = CameraControllerState.Lens.freeFlyConfig.Set(c, {c.freeFlyConfig with moveSensitivity = s})
    cState
        
    
  let rec update (model : Model) (msg : Message) =   
    match msg with
      | Camera m when model.pickingActive = false -> 
        { model with cameraState = FreeFlyController.update model.cameraState m; }
      | Message.MoveSensitivity s when model.pickingActive = false ->
        { model with cameraState = updateCamSpeed model.cameraState s } 
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
            model.cameraState.view |> toCameraStateLean |> OpcSelectionViewer.Serialization.save ".\camstate" |> ignore
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
                |> HMap.tryFind model.boundingBox   // WRONG use local boundingbox
                |> Option.bind (fun kk -> 
                    OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some kk.kdTree) fray 
                    |> Option.map (fun closest ->
                        fray.Ray.GetPointOnRay closest
                    )
                )

            let updatePickM = PickingApp.update model.picking (HitSurface (a,b))
            let lastPick = updatePickM.intersectionPoints |> PList.tryFirst
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
            {model with opcAttributes = SurfaceAttributes.update model.opcAttributes msg }
      | DrawingAction msg -> 
            { model with drawing = DrawingApp.update model.drawing msg }
      | AnnotationAction msg -> 
            { model with annotations = AnnotationApp.update model.annotations msg }
      | _ -> model


  let sg (model : MModel)
         (opc   : ISg<PickingAction>) 
         (runtime : Aardvark.Rendering.GL.Runtime) =
    let shadowMapSize = Mod.init (V2i(4096, 4096))

    let shadowCam = CameraView.lookAt (V3d.III * 2.0) V3d.Zero V3d.OOI
    let shadowProj = Frustum.perspective 60.0 0.1 100.0 1.0
    let camView = model.cameraState.view
    
    //let initialView = CameraView.lookAt (V3d(9.3, 9.9, 8.6)) V3d.Zero V3d.OOI
    //let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    // let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 1000.0 (float s.X / float s.Y))

    // Sg.projTrafo (Frustum.ortho (Box3d.Unit) |> Frustum.projTrafo |> Mod.constant)
    let proj = Frustum.ortho (Box3d.Unit) |> Mod.constant

    //let angle = Mod.init 0.0
    let rotation =
        controller {
            let! dt = differentiate Mod.time
            return fun f -> f + dt.TotalSeconds * 0.6
        }
  
    let angle = AFun.integrate rotation 0.0
    let lightSpaceView =
        angle |> Mod.map (fun angle -> Trafo3d.RotationZ(angle) * (shadowCam |> CameraView.viewTrafo))
    let lightSpaceViewProjTrafo = lightSpaceView |> Mod.map (fun view -> view * (shadowProj |> Frustum.projTrafo))
    let lightPos = lightSpaceView |> Mod.map (fun t -> t.GetViewPosition())


    let eff = Effect.ofFunction Precision.Shader.trafo
    Helpers.printShader eff
    

    let sceneSg (fragmentShader : list<FShadeEffect>) =
      opc
        |> Sg.cullMode (Mod.constant CullMode.None)
        |> Sg.effect ( (Precision.Shader.trafo |> toEffect) :: fragmentShader )
        |> Sg.uniform "LightViewMatrix" lightSpaceViewProjTrafo
        |> Sg.trafo ( Trafo3d.Translation(V3d(0.0,0.0,0.3)) |> Mod.constant )



    let signature = 
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]
 
    let shadowDepth =
        sceneSg [ DefaultSurfaces.vertexColor |> toEffect ]
            |> Sg.viewTrafo lightSpaceView
            |> Sg.projTrafo (shadowProj |> Frustum.projTrafo |> Mod.constant)
            |> Sg.compile runtime signature   
            |> RenderTask.renderToDepth shadowMapSize

    sceneSg [ Precision.Shader.shadowShader |> toEffect; Precision.Shader.lighting |> toEffect ]
        |> Sg.uniform "lightLocation" lightPos
        |> Sg.texture DefaultSemantic.DiffuseColorTexture shadowDepth
        |> Sg.viewTrafo (camView |> Mod.map CameraView.viewTrafo)
        |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)

    


                    
  let view (runtime : Aardvark.Rendering.GL.Runtime) (m : MModel) =
                                             
      let box = 
        m.patchHierarchies
          |> List.map(fun x -> x.tree |> QTree.getRoot) 
          |> List.map(fun x -> x.info.LocalBoundingBox)
          |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid
      
      let tmpOpc = m.opcInfos

      let opcs = 
        tmpOpc
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg m.opcAttributes.selectedScalar m.pickingActive m.cameraState.view info)
          |> Sg.set
          |> Sg.uniform "lightLocation" m.lightPos
          //|> Sg.uniform "LightDir" (Mod.constant (-1.0 * V3d.OOI))
          |> Sg.effect [
            TestShader.simpleOpcColourShader
            ]
          //|> Sg.effect [ 
          //  toEffect Shader.stableTrafo
          //  toEffect DefaultSurfaces.diffuseTexture
          //  ]

      let near = m.mainFrustum |> Mod.map(fun x -> x.near)
      let far = m.mainFrustum |> Mod.map(fun x -> x.far)


      let eff = Effect.ofFunction Precision.Shader.trafo
      Helpers.printShader eff
      let scene = opcs //sg m opcs runtime
        //[
        //    opcs
        //]
        //|> Sg.ofList

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
                { kind = Stylesheet; name = "semantic.css"; url = "semantic.css" }
                { kind = Script; name = "semantic.js"; url = "semantic.js" }
                { kind = Script; name = "essential"; url = "essentialstuff.js" }
                { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
                { kind = Script; name = "spectrum.js";  url = "spectrum.js" }
                { kind = Stylesheet; name = "spectrum.css";  url = "spectrum.css"}
          ]

      let addGui =
        let sliderGui =
          slider { min = 0.0; max = 10.0; step = 0.1 } [clazz "ui inverted green slider"] m.cameraState.freeFlyConfig.moveSensitivity Message.MoveSensitivity
        let style' = "color: white; font-family:Consolas; min-width:10em"
        table [clazz "item"] [
                    tr[][
                        td[style style'][text "CameraSpeed:    "]
                        td[style style'][sliderGui]
                    ]
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
              ]
              div[style "color:white; margin: 5px 15px 5px 5px"][
                DrawingApp.viewGui m.drawing |> UI.map DrawingAction
              ]
              div[style "color:white; margin: 5px 15px 5px 5px"][
                addGui
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
              onLayoutChanged UpdateDockConfig ]
        )

  let app (debug : bool) (glApp : OpenGlApplication) (dir : string) 
          (axisFile : option<string>) (rotate : bool) =

      OpcSelectionViewer.Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

      let runtime = glApp.Runtime

      let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton

      let axis = 
        axisFile |> Option.map(fun fileName -> OpcSelectionViewer.AxisFunctions.loadAxis fileName)
                 |> Option.defaultValue None

      let patchHierarchies =
        [ 
          for h in phDirs do
            yield PatchHierarchy.load OpcSelectionViewer.Serialization.binarySerializer.Pickle OpcSelectionViewer.Serialization.binarySerializer.UnPickle (h |> OpcPaths)
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

            let kdtrees = 
                try 
                    KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ OpcSelectionViewer.Serialization.binarySerializer
                with e -> 
                    Log.warn "Could not load kd trees. %A" e
                    HMap.empty

            yield {
              patchHierarchy = h
              kdTree         = Aardvark.VRVis.Opc.KdTrees.expandKdTreePaths h.opcPaths.Opc_DirAbsPath kdtrees
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
          let csLight : CameraStateLean = OpcSelectionViewer.Serialization.loadAs ".\camstate"
          { FreeFlyController.initial with view = csLight |> fromCameraStateLean }
        else 
          { FreeFlyController.initial with view = CameraView.lookAt (box.Max) box.Center up; }  
      
      let lightPos = box.Center + V3d(box.Size.X, box.Size.Y, box.Size.Z)

      let camState = restoreCamState

      let ffConfig = { camState.freeFlyConfig with lookAtMouseSensitivity = 0.004; lookAtDamping = 50.0; moveSensitivity = 4.0}
      let camState = camState |> OpcSelectionViewer.Lenses.set (CameraControllerState.Lens.freeFlyConfig) ffConfig

      let initialDockConfig = 
        config { 
          content (
              horizontal 10.0 [
                  element { id "render"; title "Render View"; weight 7.0 }
                  element { id "controls"; title "Controls"; weight 3.0 } 
                  element { id "falseColors"; title "FalseColors"; weight 3.0 }  
              ]
          )
          appName "OpcSelectionViewer"
          useCachedConfig true
        }
            
      let initialModel : Model = 

        { 
          debug              = debug
          cameraState        = camState
          mainFrustum        = Frustum.perspective 60.0 0.01 1000.0 1.0
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies    
          boundingBox        = box
          lightPos           = lightPos
          axis               = None
          
          threads            = FreeFlyController.threads camState |> ThreadPool.map Camera
          boxes              = List.empty //kdTrees |> HMap.toList |> List.map fst
      
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
          view   = view runtime         
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }
       