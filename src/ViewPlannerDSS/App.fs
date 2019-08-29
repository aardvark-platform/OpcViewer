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
    open Aardvark.Base
    open Aardvark.Base.MultimethodTest
    open DevILSharp
    
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
            let pickingModel, drawingModel =
                match msg with
                    | HitSurface (a,b) -> //,_) -> 
                        let updatePickM = PickingApp.update model.pickingModel (HitSurface (a,b))
                        let lastPick = updatePickM.intersectionPoints |> PList.tryFirst
                        let updatedDrawM =
                            match lastPick with
                            | Some p -> DrawingApp.update model.drawing (DrawingAction.AddPoint (p, None))
                            | None -> model.drawing
                        updatePickM, updatedDrawM
                    | PickPointOnPlane p ->
                        PickingApp.update model.pickingModel (PickPointOnPlane p), model.drawing
                    | _ -> PickingApp.update model.pickingModel msg, model.drawing
            { model with pickingModel = pickingModel; drawing = drawingModel }
        | UpdateDockConfig cfg ->
            { model with dockConfig = cfg }

        | RoverAction msg -> 
            let r = RoverApp.update model.rover msg
            {model with rover = r}
                    
        | SaveConfigs msg ->
            match msg with 
            | Some CameraState -> 
                Log.line "[App] saving camstate"
                model.cameraState.view |> toCameraStateLean |> OpcSelectionViewer.Serialization.save ".\camerastate" |> ignore 
                model
            | Some PlaneState ->
                Log.line "[App] saving plane points"
                model.pickingModel.intersectionPoints |> toPlaneCoords |> OpcSelectionViewer.Serialization.save ".\planestate" |> ignore
                model
            | Some RoverState ->
                let intersect = model.pickingModel.intersectionPoints |> PList.toList
                let r =
                    if intersect |> List.isEmpty then model
                    elif intersect.Length < 2 then model
                    else
                        Log.line "[App] saving rover position and target"
                        let n = model.rover.up
                        let m = (intersect |> List.item (1)) + n
                        let t = intersect |> List.item (0)
                        let adaptedList = [m;t] |> PList.ofList
                        adaptedList |> toRoverCoords |> OpcSelectionViewer.Serialization.save ".\Roverstate" |> ignore
                     
                        let forward = t-m
                        let cam = CameraView.look model.rover.position forward.Normalized model.rover.up

                        let p = PickingApp.update model.pickingModel (PickingAction.ClearPoints)
                        let d = DrawingApp.update model.drawing (DrawingAction.Clear) 

                        let hrcam = {model.rover.HighResCam with cam = { model.rover.HighResCam.cam with position = m; camera = {model.rover.HighResCam.cam.camera with view = cam}} }
                        let r = { model.rover with position = m; target = t; HighResCam = hrcam; projsphere = {model.rover.projsphere with position = m}} 
                        { model with rover = r; pickingModel = p; drawing = d}

                r
            | None -> model



        | _ -> model
    
    //---

    //---VIEW
    let view (m : MModel) =
                                                 
      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> Sg.createSingleOpcSg (Mod.constant None) m.pickingActive m.cameraState.view info)
          |> Sg.set

  
       //projection points on sphere
  
      let ps =
        m.rover.projPoints  //REVIEW
            |> AList.toMod
            |> Mod.map(fun li -> 
                
                let list = li |> PList.toList
                let shift = //REVIEW
                    match list.IsEmpty with
                        | true -> V3d.OOO
                        | false -> list.Head
                
                let shifted = list |> List.map(fun p -> p - shift)
                let arr = shifted |> List.toArray |> Mod.constant
                let shiftV = Trafo3d.Translation(shift)
            
                Sg.draw IndexedGeometryMode.PointList 
                    |> Sg.vertexAttribute DefaultSemantic.Positions arr
                    |> Sg.trafo (Mod.constant shiftV)
                    |> Sg.effect [
                        toEffect DefaultSurfaces.stableTrafo
                        toEffect (DefaultSurfaces.constantColor C4f.Red)
                        Shader.PointSprite.Effect
                        ]
            
                    |> Sg.uniform "PointSize" (Mod.constant 10.0)


            )
      
      let points = ps|> Mod.map(fun cast -> (cast:ISg<PickingAction>)) 


      let transl = m.rover.position |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let rov = 
           Sg.sphere 5 (Mod.constant C4b.Yellow) (Mod.constant 0.1)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo transl
          
      
      //draw all axis
      let shiftVec = Mod.map(fun p -> Trafo3d.Translation(p)) m.rover.position

      //up axis
      let upAxis = 
        alist {
            let! p = m.rover.position
            let! up = m.rover.up
            let upP = p+(up * 2.0)


            //shift the points
            let shiftedPos = p - p
            let shiftedUp = upP - p
            yield shiftedPos
            yield shiftedUp
        }
    
      let upAxisLine = 
        upAxis
            |> AList.toMod
            |> Mod.map (fun m -> 
                let upArr = m |> PList.toArray
                        
                Sg.draw IndexedGeometryMode.LineList
                |> Sg.trafo shiftVec
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant upArr) 
                |> Sg.uniform "Color" (Mod.constant C4b.Blue)
                |> Sg.uniform "LineWidth" (Mod.constant 2.0)
                |> Sg.effect [
                    toEffect DefaultSurfaces.stableTrafo
                    toEffect DefaultSurfaces.thickLine
                    toEffect DefaultSurfaces.sgColor
                        ]
            )
       
      let up = upAxisLine|> Mod.map(fun cast -> (cast:ISg<PickingAction>))
    
      let forwardAxis = 
         alist {
            let! p = m.rover.position
            let! t = m.rover.target
            let tp = t
            //shift points
            let shiftedPos = p - p
            let shiftedF = tp - p
            yield shiftedPos
            yield shiftedF
        }
      
      let forwardAxisLine = 
        forwardAxis
            |> AList.toMod
            |> Mod.map (fun m -> 
                let fArr = m |> PList.toArray
                        
                Sg.draw IndexedGeometryMode.LineList  
                |> Sg.trafo shiftVec
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant fArr) 
                |> Sg.uniform "Color" (Mod.constant C4b.Green)
                |> Sg.uniform "LineWidth" (Mod.constant 2.0)
                |> Sg.effect [
                    toEffect DefaultSurfaces.stableTrafo
                    toEffect DefaultSurfaces.thickLine
                    toEffect DefaultSurfaces.sgColor
                        ]
            )
      
      let forward = forwardAxisLine|> Mod.map(fun cast -> (cast:ISg<PickingAction>))
    
      let rightAxis =
       alist {
            let! p = m.rover.position
            let! up = m.rover.up
            let! target = m.rover.target

            let forw = (target-p).Normalized
            let r = forw.Cross(up)
            let rp = p+r
            //shift
            let shiftedPos = p-p
            let shiftedR = rp - p
            yield shiftedPos
            yield shiftedR
        }
      
      let rightAxisLine = 
        rightAxis
            |> AList.toMod
            |> Mod.map (fun m -> 
                let rArr = m |> PList.toArray
                        
                Sg.draw IndexedGeometryMode.LineList
                |> Sg.trafo shiftVec
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant rArr) 
                |> Sg.uniform "Color" (Mod.constant C4b.White)
                |> Sg.uniform "LineWidth" (Mod.constant 2.0)
                |> Sg.effect [
                    toEffect DefaultSurfaces.stableTrafo
                    toEffect DefaultSurfaces.thickLine
                    toEffect DefaultSurfaces.sgColor
                        ]
            )
      
      let right = rightAxisLine|> Mod.map(fun cast -> (cast:ISg<PickingAction>))
      
      //camera forward axis
      let camForward = 
         alist {
            let! p = m.rover.position
            let! view = m.rover.HighResCam.cam.camera.view
            let f = (view.Forward*10.0)
            //shift points
            let shiftedPos = p - p
            let shiftedF = f - p
            yield shiftedPos
            yield (p+shiftedF)
        }
      
      let camForwardLine = 
        camForward
            |> AList.toMod
            |> Mod.map (fun m -> 
                let fArr = m |> PList.toArray
                        
                Sg.draw IndexedGeometryMode.LineList
                |> Sg.trafo shiftVec
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant fArr) 
                |> Sg.uniform "Color" (Mod.constant C4b.DarkGreen)
                |> Sg.uniform "LineWidth" (Mod.constant 2.0)
                |> Sg.effect [
                    toEffect DefaultSurfaces.stableTrafo
                    toEffect DefaultSurfaces.thickLine
                    toEffect DefaultSurfaces.sgColor
                        ]
            )
      
      let camForw = camForwardLine|> Mod.map(fun cast -> (cast:ISg<PickingAction>))



      let targettrafo = m.rover.target |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let target = 
           Sg.sphere 5 (Mod.constant C4b.DarkMagenta) (Mod.constant 0.2)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo targettrafo
      
      
      let frustumModel (vp:IMod<Trafo3d>) (col:C4b)= 
        Sg.wireBox' col (Box3d(V3d.NNN,V3d.III))
                            |> Sg.noEvents
                            |> Sg.trafo (vp |> Mod.map (fun vp -> vp.Inverse))
                            |> Sg.shader {
                                do! DefaultSurfaces.stableTrafo
                                do! DefaultSurfaces.vertexColor
                                }

      //high resolution camera view frustum
      let view = m.rover.HighResCam.cam.camera.view
      let fr = m.rover.HighResCam.cam.frustum
      let viewLi = m.rover.HighResCam.cam.viewList
      let vp = (RoverModel.getViewProj view fr) 
      let frustumBox = frustumModel vp C4b.Red

      //stereo cam
      //LEFT
      let viewL = m.rover.WACLR.camL.camera.view
      let frL = m.rover.WACLR.camL.frustum
      let vpL = (RoverModel.getViewProj viewL frL) 
      let frustumBoxL = frustumModel vpL C4b.Red

      //RIGHT
      let viewR = m.rover.WACLR.camR.camera.view
      let frR = m.rover.WACLR.camR.frustum
      let vpR = (RoverModel.getViewProj viewR frR) 
      let frustumBoxR = frustumModel vpR C4b.Magenta
       
      //set of frustums
      let sgFrustums = 
            viewLi 
                |> AList.map (fun v -> 
                    let vp = (RoverModel.getViewProj (Mod.constant v) fr)    
                    frustumModel vp C4b.White
                             )       
                |> AList.toASet
                |> Sg.set
    
      
        

   

      
      //highlights the area of the model which is inside the rover's view frustum
      let shading (vp:IMod<Trafo3d>) = 
        opcs
            |> Sg.cullMode (Mod.constant CullMode.Back)
            |> Sg.depthTest (Mod.constant DepthTestMode.Less)
            |> Sg.uniform "FootprintMVP" vp
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! Shading.vert
                do! Shading.frag
            }

      let shadingHR = shading vp
   
    
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
                                    |> PlaneFitting.planeFit
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

      let fullSgHR = 
        [
          m.drawing |> DrawingApp.view
          rov
          target
          frustumBox
          up |> Sg.dynamic
          forward |> Sg.dynamic
          right |> Sg.dynamic
          camForw |> Sg.dynamic
          points |> Sg.dynamic
          sgFrustums
          
        ] |> Sg.ofList
      
      let fullSgStereo = 
        [
          m.drawing |> DrawingApp.view
          rov
          target
          frustumBoxL
          frustumBoxR
        ] |> Sg.ofList
    

      let roverCamSg = 
       [
          m.drawing |> DrawingApp.view
          points |> Sg.dynamic
          frustumBox
        ] |> Sg.ofList
    
      //stereo cam
      let sceneCamL = 
            [
            m.drawing |> DrawingApp.view
            points |> Sg.dynamic
            frustumBoxL
            ] |> Sg.ofList
      
      let sceneCamR = 
            [
            m.drawing |> DrawingApp.view
            points |> Sg.dynamic
            frustumBoxR
            ] |> Sg.ofList

      
      
      
      //high res
      let fullSceneHR = 
        m.annotations |> AnnotationApp.viewGrouped shadingHR RenderPass.main fullSgHR

      let sceneHR = 
         m.annotations |> AnnotationApp.viewGrouped shadingHR RenderPass.main roverCamSg
      
      //stereo
      let fullSceneStereo = 
        m.annotations |> AnnotationApp.viewGrouped shadingHR RenderPass.main fullSgStereo

      let sceneL = 
         m.annotations |> AnnotationApp.viewGrouped shadingHR RenderPass.main sceneCamL
      
      let sceneR = 
        m.annotations |> AnnotationApp.viewGrouped shadingHR RenderPass.main sceneCamR


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
       
        FreeFlyController.controlledControl  m.rover.HighResCam.cam.camera Camera m.rover.HighResCam.cam.frustum 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "false";      
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
         ]) 
            
         (sceneHR |> Sg.map PickingAction)
      




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
         (fullSceneStereo |> Sg.map PickingAction) 
           
           
      let dependencies =   
        Html.semui @ [        
          { name = "spectrum.js";  url = "spectrum.js";  kind = Script     }
          { name = "spectrum.css";  url = "spectrum.css";  kind = Stylesheet     }
          ]



   
      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl; textOverlays (m.cameraState.view)] 
          )
        
        | Some "leftCam" ->
            require Html.semui (
              div [clazz "ui"; style "background: #1B1C1E"] [roverCamControl]
          )

        | Some "controls" -> 
          require dependencies (
            body [style "width: 100%; height:100%; background: transparent";] [
              div[style "color:white; margin: 5px 15px 5px 5px"][

                h4[][text "Menu Options"]
                div [ clazz "item" ] [ 
                dropdown { placeholder = "Save..."; allowEmpty = false } [ clazz "ui simple inverted selection dropdown" ] (m.saveOptions |> AMap.map (fun k v -> text v)) m.currentSaveOption Action.SaveConfigs 
                     ]   

                h4[][text "Rover Controls"]
                p[][div[][Incremental.text (m.rover.pan.current |>Mod.map (fun f -> "Panning - current value: " + f.ToString())); slider { min = -180.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.pan.current RoverAction.ChangePan]] |> UI.map RoverAction 
                p[][div[][Incremental.text (m.rover.tilt.current |> Mod.map (fun f -> "Tilting - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.tilt.current RoverAction.ChangeTilt]] |> UI.map RoverAction  
                p[][div[][text "Select Camera: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.cameraOptions |> AMap.map (fun k v -> text v)) m.rover.currentCamType RoverAction.SwitchCamera ]] |> UI.map RoverAction
                p[][div[][text "Select Pan Overlap: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.panOverlapOptions |> AMap.map (fun k v -> text v)) m.rover.currentPanOverlap RoverAction.ChangePanOverlap ]] |> UI.map RoverAction
                p[][div[][text "Select Tilt Overlap: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.tiltOverlapOptions |> AMap.map (fun k v -> text v)) m.rover.currentTiltOverlap RoverAction.ChangeTiltOverlap ]] |> UI.map RoverAction


                //button [onClick (fun _ -> RoverAction.MoveToRegion)]  [text "Move to region"] |> UI.map RoverAction
                button [onClick (fun _ -> RoverAction.CalculateAngles)]  [text "calculate values"] |> UI.map RoverAction
                button [onClick (fun _ -> RoverAction.RotateToPoint)]  [text "rotate to points"] |> UI.map RoverAction

              ]
            ]
          )
        
        | Some "rightCam" ->
            body [] [
              div [style "color: white; font-size: large; background-color: black; width: 100%; height: 100%"] [text "currently not active"]
          ]
          
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

      let initialDockConfig = 
        config {
          content (

              horizontal 23.0 [
                
                vertical 17.0 [
                element {id "render"; title "Main View"; weight 9.0}
                horizontal 8.0[
                element {id "leftCam"; title "HR-Cam / WACL"; weight 4.0}
                element {id "rightCam"; title "WACR"; weight 4.0}
                ]
                
                ]
                element {id "controls"; title "Controls"; weight 6.0}
                
              
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
      let positionCamL = initialRoverPos - rightV
      let positionCamR = initialRoverPos + rightV
      let forwardL = forward - rightV
      let forwardR = forward + rightV
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
          dockConfig         = initialDockConfig        
          region             = None
          roiBboxFull        = false
          saveOptions        = HMap.ofList [CameraState, "CameraState"; RoverState, "RoverState"; PlaneState, "PlaneState"]
          currentSaveOption  = None
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }


