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
    open Aardvark.Base
    open Aardvark.Base.MultimethodTest
    
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
                        | Some p -> { model.rover with position = (p+n); projsphere = {model.rover.projsphere with position = (p+n)}}
                        | None -> model.rover
                { model with rover = roverModel}



            | Keys.Enter ->
                let pointsOnAxisFunc = OpcSelectionViewer.AxisFunctions.pointsOnAxis None
                let updatedPicking = PickingApp.update model.pickingModel (PickingAction.AddBrush pointsOnAxisFunc)
                let points = model.pickingModel.intersectionPoints
            
                { model with pickingModel = updatedPicking; region = Some points; rover = {model.rover with reg = Some points}}


            | Keys.T ->
                let pointsOnAxisFunc = OpcSelectionViewer.AxisFunctions.pointsOnAxis None
                let updatedPicking = PickingApp.update model.pickingModel (PickingAction.AddTestBrushes pointsOnAxisFunc)
            
                { model with pickingModel = updatedPicking; }

            | Keys.F1 -> {model with roiBboxFull = not model.roiBboxFull}

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

        | RoverAction msg -> //REVIEW
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
                
                  | MoveToRegion ->
                    let r = RoverApp.update model.rover (MoveToRegion)
                    {model with rover = r}
                
                  | SwitchCamera c ->
                    let r = RoverApp.update model.rover (SwitchCamera c)
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
       

       //projection points on sphere
      
      let i =
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
                        toEffect (DefaultSurfaces.constantColor C4f.White)
                        Shader.PointSprite.Effect
                        ]
            
                    |> Sg.uniform "PointSize" (Mod.constant 10.0)


            )
      
      let points = i|> Mod.map(fun cast -> (cast:ISg<PickingAction>)) 


      //let po = m.rover.projPoints
      //let li = po |> AList.toList
      
      
      
      //let shifted = li |> List.map(fun p -> p - shift)
      //let arr = shifted |> List.toArray |> Mod.constant
      //let shiftV = Trafo3d.Translation(shift)
      //let points = 
      //  Sg.draw IndexedGeometryMode.PointList 
      //   |> Sg.vertexAttribute DefaultSemantic.Positions arr
      //   |> Sg.trafo (Mod.constant shiftV)
      //   |> Sg.effect [
      //      toEffect DefaultSurfaces.stableTrafo
      //      toEffect (DefaultSurfaces.constantColor C4f.White)
      //      //Shader.PointSprite.Effect
      //      ]
            
      //      |> Sg.uniform "PointSize" (Mod.constant 10.0)



        //|> Sg.translate' (Mod.constant shift)
        //|> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant arr) 
        //|> Sg.uniform "Color" (Mod.constant C4b.Yellow)
        //|> Sg.uniform "PointSize" (Mod.constant 2.0)
        //|> Sg.effect [
        //    toEffect DefaultSurfaces.stableTrafo
        //    toEffect DefaultSurfaces.sgColor
        //    ]
        
        
      
      let rotateIntoCoordinateSystem (vector:V3d) = 
        
        let pos = m.rover.position
        let target = m.rover.target
        let up = m.rover.up

        let forward = Mod.map2(fun (p:V3d) (t:V3d) -> ((t - p).Normalized)) pos target
        let right = Mod.map2(fun (f:V3d) (u:V3d) -> ((f.Cross(u)).Normalized)) forward up
        let rotZ = up |> Mod.map(fun u -> Trafo3d.RotateInto(V3d.OOI,u))
        let rotY = Mod.map(fun (r:V3d) -> Trafo3d.RotateInto(V3d.OIO,r))right
        let rotX = Mod.map(fun (f:V3d) -> Trafo3d.RotateInto(V3d.IOO,f))forward

        //order z y x
        let rotatedByZ = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotZ
        let rotatedByY = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotY rotatedByZ
        let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotX rotatedByY

        //order z x y
        //let rotatedByZ = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotZ
        //let rotatedByX = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotX rotatedByZ
        //let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotY rotatedByX

        //order x y z
        //let rotatedByX = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotX
        //let rotatedByY = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotY rotatedByX
        //let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotZ rotatedByY

        //order x z y
        //let rotatedByX = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotX
        //let rotatedByZ = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotZ rotatedByX
        //let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotY rotatedByZ

        ////order y x z
        //let rotatedByY = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotY
        //let rotatedByX = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotX rotatedByY
        //let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotZ rotatedByX

        //order y z x
        //let rotatedByY = Mod.map(fun (r:Trafo3d) -> r.Forward.TransformPos vector)rotY
        //let rotatedByZ = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotZ rotatedByY
        //let rotatedFinal = Mod.map2(fun (r:Trafo3d)(v:V3d) -> r.Forward.TransformPos v)rotX rotatedByZ

        rotatedFinal

        
      //point on sphere where theta = 90 phi = 90
      let xCartesian = cos(90.0)*sin(90.0)
      let yCartesian = sin(90.0)*sin(90.0)
      let zCartesian = cos(90.0)
      let ref = V3d(xCartesian, yCartesian, zCartesian) 
      let s = m.rover.position

      ///let rotated = rotateIntoCoordinateSystem ref
      let shifted = Mod.map(fun s  -> ref + s) s 
      let translation = Mod.map(fun a -> Trafo3d.Translation(a)) shifted

      let refSphere = 
           Sg.sphere 5 (Mod.constant C4b.Green) (Mod.constant 0.05)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo translation
      
      //
     
     //point on sphere where theta = 90 phi = 180
      let xPole = cos(90.0)*sin(180.0)
      let yPole = sin(90.0)*sin(180.0)
      let zPole = cos(180.0)
      let pole = V3d(xPole, yPole, zPole) 
      //let poleRef = rotateIntoCoordinateSystem pole
      let poleshifted = Mod.map(fun s  -> pole + s)  s 
      let poletranslation = Mod.map(fun a -> Trafo3d.Translation(a)) poleshifted

      let poleSphere = 
           Sg.sphere 5 (Mod.constant C4b.Blue) (Mod.constant 0.05)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo poletranslation
    
      
      //point on sphere where theta 0 = phi = 90
      let xF = sin(90.0)
      let yF = 0.0
      let zF = cos(90.0)
      let pointZero = V3d(xF, yF, zF)
      //let pZRef = rotateIntoCoordinateSystem pointZero
      let pZShifted = Mod.map(fun s -> pointZero + s)  s 
      let pZTranslation = Mod.map(fun a -> Trafo3d.Translation(a)) pZShifted

      let pointZeroSphere = 
           Sg.sphere 5 (Mod.constant C4b.Magenta) (Mod.constant 0.05)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo pZTranslation
      



      //let sphere = Sphere3d(V3d.OOO, 1.0)
      let transl = m.rover.position |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      //let rot = m.rover.up |> Mod.map(fun u -> Trafo3d.RotateInto(V3d.OIO, u))
      //let rot2 = forw |> Mod.map(fun r -> Trafo3d.RotateInto(V3d.OOI, r))
      //let trafo1 = Mod.map2 (fun r1 r2 -> r1*r2)  rot rot2
      //let rovertrafo = Mod.map2(fun t r -> r * t) transl trafo1
      //let geom = IndexedGeometryPrimitives.Sphere.wireframePhiThetaSphere sphere 5 C4b.Cyan
      //let rov = 
      //     geom
      //      |> Sg.ofIndexedGeometry
      //      |> Sg.trafo transl
      //      |> Sg.uniform "Color" (Mod.constant C4b.Cyan)
      //      |> Sg.effect [
      //          toEffect DefaultSurfaces.stableTrafo
      //          toEffect DefaultSurfaces.sgColor
      //      ]
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
            |> Mod.map (fun m -> //REVIEW
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
                        
                Sg.draw IndexedGeometryMode.LineList  //REVIEW
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
            let! view = m.rover.camera.view
            let f = (view.Forward*2.0)
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



      let projTrafo1 = m.rover.projPoint1 |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let projection1 = 
          Sg.sphere 5 (Mod.constant C4b.Yellow) (Mod.constant 0.05)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo projTrafo1
      
      let projTrafo2 = m.rover.projPoint2 |> Mod.map (fun pos -> Trafo3d.Translation(pos.X, pos.Y, pos.Z))
      let projection2 = 
          Sg.sphere 5 (Mod.constant C4b.Yellow) (Mod.constant 0.05)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo projTrafo2

      
      

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
    
     
     //visualize corner points
      let corners = m.rover.corners

      //shift vector
      //let shifted = 
      //  corners
      //      |> Mod.map (fun point -> 
      //          match point with
      //          | None -> List.empty
      //          | Some p -> 
      //              let l = p |> AList.toList 
      //              let first = l.Head
      //              l |> List.map(fun point -> point - first)
      //              )

      //front corners
      let LBF = 
         m.rover.cornerLBF |> Mod.map(fun p -> 
      
            match p with
                | Some lbf -> 
                    Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(lbf)))
                | None -> Sg.empty
        )

      let RBF = 
         m.rover.cornerRBF |> Mod.map(fun p -> 
      
            match p with
                | Some rbf -> 
                    Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(rbf)))
                | None -> Sg.empty
                
        )

      let LTF = 
         m.rover.cornerLTF |> Mod.map(fun p -> 
      
            match p with
                | Some ltf -> 
                    Sg.sphere 5 (Mod.constant C4b.Green) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(ltf)))
                | None -> Sg.empty
                
        )
    
      let RTF = 
         m.rover.cornerRTF |> Mod.map(fun p -> 
      
            match p with
                | Some rtf -> 
                    Sg.sphere 5 (Mod.constant C4b.Yellow) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(rtf)))
                | None -> Sg.empty
                
        )

    
    //back corners
      let LBB = 
         m.rover.cornerLBB |> Mod.map(fun p -> 
      
            match p with
                | Some lbb -> 
                    Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(lbb)))
                | None -> Sg.empty
        )

      let RBB = 
         m.rover.cornerRBB |> Mod.map(fun p -> 
      
            match p with
                | Some rbb -> 
                    Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(rbb)))
                | None -> Sg.empty
                
        )

      let LTB = 
         m.rover.cornerLTB |> Mod.map(fun p -> 
      
            match p with
                | Some ltb -> 
                    Sg.sphere 5 (Mod.constant C4b.White) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(ltb)))
                | None -> Sg.empty
                
        )
    
      let RTB = 
         m.rover.cornerRTB |> Mod.map(fun p -> 
      
            match p with
                | Some rtb -> 
                    Sg.sphere 5 (Mod.constant C4b.White) (Mod.constant 0.1)
                        |> Sg.noEvents
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.vertexColor
                                    ]
                        |> Sg.trafo (Mod.constant(Trafo3d.Translation(rtb)))
                | None -> Sg.empty
                
        )

      //line between bottom points
      let shiftP = m.rover.cornerLBF
      let lbb = m.rover.cornerLBB
      let rbb = m.rover.cornerRBB
      let rbf = m.rover.cornerRBF

      let pointsB = 
        Mod.map2 (fun (p1:Option<V3d>) (p2:Option<V3d>) ->
            match p1,p2 with
                | Some p1, Some p2 -> 
                    let p1Shifted = p1 - p1
                    let p2Shifted = p2 - p1
                    [|p1Shifted; p2Shifted|]
                | _ -> [||]

        ) shiftP lbb
    
       
      let shiftTrafo = 
        Mod.map(fun p -> 
            match p with
                | Some v -> Trafo3d.Translation(v)
                | None -> Trafo3d.Translation(V3d.OOO)
                ) shiftP
      let line1 = 
        Sg.draw IndexedGeometryMode.LineList
        |> Sg.trafo shiftTrafo
        |> Sg.vertexAttribute DefaultSemantic.Positions pointsB 
        |> Sg.uniform "Color" (Mod.constant C4b.Cyan)
        |> Sg.uniform "LineWidth" (Mod.constant 2.0)
        |> Sg.effect [
            toEffect DefaultSurfaces.stableTrafo
            toEffect DefaultSurfaces.thickLine
            toEffect DefaultSurfaces.sgColor
            ]
      
      let line = (line1:ISg<PickingAction>)

      //let tup = shiftP, ltf, rbf,rtf
      //let lineBottom = 
      //  Mod.map(fun t -> 
      //      Mod.map(fun r -> 
      //      match r with
      //          | Some ap, Some bp, Some cp, Some dp -> 
      //              let apSh = ap - ap
      //              let bpSh = bp - ap
      //              let cpSh = cp - ap
      //              let dpSh = dp - ap
      //              [|apSh; bpSh;cpSh;dpSh|]
      //          | _ -> [||]
            
      //          )t
      //      )tup
       



      //let leftBottomFront = 
      // shifted |> Mod.map(fun l -> 
     
      //  match l.IsEmpty with
      //      | true -> Sg.empty
      //      | false -> 
      //          let trafo = Mod.map(fun (s:List<V3d>) -> Trafo3d.Translation(s.Item(1))) shifted
      //          Sg.sphere 5 (Mod.constant C4b.DarkCyan) (Mod.constant 0.2)
      //          |> Sg.noEvents
      //          |> Sg.effect [ 
      //              toEffect Shader.stableTrafo
      //              toEffect DefaultSurfaces.vertexColor
      //              ]
      //          |> Sg.trafo trafo
      //    )
      //let LBF = Mod.map(fun lf -> lf:ISg<PickingAction>)leftBottomFront

      

      
      




      let ROIbox = 
       m.rover.reg |> 
        Mod.map (fun p ->
            match p with
                | None -> Sg.empty
                | Some points -> 
                        let lis = points |> AList.toPList
                         //set shift vector
                        let shift = lis |> PList.first

                        //shift points
                        let list = lis |> PList.toList 
                        let lis2 = list |> List.map (fun (p:V3d) ->  p - shift)
                        let box = Box3d(lis2)
                        let translation = Trafo3d.Translation(shift)
                        let rotation = Mod.map (fun up -> Trafo3d.RotateInto(V3d.OIO,up)) m.rover.up
                        let trafo =  Mod.map(fun r -> r * translation) rotation
                        //let rotation = Trafo3d.RotateInto(V3d.OIO,box.Center.Normalized)
                        //let trafo =  rotation * translation
                        //let toggle = m.roiBboxFull
                        //let boxForm = toggle |> Mod.map (fun f -> 
                        //                            match f with
                        //                            | true -> Sg.box' C4b.Cyan box
                        //                            | false -> Sg.wireBox' C4b.Cyan box
                        //                                )
                        //boxForm |> Mod.map (fun dr -> 
                        Sg.wireBox' C4b.Cyan box
                             |> Sg.noEvents
                             |> Sg.trafo (Mod.constant translation)//trafo//(Mod.constant trafo)
                             |> Sg.effect [
                                              toEffect DefaultSurfaces.stableTrafo
                                              toEffect DefaultSurfaces.vertexColor
                                            ]
                                            //)
                  
                                   
            
                   )    
                   |> Sg.dynamic
              
         
     

      //let camPos = m.rover.position
      //let p1Pos = m.rover.boxP1
      //let p2Pos = m.rover.boxP2


      //let points1 = 
      //  Mod.map2 (fun (camP:V3d) (p1:Option<V3d>) ->
      //      match p1 with
      //          | Some po -> 
      //              //let p1L = V3d(po.X, po.Y, po.Z * 2.0)
      //              //shift the points
      //              let camShifted = camP - camP
      //              let p1LShifted = po - camP
      //              [|camShifted; p1LShifted|]
      //          | None -> [||]
      //  ) camPos p1Pos
    
      //let points2 = 
      //  Mod.map2 (fun (camP:V3d) (p2:Option<V3d>) ->
      //      match p2 with
      //          | Some po -> 
      //              //let p2L = V3d(po.X, po.Y, po.Z * 2.0)
      //              //shift the points
      //              let camShifted = camP - camP
      //              let p2LShifted = po - camP
      //              [|camShifted; p2LShifted|]
      //          | None -> [||]
      //  ) camPos p2Pos
    
      //let shiftTrafo = Mod.map(fun p -> Trafo3d.Translation(p)) camPos
      //let line1 = 
      //  Sg.draw IndexedGeometryMode.LineList
      //  |> Sg.trafo shiftTrafo
      //  |> Sg.vertexAttribute DefaultSemantic.Positions points1 
      //  |> Sg.uniform "Color" (Mod.constant C4b.Cyan)
      //  |> Sg.uniform "LineWidth" (Mod.constant 2.0)
      //  |> Sg.effect [
      //      toEffect DefaultSurfaces.stableTrafo
      //      toEffect DefaultSurfaces.thickLine
      //      toEffect DefaultSurfaces.sgColor
      //      ]

      //let line2 = 
      //  Sg.draw IndexedGeometryMode.LineList
      //  |> Sg.trafo shiftTrafo
      //  |> Sg.vertexAttribute DefaultSemantic.Positions points2 
      //  |> Sg.uniform "Color" (Mod.constant C4b.Cyan)
      //  |> Sg.uniform "LineWidth" (Mod.constant 2.0)
      //  |> Sg.effect [
      //      toEffect DefaultSurfaces.stableTrafo
      //      toEffect DefaultSurfaces.thickLine
      //      toEffect DefaultSurfaces.sgColor
      //      ]





      //let line1 = 
      //  points1 |> Mod.map (fun l -> 
      //      Picking.Sg.drawOutline (Mod.constant C4b.DarkBlue) (Mod.constant C4b.DarkBlue) (Mod.constant 1.0) (Mod.constant 0.01) (Mod.constant 2.0) (Mod.constant true) l  
      //      )
      
      //let line2 = 
      //  points2 |> Mod.map (fun l -> 
      //      Picking.Sg.drawOutline (Mod.constant C4b.DarkBlue) (Mod.constant C4b.DarkBlue) (Mod.constant 1.0) (Mod.constant 0.01) (Mod.constant 2.0) (Mod.constant true) l  
      //      )
       
        

      //let region = m.region
      //let centerPoint = 
      //  let r = region |> AList.toList
      //  match r.Length with
      //      | 0 -> Sg.empty
      //      | _ -> 
      //          let sum = r.Sum()
      //          let c = r |> List.length
      //          let average = sum / (float c)
      //          let tr = Trafo3d.Translation(average)
      //          Sg.sphere 5 (Mod.constant C4b.DarkCyan) (Mod.constant 0.2)
      //          |> Sg.noEvents
      //          |> Sg.effect [ 
      //                      toEffect Shader.stableTrafo
      //                      toEffect DefaultSurfaces.vertexColor
      //                              ]
      //          |> Sg.trafo (Mod.constant tr)
                
            
       
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
          up |> Sg.dynamic
          forward |> Sg.dynamic
          right |> Sg.dynamic
          camForw |> Sg.dynamic
          //refSphere
          //poleSphere
          //pointZeroSphere
          points |> Sg.dynamic
        ] |> Sg.ofList
    
      let roverCamScene = 
       [
          PickingApp.view m.pickingModel
          //drawPlane
          //target
          //shading
          frustumBox
          //boxP1 |> Sg.dynamic
          //boxP2 |> Sg.dynamic
          //line1
          //line2
          LBF|> Sg.dynamic
          RBF|> Sg.dynamic
          LTF|> Sg.dynamic
          RTF|> Sg.dynamic
          LBB|> Sg.dynamic
          RBB|> Sg.dynamic
          LTB|> Sg.dynamic
          RTB|> Sg.dynamic
          line
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

      
      //let asp = Mod.map(fun aspect -> aspect |> Frustum.withAspect) m.rover.frustum


      

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
                //p[][text "Press R to place rover at picked point"]
                //p[][text "Press L to select picked point as rover target"]
                p[][Incremental.text (m.rover.position |> Mod.map (fun f -> f.ToString())) ]
                p[][div[][Incremental.text (m.rover.pan.current |>Mod.map (fun f -> "Panning - current value: " + f.ToString())); slider { min = 0.0; max = 360.0; step = 1.0 } [clazz "ui blue slider"] m.rover.pan.current RoverAction.ChangePan]] |> UI.map RoverAction 
                p[][div[][Incremental.text (m.rover.tilt.current |> Mod.map (fun f -> "Tilting - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.tilt.current RoverAction.ChangeTilt]] |> UI.map RoverAction  
                p[][div[][text "Select Camera: "; dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.cameraOptions |> AMap.map (fun k v -> text v)) m.rover.currentCamType RoverAction.SwitchCamera ]] |> UI.map RoverAction
                
                //button [onClick (fun _ -> RoverAction.MoveToRegion (m.region |> AList.toPList))] [text "Move to region"] |> UI.map RoverAction
                button [onClick (fun _ -> RoverAction.MoveToRegion)]  [text "Move to region"] |> UI.map RoverAction





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
          region             = None
          roiBboxFull        = false
        }

      {
          initial = initialModel             
          update = update
          view   = view          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<Model, MModel>
      }


