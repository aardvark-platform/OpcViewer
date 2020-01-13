namespace ViewPlanner
open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.Base.Rendering

open Aardvark.SceneGraph
open FShade

open Aardvark.UI
open Aardvark.UI.Trafos

open OpcViewer.Base
open OpcViewer.Base.Picking

open Rabbyte.Drawing

open ViewPlanner.Rover

module Sg =
  let sphereRadius = 0.05


   //---RENDERVIEWS
  let createView (sg:ISg<Action>) (cam:IMod<CameraType>) (rover:MRoverModel) = 
      
    let att = 
        (AttributeMap.ofList [ 
            style "width: 100%; height:100%"; 
            attribute "showFPS" "false";      
            attribute "data-renderalways" "false"
            attribute "data-samples" "4"
        ]) 

    let cams = 
     adaptive {
        let! c = cam
        let left, right = 
            match c with 
            | HighResCam -> 
                let cameraMod = rover.HighResCam.cam.Current |> Mod.map (fun x -> 
                    let viewHR = x.camera.view
                    let frHR = x.frustum
                    Camera.create viewHR frHR)
                        
                (cameraMod, cameraMod)

            | WACLR -> 
                let camLMod = rover.WACLR.camL.Current |> Mod.map (fun x ->
                    let viewLeft = x.camera.view
                    let frLeft = x.frustum
                    Camera.create viewLeft frLeft
                    )
                    
                let camRMod = rover.WACLR.camR.Current |> Mod.map (fun x ->
                    let viewLeft = x.camera.view
                    let frLeft = x.frustum
                    Camera.create viewLeft frLeft
                    )
                (camLMod, camRMod)
          
        return left,right

         }
    
    let l = cams |> Mod.bind (fun f -> fst f)
    let r = cams |> Mod.bind (fun f -> snd f)

    let sgR = 
       let r = 
        adaptive {
            let! cam = cam 
            let result = 
                match cam with
                | HighResCam -> Sg.empty
                | WACLR -> sg
            return result
            }
       r |> Sg.dynamic

    let domL = DomNode.RenderControl(att, l, sg, RenderControlConfig.standard, None)
    let domR = DomNode.RenderControl(att, r, sgR, RenderControlConfig.standard, None)

    (domL, domR)
    

  //visualisation tools

  //---POSITION VISUALISATION
  let sphereVisualisation (color:C4b) (radius:float) (pos:IMod<V3d>) =
    
     let trafo = pos |> Mod.map(fun p -> Trafo3d.Translation(p))
     Sg.sphere 5 (Mod.constant color) (Mod.constant radius)
            |> Sg.noEvents
            |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.vertexColor
                    ]
            |> Sg.trafo trafo

    
  let projectionPoints (list:alist<V3d>) = 
            
    let sg:ISg<PickingAction> = 
        list
        |> AList.toMod
        |> Mod.map(fun li -> 
                
            let list = li |> PList.toList
            let shift = 
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
        |> Sg.dynamic
          
    sg

  //---

 
  //---AXIS VISUALISATION---
  let lineList (pos:IMod<V3d>) (axis:IMod<V3d>) (offset:float) = 
     alist {
        let! a = axis
        let! p = pos
        let ashifted = (a*offset)
        //shift points
        let shiftedPos = p - p
        let shiftedAxis = ashifted - p
        yield shiftedPos
        yield (p+shiftedAxis)
        }

  let lineBetweenPoints (points:V3d[]) (color:C4b) (lineWidth:float) (shift:IMod<Trafo3d>)= 

    Sg.draw IndexedGeometryMode.LineList
        |> Sg.trafo shift
        |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant points) 
        |> Sg.uniform "Color" (Mod.constant color)
        |> Sg.uniform "LineWidth" (Mod.constant lineWidth)
        |> Sg.effect [
            toEffect DefaultSurfaces.stableTrafo
            toEffect DefaultSurfaces.thickLine
            toEffect DefaultSurfaces.sgColor
                ]
   
  let axisVisualisation (list:alist<V3d>) (color:C4b) (shift:IMod<Trafo3d>) =
    list
        |> AList.toMod
        |> Mod.map (fun m -> 
            let arr = m |> PList.toArray
                 
            lineBetweenPoints arr color 2.0 shift
        )

  let lineLength = 1.0

  let cameraAxes  (view:IMod<CameraView>) (rover:MRoverModel) = 
   
    let pos = Mod.map (fun (c:CameraView) -> c.Location) view
    let forward = Mod.map (fun (c:CameraView) -> c.Forward) view
    let right = Mod.map (fun (c:CameraView) -> c.Right) view      
    let shift = Mod.map(fun p -> Trafo3d.Translation(p)) pos

    let rightAxis = 
        let list = lineList pos right lineLength
        axisVisualisation list C4b.White shift

    let forwardAxis = 
        let list = lineList pos forward lineLength
        axisVisualisation list C4b.Green shift

    let upAxis = 
        let list = lineList pos rover.up lineLength
        axisVisualisation list C4b.Blue shift
    
    let set:ISg<PickingAction> = 
        [
        rightAxis |> Sg.dynamic
        forwardAxis |> Sg.dynamic
        upAxis |> Sg.dynamic
        ] |> Sg.ofList
    
    set
 //--- 

 //---FRUSTUM VISUALISATION
  let frustumModel (vp:IMod<Trafo3d>) (col:C4b)= 
    Sg.wireBox' col (Box3d(V3d.NNN,V3d.III))
        |> Sg.noEvents
        |> Sg.trafo (vp |> Mod.map (fun vp -> vp.Inverse))
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.vertexColor
            }
  

  let frustumModel2 (vp:IMod<Trafo3d>) (col:C4b)= 
    Sg.wireBox' col (Box3d(V3d.NNN,V3d.III))
        |> Sg.noEvents
        |> Sg.trafo (vp |> Mod.map (fun vp -> vp.Inverse))
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.thickLine 
            do! DefaultSurfaces.vertexColor
            }
        |> Sg.uniform "LineWidth" (Mod.constant 5.0)

   
  let activeFrustum (view:IMod<CameraView>) (frustum:IMod<Frustum>)  =
    let vp = (RoverModel.getViewProj view frustum) 
    frustumModel2 vp C4b.DarkRed

  let sgFrustums (list:alist<CameraView>) (frustum:IMod<Frustum>) (view:IMod<CameraView>) = 

    let samplingFrustums = 
        list 
            |> AList.map (fun v -> 
                let vp = (RoverModel.getViewProj (Mod.constant v) frustum)    
                frustumModel vp C4b.White
                            )       
            |> AList.toASet
            |> Sg.set
      
    [
    samplingFrustums
    ] |> Sg.ofList

  
  let visualizeViewPlan (vp:MViewPlan) (r:MRoverModel)= 

    alist {
        let placement = vp.placement
        let target = sphereVisualisation C4b.Magenta 0.01 placement.target

        let cam = vp.cameraVariables
        let axes = cam |> AList.map (fun a -> cameraAxes a.camera.view r)
        let frustums = cam |> AList.map (fun f -> sgFrustums f.viewList f.frustum f.camera.view)

        let activeFrustums = cam |> AList.map (fun f ->  
                let views = f.viewList |> AList.toMod 
                let active = Mod.map2 (fun view idx -> view |> PList.toArray |> fun x -> x.[idx]
                                    ) views r.walkThroughIdx 
                activeFrustum active f.frustum
                )

        let positions = cam |> AList.map (fun p -> sphereVisualisation C4b.Red sphereRadius p.position)
        let projPoints = vp.projPoints |> AList.map (fun pr -> sphereVisualisation C4b.DarkYellow sphereRadius (Mod.constant pr) )

        for a in axes do
          yield a

        for f in frustums do
          yield f

        for af in activeFrustums do
            yield af
    
        for p in positions do
          yield p
    
        for pr in projPoints do
         yield pr

        yield target

    } |> AList.toASet |> Sg.set


  let roverPlacementModeScene (m:MModel) = 

    let rover = m.rover
    let placements = rover.positionsList
    
    

    let sgs = 
        alist{
            for p in placements do
     
            let p1 = sphereVisualisation C4b.Red sphereRadius p.position
            let p2 = sphereVisualisation C4b.Magenta sphereRadius p.target

            let! po = p.position
            let! t = p.target
            let arr = [|(po-po); (t-po)|]

            let trafo = p.position |> Mod.map(fun p -> Trafo3d.Translation(p))
            let line = lineBetweenPoints arr C4b.DarkGreen 1.0 trafo

            yield p1
            yield p2
            yield line
        }
    
    sgs |> AList.toASet |> Sg.set
  

  //show the last view plan in the viewplan list; 
  let sampleModeScene (m:MModel) = 
    
    alist {
        
        let! selectedPos = m.rover.selectedPosition
        let draw = m.drawing |> DrawingApp.view
        let plans = m.rover.viewplans
        let! p = plans.Content

        match p.IsEmpty(), selectedPos with 
        | true, None -> yield draw
        | true, Some placement -> 
            
            let p1 = sphereVisualisation C4b.Red sphereRadius placement.position
            let p2 = sphereVisualisation C4b.Magenta sphereRadius placement.target

            yield p1
            yield p2
            yield draw

        | false, _ -> 
            let vp = p.Item(p.MaxIndex)
            yield draw
            yield visualizeViewPlan vp m.rover

    }|> AList.toASet |> Sg.set
    

  //show the selected viewplan
  let viewplanModeScene (m:MModel) = 
    
    alist {
    
       let! selected = m.rover.selectedViewPlan
       let draw = m.drawing |> DrawingApp.view

       match selected with
       | Some vp ->
            
            let vis = visualizeViewPlan vp m.rover
            yield vis
            yield draw
     
       | None -> yield draw

    } |> AList.toASet |> Sg.set


  let setRenderViewScene (curr:IMod<Option<ModeOption>>) (m:MModel) = 
    
    let isg = curr |> Mod.map (fun f -> 
        match f with
        | Some StandardMode -> Sg.empty  
        | Some RoverPlacementMode -> roverPlacementModeScene m
        | Some ViewPlanMode -> viewplanModeScene  m
        | Some SampleMode -> sampleModeScene m
        | None -> Sg.empty  
       )
    isg |> Sg.dynamic


//---OTHER

  let myPlane (m:MModel)= 

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


   //highlights the area of the model which is inside the rover's view frustum
      //let shading (vp:IMod<Trafo3d>) = 
      //  opcs
      //      |> Sg.cullMode (Mod.constant CullMode.Back)
      //      |> Sg.depthTest (Mod.constant DepthTestMode.Less)
      //      |> Sg.uniform "FootprintMVP" vp
      //      |> Sg.shader {
      //          do! DefaultSurfaces.diffuseTexture
      //          do! Shading.vert
      //          do! Shading.frag
      //      }

      //let shadingHR = shading vp



      //let roverCamSg = 
      // [
      //    m.drawing |> DrawingApp.view
      //    points |> Sg.dynamic
      //  ] |> Sg.ofList
    
      //stereo cam
      //let sceneCamL = 
      //      [
      //      m.drawing |> DrawingApp.view
      //      points |> Sg.dynamic
      //      ] |> Sg.ofList
      
      //let sceneCamR = 
      //      [
      //      opcs
      //      m.drawing |> DrawingApp.view
      //      points |> Sg.dynamic
      //      ] |> Sg.ofList

       
      //let sceneLeft = 
      //  let result = 
      //      adaptive {
      //      let! cam = m.rover.camera
      //      let c = match cam with 
      //              | HighResCam -> roverCamSg
      //              | WACLR -> sceneCamL
        
      //      return c
      //  }
      //  result |> Sg.dynamic
    
      //let sceneRight = 
      //  let result = 
      //      adaptive {
      //      let! cam = m.rover.camera
      //      let c = match cam with 
      //              | HighResCam -> Sg.empty
      //              | WACLR -> sceneCamR
        
      //      return c
      //  }

      //  result |> Sg.dynamic