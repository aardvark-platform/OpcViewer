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

open ViewPlanner.Rover

module Sg =
 
   //---RENDERVIEWS
  let createView (sg:ISg<Action>) (cam:IMod<CameraType>) (rover:MRoverModel) = 
      
    let att = 
        (AttributeMap.ofList [ 
            style "width: 100%; height:100%"; 
            attribute "showFPS" "false";      
            attribute "data-renderalways" "false"
            attribute "data-samples" "4"
        ]) 

    alist {
         let! c = cam
         match c with 
                | HighResCam -> 
                    let cameraMod = rover.HighResCam.cam.Current |> Mod.map (fun x -> 
                        let viewHR = x.camera.view
                        let frHR = x.frustum
                        Camera.create viewHR frHR)

                    let domL = DomNode.RenderControl (att, cameraMod, sg, RenderControlConfig.standard, None)
                    let domR = DomNode.RenderControl (att, cameraMod, Sg.empty, RenderControlConfig.standard, None)
                    yield domL
                    yield domR

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

                    let domL = DomNode.RenderControl (att, camLMod, sg, RenderControlConfig.standard, None)
                    let domR = DomNode.RenderControl (att, camRMod, sg, RenderControlConfig.standard, None)
                    yield domL
                    yield domR
     

        }



    //    adaptive {
    //        let! c = cam
    //        let left, right = 
    //            match c with 
    //            | HighResCam -> 
    //                let cameraMod = rover.HighResCam.cam.Current |> Mod.map (fun x -> 
    //                    let viewHR = x.camera.view
    //                    let frHR = x.frustum
    //                    Camera.create viewHR frHR)

    //                let domL = DomNode.RenderControl (att, cameraMod, sg, RenderControlConfig.standard, None)
    //                (domL, None)

    //            | WACLR -> 
                    
    //                let camLMod = rover.WACLR.camL.Current |> Mod.map (fun x ->
    //                    let viewLeft = x.camera.view
    //                    let frLeft = x.frustum
    //                    Camera.create viewLeft frLeft
    //                    )
                    
    //                let camRMod = rover.WACLR.camR.Current |> Mod.map (fun x ->
    //                    let viewLeft = x.camera.view
    //                    let frLeft = x.frustum
    //                    Camera.create viewLeft frLeft
    //                    )

    //                let domL = DomNode.RenderControl (att, camLMod, sg, RenderControlConfig.standard, None)
    //                let domR = DomNode.RenderControl (att, camRMod, sg, RenderControlConfig.standard, None)
    //                (domL, Some domR)

    //    return left,right

    //}
    //r

  //visualisation tools

  //---POSITION VISUALISATION
  let sphereVisualisation (color:C4b) (radius:float) (trafo:IMod<Trafo3d>) =
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
   
   
  let axisVisualisation (list:alist<V3d>) (color:C4b) (shift:IMod<Trafo3d>) =
        list
            |> AList.toMod
            |> Mod.map (fun m -> 
                let upArr = m |> PList.toArray
                        
                Sg.draw IndexedGeometryMode.LineList
                |> Sg.trafo shift
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant upArr) 
                |> Sg.uniform "Color" (Mod.constant color)
                |> Sg.uniform "LineWidth" (Mod.constant 2.0)
                |> Sg.effect [
                    toEffect DefaultSurfaces.stableTrafo
                    toEffect DefaultSurfaces.thickLine
                    toEffect DefaultSurfaces.sgColor
                        ]
            )


  let cameraAxes  (view:IMod<CameraView>) (rover:MRoverModel) = 
   
    let pos = rover.position
    let up = rover.up
    let forward = Mod.map (fun (c:CameraView) -> c.Forward) view
    let right = Mod.map (fun (c:CameraView) -> c.Right) view      
    let shift = Mod.map(fun p -> Trafo3d.Translation(p)) pos

    let rightAxis = 
        let list = lineList pos right 5.0
        axisVisualisation list C4b.White shift

    let forwardAxis = 
        let list = lineList pos forward 10.0
        axisVisualisation list C4b.Green shift

    let upAxis = 
        let list = lineList pos up 5.0
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


  let sgFrustums (list:alist<CameraView>) (frustum:IMod<Frustum>) (view:IMod<CameraView>) = 
      
      let vp = (RoverModel.getViewProj view frustum) 
      let camFrustum = frustumModel vp C4b.DarkRed
      let samplingFrustums = 
            list 
                |> AList.map (fun v -> 
                    let vp = (RoverModel.getViewProj (Mod.constant v) frustum)    
                    frustumModel vp C4b.White
                             )       
                |> AList.toASet
                |> Sg.set
      
      [
          camFrustum
          samplingFrustums
      
      ] |> Sg.ofList

 

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