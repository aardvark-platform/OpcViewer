module Utilities

    //let collectionOfFunctions = 
        
        //let pointViewSpace = viewM.Forward * (V4d(interestPoint,1.0))
        //let va = -V3d.ZAxis
        //let vb = pointViewSpace.XYZ.Normalized
        //let cross = vb.Cross(va)
        //let d = cross.Dot(V3d.YAxis)
        //let d1 = va.Dot(vb)
        //let signedAngle = atan2 d d1  * Constant.DegreesPerRadian

        ////tilting
        //let iProj = viewM.Forward.TransformPos interestPoint
        //let tiltAngle = atan2 -iProj.Y -iProj.Z * Constant.DegreesPerRadian
        //printfn "%A tilt:" tiltAngle
        //let roverWithTilt = tilting (setTilt m (m.tilt.current + tiltAngle))

        ////panning
        //let viewM2 = roverWithTilt.camera.view.ViewTrafo
        //let iProj2 = viewM2.Forward.TransformPos interestPoint
        //let panAngle = atan2 iProj2.X -iProj2.Z * Constant.DegreesPerRadian
        //printfn "%A pan:" panAngle
        //panning (setPan roverWithTilt (roverWithTilt.pan.current + panAngle))




        //in world space
        //let f = m.camera.view.Forward.Normalized
        //let v = (interestPoint - m.camera.view.Location).Normalized
        //let length = f.Length * v.Length
        //let rad = acos((f.Dot(v)) / length) 
        //let angle = (rad/Math.PI)*180.0


        //with clipping
        //let projM = (Frustum.projTrafo(m.frustum))
        //let clip = projM.Forward * pointViewSpace

        ////check if point is within frustum
        //let low = -clip.W
        //let upp = clip.W
        //let inside =  (clip.X > low && clip.X < upp && clip.Y > low && clip.Y < upp && clip.Z > low && clip.Z < upp) 
        //let onRight = clip.X > upp
        //let onLeft = clip.X < low

        //let ro = 
        //    match inside, onRight, onLeft with
        //    | true, _, _ -> m
        //    | false, true, false -> panning (setPan m (m.pan.current + angle))
        //    | false, false, true -> panning (setPan m (m.pan.current - angle))
        //    |  _ -> m


        //visualisation of corner points
    //  let corners = m.rover.corners

    //  let LBF = 
    //     m.rover.cornerLBF |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some lbf -> 
    //                Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(lbf)))
    //            | None -> Sg.empty
    //    )

    //  let RBF = 
    //     m.rover.cornerRBF |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some rbf -> 
    //                Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(rbf)))
    //            | None -> Sg.empty
                
    //    )

    //  let LTF = 
    //     m.rover.cornerLTF |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some ltf -> 
    //                Sg.sphere 5 (Mod.constant C4b.Green) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(ltf)))
    //            | None -> Sg.empty
                
    //    )
    
    //  let RTF = 
    //     m.rover.cornerRTF |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some rtf -> 
    //                Sg.sphere 5 (Mod.constant C4b.Yellow) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(rtf)))
    //            | None -> Sg.empty
                
    //    )

    
    ////back corners
    //  let LBB = 
    //     m.rover.cornerLBB |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some lbb -> 
    //                Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(lbb)))
    //            | None -> Sg.empty
    //    )

    //  let RBB = 
    //     m.rover.cornerRBB |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some rbb -> 
    //                Sg.sphere 5 (Mod.constant C4b.DarkGreen) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(rbb)))
    //            | None -> Sg.empty
                
    //    )

    //  let LTB = 
    //     m.rover.cornerLTB |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some ltb -> 
    //                Sg.sphere 5 (Mod.constant C4b.White) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(ltb)))
    //            | None -> Sg.empty
                
    //    )
    
    //  let RTB = 
    //     m.rover.cornerRTB |> Mod.map(fun p -> 
      
    //        match p with
    //            | Some rtb -> 
    //                Sg.sphere 5 (Mod.constant C4b.White) (Mod.constant 0.1)
    //                    |> Sg.noEvents
    //                    |> Sg.effect [ 
    //                        toEffect Shader.stableTrafo
    //                        toEffect DefaultSurfaces.vertexColor
    //                                ]
    //                    |> Sg.trafo (Mod.constant(Trafo3d.Translation(rtb)))
    //            | None -> Sg.empty
                
    //    )

    //line between bottom points
      //let shiftP = m.rover.cornerLBF
      //let lbb = m.rover.cornerLBB
      //let rbb = m.rover.cornerRBB
      //let rbf = m.rover.cornerRBF

      //let pointsB = 
      //  Mod.map2 (fun (p1:Option<V3d>) (p2:Option<V3d>) ->
      //      match p1,p2 with
      //          | Some p1, Some p2 -> 
      //              let p1Shifted = p1 - p1
      //              let p2Shifted = p2 - p1
      //              [|p1Shifted; p2Shifted|]
      //          | _ -> [||]

      //  ) shiftP lbb
    
       
      //let shiftTrafo = 
      //  Mod.map(fun p -> 
      //      match p with
      //          | Some v -> Trafo3d.Translation(v)
      //          | None -> Trafo3d.Translation(V3d.OOO)
      //          ) shiftP
      //let line1 = 
      //  Sg.draw IndexedGeometryMode.LineList
      //  |> Sg.trafo shiftTrafo
      //  |> Sg.vertexAttribute DefaultSemantic.Positions pointsB 
      //  |> Sg.uniform "Color" (Mod.constant C4b.Cyan)
      //  |> Sg.uniform "LineWidth" (Mod.constant 2.0)
      //  |> Sg.effect [
      //      toEffect DefaultSurfaces.stableTrafo
      //      toEffect DefaultSurfaces.thickLine
      //      toEffect DefaultSurfaces.sgColor
      //      ]
      
      //let line = (line1:ISg<PickingAction>)

      //let ROIbox = 
      // m.rover.reg |> 
      //  Mod.map (fun p ->
      //      match p with
      //          | None -> Sg.empty
      //          | Some points -> 
      //                  let lis = points |> AList.toPList
      //                   //set shift vector
      //                  let shift = lis |> PList.first

      //                  //shift points
      //                  let list = lis |> PList.toList 
      //                  let lis2 = list |> List.map (fun (p:V3d) ->  p - shift)
      //                  let box = Box3d(lis2)
      //                  let translation = Trafo3d.Translation(shift)
      //                  let rotation = Mod.map (fun up -> Trafo3d.RotateInto(V3d.OIO,up)) m.rover.up
      //                  let trafo =  Mod.map(fun r -> r * translation) rotation
      //                  //let rotation = Trafo3d.RotateInto(V3d.OIO,box.Center.Normalized)
      //                  //let trafo =  rotation * translation
      //                  //let toggle = m.roiBboxFull
      //                  //let boxForm = toggle |> Mod.map (fun f -> 
      //                  //                            match f with
      //                  //                            | true -> Sg.box' C4b.Cyan box
      //                  //                            | false -> Sg.wireBox' C4b.Cyan box
      //                  //                                )
      //                  //boxForm |> Mod.map (fun dr -> 
      //                  Sg.wireBox' C4b.Cyan box
      //                       |> Sg.noEvents
      //                       |> Sg.trafo (Mod.constant translation)//trafo//(Mod.constant trafo)
      //                       |> Sg.effect [
      //                                        toEffect DefaultSurfaces.stableTrafo
      //                                        toEffect DefaultSurfaces.vertexColor
      //                                      ]
      //                                      //)
                  
                                   
            
      //             )    
      //             |> Sg.dynamic

      //view space atan2
      //let viewM = m.camera.view.ViewTrafo
        //let projM = Frustum.projTrafo(m.frustum)
        //let viewProj = viewM * projM

        ////transform points to projection space
        //let transformedpoints = region |> PList.toList |> List.map (fun p -> viewProj.Forward.TransformPosProj p)

        ////set up bounding box
        //let boxPoints = transformedpoints |> List.map(fun p -> ((V2d(p.X, p.Y) + V2d.One) * 0.5))
        //let bBox =  boxPoints |> Box2d //coords between 0 and 1
        //let size = bBox.Size
        //let leftBottomP = V3d(bBox.Min,1.0)
      
        ////transform point back to view space
        //let invP = projM.Backward.TransformPos leftBottomP

        ////Rotating of the camera
        ////let iProj = viewM.Forward.TransformPos bBox.Center
        //let iProj = invP
        //let tiltAngle = atan2 -iProj.Y -iProj.Z
        //let panAngle = atan2 iProj.X -iProj.Z 

        //let rotTrafo = Trafo3d.Rotation(tiltAngle, panAngle, 0.0)
        //let newView = CameraView.ofTrafo (m.camera.view.ViewTrafo * rotTrafo)

       
        //{m with camera =  {m.camera with view = newView} }


        //ATTEMPT:calculate bounding box
        //let pointsInViewSpace = points  |> List.map (fun p -> cam.ViewTrafo.Forward.TransformPos p) 

        ////get corners of box
        //let box = pointsInViewSpace |> Box3d

        //let xMin = box.Min.X
        //let xMax = box.Max.X
        //let yMin = box.Min.Y
        //let yMax = box.Max.Y
        //let zMin = box.Min.Z
        //let zMax = box.Max.Z
        
        ////8 corner points
        //let leftBottomFront = V3d(xMin, yMin, zMin)
        //let rightBottomFront = V3d(xMax, yMin, zMin)
        //let leftTopFront = V3d(xMin, yMax, zMin)
        //let rightTopFront = V3d(xMax, yMax, zMin)
        //let leftBottomBack = V3d(xMin, yMin, zMax)
        //let rightBottomBack = V3d(xMax, yMin, zMax)
        //let leftTopBack = V3d(xMin, yMax, zMax)
        //let rightTopBack = V3d(xMax, yMax, zMax)

        ////store them in list and transform every point back to world space
        //let cornerList = [leftBottomFront; rightBottomFront; leftTopFront; rightTopFront; leftBottomBack;rightBottomBack; leftTopBack; rightTopBack]
        //let cornersInWorldSpace = cornerList |> List.map(fun corner -> cam.ViewTrafo.Backward.TransformPos corner)
        //let cornersPList = cornersInWorldSpace |> PList.ofList

        //let LBF = cornersInWorldSpace.Item(0)
        //let RBF = cornersInWorldSpace.Item(1)
        //let LTF = cornersInWorldSpace.Item(2) //p1
        //let RTF = cornersInWorldSpace.Item(3) //p2
        //let LBB = cornersInWorldSpace.Item(4)
        //let RBB = cornersInWorldSpace.Item(5)
        //let LTB = cornersInWorldSpace.Item(6)
        //let RTB = cornersInWorldSpace.Item(7)

        //let p1 = leftTopFront
        //let p2 = rightTopFront

        //let camPos = m.camera.view.Location
        //let dir1 = LTB - camPos
        //let dir2 = RTB - camPos
        //let p1Norm = dir1.Normalized
        //let p2Norm = dir2.Normalized

        //let angleBetween = (acos(p1Norm.Dot(p2Norm))) * Constant.DegreesPerRadian

        //printfn "%A angle:" angleBetween

        //let p1ws = p1 |> cam.ViewTrafo.Backward.TransformPos 
        //let p2ws = p2 |> cam.ViewTrafo.Backward.TransformPos 

     
        //{m with camera =  {m.camera with view = cam}; cornerLBF = Some LBF; cornerLTF = Some LTF; 
        //    cornerRBF = Some RBF; cornerRTF = Some RTF; 
        //    cornerLBB = Some LBB; cornerRBB = Some RBB;
        //    cornerLTB = Some LTB; cornerRTB = Some RTB;
        //    corners = Some cornersPList }

        //{m with camera =  {m.camera with view = viewM2} }