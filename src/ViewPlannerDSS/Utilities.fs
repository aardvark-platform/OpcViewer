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

