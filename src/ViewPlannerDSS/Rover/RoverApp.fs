namespace ViewPlanner.Rover

open System
open Aardvark.Base

module RoverApp =
    open Aardvark.UI

    let panning (m:RoverModel) =
        let forward = m.camera.view.Forward
        let up = m.up //rotate around global up axis
        let panRotation = Rot3d(up, m.pan.delta.RadiansFromDegrees())
        let targetWithPan = panRotation.TransformDir(forward)
        let newView = CameraView.look m.position targetWithPan.Normalized up
        {m with camera =  {m.camera with view = newView} }


    
    let tilting (m:RoverModel) =
        let forward = m.camera.view.Forward
        let right = m.camera.view.Right
        let tiltRotation = Rot3d(right, m.tilt.delta.RadiansFromDegrees())
        let targetWithTilt = tiltRotation.TransformDir(forward).Normalized

        let newView = CameraView.look m.position targetWithTilt m.camera.view.Up
        {m with camera =  {m.camera with view = newView} }


    let setPan(m:RoverModel) (value:float) =
        let dt = m.pan.previous - value
        let prev = m.pan.current
        let curr = value
        {m with pan = {m.pan with delta = dt; previous = prev; current = curr}}


    let setTilt(m:RoverModel) (value:float) =
        let dt = m.tilt.previous - value
        let prev = m.tilt.current
        let curr = value
        {m with tilt = {m.tilt with delta = dt; previous = prev; current = curr}}
    

    let moveFrustum (m:RoverModel) (region:plist<V3d>)=
        let viewM = m.camera.view.ViewTrafo

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


        //region of interest
        let bbox = Box3d(region)
        let interestPoint = bbox.Center

        let point1 = bbox.Min.X
        let point2 = bbox.Max.X
        let angleBetweenPoints = atan2 point1 point2 * Constant.DegreesPerRadian
        printfn "%A angleBetween:" angleBetweenPoints


         //tilting
        let iProj = viewM.Forward.TransformPos interestPoint
        let tiltAngle = atan2 -iProj.Y -iProj.Z
        let panAngle = atan2 iProj.X -iProj.Z 

        let rotTrafo = Trafo3d.Rotation(tiltAngle, panAngle, 0.0)
        let newView = CameraView.ofTrafo (m.camera.view.ViewTrafo * rotTrafo)

       
        {m with camera =  {m.camera with view = newView} }

       
        
     
       
        
  
      

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

    
        //ro


    let changeCam (rover:RoverModel) (camtype:Option<CameraType>)=
        
        match camtype with
            |Some Camera60 -> 
                let fr = Frustum.perspective 60.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera60}
            
            |Some Camera30 ->
                let fr = Frustum.perspective 30.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera30}
            
            |Some Camera15 ->
                let fr = Frustum.perspective 15.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera15}
            
            |Some Stereo -> rover //TODO 

            |None -> rover





    let update (rover:RoverModel) (action:RoverAction) =
        
        match action with
            |ChangePosition newPos -> {rover with position = newPos} 

            |ChangePan p -> 
                let rover' = setPan rover p
                panning rover'

            |ChangeTilt t -> 
                let rover' = setTilt rover t
                tilting rover'
            
            |MoveToRegion p ->
                moveFrustum rover p
            
            |SwitchCamera cam ->
                changeCam rover  cam
                
              
    


