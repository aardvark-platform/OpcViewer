namespace ViewPlanner.Rover

open Aardvark.Base

module RoverApp =
    open Aardvark.UI

    let panning (m:RoverModel) =
        let forward = m.camera.view.Forward
        let up = m.camera.view.Up
        let panRotation = Rot3d(up, m.pan.delta.RadiansFromDegrees())
        let targetWithPan = panRotation.TransformDir(forward)

        let newView = CameraView.look m.position targetWithPan.Normalized up
        {m with camera =  {m.camera with view = newView} }


    
    let tilting (m:RoverModel) =
        let forward = m.camera.view.Forward
        let right = m.camera.view.Right
        let tiltRotation = Rot3d(right, m.tilt.delta.RadiansFromDegrees())
        let targetWithTilt = tiltRotation.TransformDir(forward)

        let newView = CameraView.look m.position targetWithTilt.Normalized m.camera.view.Up
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
    

    let moveFrustum (m:RoverModel) (interestPoint:V3d) =
        let viewM = m.camera.view.ViewTrafo
        let projM = (Frustum.projTrafo(m.frustum))
        let vpm = viewM * projM
        let ro = m

        let ma = vpm.Forward
        let p = V4d(interestPoint, 1.0)

        let clip = ma * p

        //check if point is within frustum
        let low = -clip.W
        let upp = clip.W
        let inside =  (clip.X > low && clip.X < upp && clip.Y > low && clip.Y < upp && clip.Z > low && clip.Z < upp) 
        let onRight = clip.X > upp
        let onLeft = clip.X < low

        let ro = 
            match inside, onRight, onLeft with
            | true, _, _ -> m
            | false, true, false -> panning (setPan m (m.pan.current + 10.0))
            | false, false, true -> panning (setPan m (m.pan.current - 10.0))
            |  _ -> m

    
        ro




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
                
              
    


