namespace ViewPlanner.Rover

open Aardvark.Base
open Aardvark.Rendering

module RoverApp =

    let panning (m:RoverModel) =
        let forward = m.camera.Forward
        let up = m.camera.Up
        let panRotation = Rot3d.Rotation(up, m.pan.delta.RadiansFromDegrees())
        let targetWithPan = panRotation.Transform(forward)

        let newView = CameraView.look m.position targetWithPan.Normalized up
        {m with camera =  newView}


    
    let tilting (m:RoverModel) =
        let forward = m.camera.Forward
        let right = m.camera.Right
        let tiltRotation = Rot3d.Rotation(right, m.tilt.delta.RadiansFromDegrees())
        let targetWithTilt = tiltRotation.Transform(forward)

        let newView = CameraView.look m.position targetWithTilt.Normalized m.camera.Up
        {m with camera =  newView}


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



    let update (rover:RoverModel) (action:RoverAction) =
        
        match action with
            |ChangePosition newPos -> {rover with position = newPos} 

            |ChangePan p -> 
                let rover' = setPan rover p
                panning rover'

            |ChangeTilt t -> 
                let rover' = setTilt rover t
                tilting rover'
                
              
    


