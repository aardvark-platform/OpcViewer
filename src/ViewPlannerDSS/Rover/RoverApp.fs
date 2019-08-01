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
    
    let calcPanTiltValues (m:RoverModel) (points:List<V3d>) (cam:CameraView) =
        
        let pointsInViewSpace = points  |> List.map (fun p -> cam.ViewTrafo.Forward.TransformPos p) 

        let box = pointsInViewSpace |> Box3d
        let p1 = box.Min.Normalized
        let p2 = (box.Min + V3d(box.SizeX, 0.0, 0.0)).Normalized

        let angleBetween = (acos(p1.Dot(p2))) * Constant.DegreesPerRadian

        printfn "%A angle:" angleBetween

        let p1ws = p1 |> cam.ViewTrafo.Backward.TransformPos 
        let p2ws = p2 |> cam.ViewTrafo.Backward.TransformPos 

        {m with boxP1 = Some p1ws; boxP2 = Some p2ws}




    let checkROIFullyInside (m:RoverModel) (region:plist<V3d>) =
        
        //calculate center point of region
        let sum = region.Sum()
        let c = region |> PList.count
        let centerpoint = sum / (float c)

        //calculate new view matrix by tilting and panning to center point
        let viewM = m.camera.view.ViewTrafo
        let iProj = viewM.Forward.TransformPos centerpoint
        let tiltAngle = atan2 -iProj.Y -iProj.Z
        let panAngle = atan2 iProj.X -iProj.Z
        let rotTrafo = Trafo3d.Rotation(tiltAngle, panAngle, 0.0)
        let viewM2 = CameraView.ofTrafo (m.camera.view.ViewTrafo * rotTrafo)

        //transform region points to projection space
        let projM = Frustum.projTrafo(m.frustum)
        let viewProj = viewM2.ViewTrafo * projM
        let transformedpoints = region |> PList.toList |> List.map (fun p -> viewProj.Forward.TransformPosProj p) 
        //let normPoints = transformedpoints |> List.map(fun p -> ((V2d(p.X, p.Y) + V2d.One) * 0.5))
        
        //check if all of the points have values between 0 and 1 
        //let allInside = List.forall (fun (point:V2d) -> (( point.X > 0.0 && point.X < 1.0) && ( point.Y > 0.0 && point.Y < 1.0)) ) normPoints
        let allInside = List.forall (fun (point:V3d) -> (( point.X > -1.0 && point.X  < 1.0) && ( point.Y > -1.0 && point.Y < 1.0)) ) transformedpoints

        let points = region |> PList.toList
       //if true then ROI fits in frustum
        match allInside with
            | true -> {m with camera =  {m.camera with view = viewM2} }
            | false -> calcPanTiltValues m points viewM2
            
    





    let moveFrustum (m:RoverModel) (region:plist<V3d>)=
       
        let v = checkROIFullyInside m region
        v
       
                

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
                
              
    


