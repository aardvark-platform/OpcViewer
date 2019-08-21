namespace ViewPlanner.Rover

open System
open Aardvark.Base

module RoverApp =
    open Aardvark.UI
    open System.Threading

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

   
    let initializeTilt (m:RoverModel) (value:float) = 
        {m with tilt = {m.tilt with previous = value; current = value}}
    
    let initializePan (m:RoverModel) (value:float) = 
        {m with pan = {m.pan with previous = value; current = value}}

    let setPan(m:RoverModel) (value:float) =
        let dt = m.pan.previous - value
        let prev = value
        //let prev = m.pan.current
        let curr = value
        {m with pan = {m.pan with delta = dt; previous = prev; current = curr}}


    let setTilt(m:RoverModel) (value:float) =
        let dt = m.tilt.previous - value
        let prev = value
        //let prev = m.tilt.current
        let curr = value
        {m with tilt = {m.tilt with delta = dt; previous = prev; current = curr}}
    
    
    let rotateIntoCoordinateSystem (m:RoverModel) (vector:V3d) = 
        
        let up = m.up
        let rotZ = Trafo3d.RotateInto(up,V3d.OOI)

        let rotatedbyZ = rotZ.Forward.TransformDir vector
        rotatedbyZ
    
        
    //returns theta, phi values in degrees
    let calcThetaPhi (position:V3d) =
        
        let x = position.X
        let y = position.Y
        let z = position.Z
   
        //quadrant
        let theta = (atan2 x y)* Constant.DegreesPerRadian 
        let phi = (acos(z)) * Constant.DegreesPerRadian

        V2d(theta,phi)



    //points: in world space
    //let calcPanTiltValues (m:RoverModel) (points:List<V3d>) (cam:CameraView) =
        
    //    let rotTrafo = Trafo3d.RotateInto(V3d.OOI, m.up)
    //    //let rotTrafo = Trafo3d.RotateInto( m.up, V3d.OOI)
    //    //project points onto projection sphere
    //    let spherePos = m.projsphere.position

        
    //    //normal points
    //    let testPoint = m.target
    //    let upPoint = m.position + (m.up*1.5)
    //    let rightPoint = 
    //        let forw = (testPoint - m.position).Normalized
    //        let r = forw.Cross(m.up)
    //        m.position + r

       
    //    //add target point for testing
    //    let l2 = [rightPoint;upPoint;testPoint]
    //    let testList = List.append points l2
    //    let shiftedPoints = testList  |> List.map (fun p -> (p - spherePos).Normalized)
    //    //let rotatedPoints = shiftedPoints  |> List.map (fun p -> rotTrafo.Forward.TransformPos p)
    //    let rotatedPoints = shiftedPoints  |> List.map (fun p -> rotateIntoCoordinateSystem m p)

    //    let r = rotatedPoints.Item(rotatedPoints.Length - 1)
    //    let thetaOfPointonForwardVec = calcTheta r.X r.Y
    //    let setR = initializePan m thetaOfPointonForwardVec
    //    let setR2 = initializeTilt setR ((acos(r.Z))*Constant.DegreesPerRadian)

    //    //calculate theta and phi for coordinates
    //    //test for the first point in the list

    //    let listOfAngles = rotatedPoints |> List.map(fun pos -> calcThetaPhi pos)
    //    //debuging
    //    for p in listOfAngles do
    //        printfn "theta phi %A %A"  (p.X) (p.Y* Constant.DegreesPerRadian) 
        
    //    let index = shiftedPoints.Length - 1 
    //    let point = rotatedPoints.Item (0)
    //    let second = rotatedPoints.Item(1)
    //    let projectionPoint1 = point + spherePos
    //    let projectionPoint2 = second + spherePos
    //    let x = point.X
    //    let y = point.Y
    //    let z = point.Z
    //    let theta = calcTheta x y
    //    let phi = acos(z) //atan2 z (sqrt((pown x 2)+(pown y 2))) 


    //    printfn "theta: %A" theta
    //    printfn "phi: %A" (phi * Constant.DegreesPerRadian)




    //    let panned = setPan setR2 (theta)
    //    let pannedRover = panning panned
    //    let tilted = setTilt pannedRover (phi* Constant.DegreesPerRadian)
    //    let newR = tilting tilted

    //    let projectionPoints = shiftedPoints |> List.map (fun p -> p + spherePos) |> PList.ofList
    //    {newR with projPoint1 = projectionPoint1; projPoint2 = projectionPoint2; projPoints = projectionPoints }


        

        



    let checkROIFullyInside (m:RoverModel) (region:Option<plist<V3d>>) =
        
        //calculate center point of region
        match region with 
            | None -> m
            | Some region -> 

                let sum = region.Sum()
                let c = region |> PList.count
                let centerpoint = sum / (float c)

        //calculate new view matrix by tilting and panning to center point
                let viewM = m.camera.view.ViewTrafo
                let iProj = viewM.Forward.TransformPos centerpoint
                let tiltAngle = atan2 -iProj.Y -iProj.Z
                let panAngle = atan2 iProj.X -iProj.Z
                //let rotTrafo = Trafo3d.Rotation(tiltAngle, panAngle, 0.0)
                let rotTrafo = Trafo3d.Rotation(0.0, panAngle, 0.0)
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
                    | false -> m//calcPanTiltValues m points viewM2
            
    
    //panR = number of required pans; tiltR = number of required tilts per pan
    //TODO: incoorporate possible overlap between pans
    let rec buildList (l:List<V2d>) (panR:int) (tiltR:int) (originalTiltR:int) (deltaTilt:float) (fov:float)=
        match panR,tiltR with 
            | (p,t) when  p = 0 && t = 0 -> l
            | (p,t) when  p >= 0 && t > 0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let newTilt = lastItem.Y + deltaTilt
                        let listItem = [V2d(lastItem.X, newTilt)]
                        let newList = List.append l listItem
                        buildList newList panR (tiltR-1) originalTiltR deltaTilt fov
            | (p,t) when p > 0 && t = 0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let newPan = lastItem.X + fov
                        let newDeltaTilt = deltaTilt * (-1.0) 
                        let listItem = [V2d(newPan, lastItem.Y)]
                        let newList = List.append l listItem
                        buildList newList (panR-1) originalTiltR originalTiltR newDeltaTilt fov
            | _,_ -> l //this case should never be reached
                
    
    //test with interpolation
    //panC, tilt C --> interpolation coefficients
    let rec buildList2 (l:List<V2d>) (inputPan:List<V2d>) (inputTilt:List<V2d>) (panR:int) (tiltR:int) (originalTiltR:int) (panC:float) (tiltC:float)=
        match panC,tiltC with 
            | (p,t) when  p > 1.0 && t > 1.0 -> l
            | (p,t) when  (p < 1.0 && t < 1.0) || (p >= 1.0 && t < 1.0) -> 
                        let lastItem = l.Item(l.Length-1)
                        let currPan = lastItem.X
                        let ref1 = V2d(currPan, inputTilt.Item(0).Y)
                        let ref2 = V2d(currPan, inputTilt.Item(1).Y)
                        //calculate interpolation coefficient
                        let i = if tiltR = 1 then 0.5 else 1.0 / (float(tiltR))
                        let newTiltC = tiltC + i
                        let interpolated = ref1 * (1.0-newTiltC) + ref2 * newTiltC                     
                        let newList = List.append l [interpolated]
                        buildList2 newList inputPan inputTilt panR tiltR originalTiltR panC newTiltC
            | (p,t) when p < 1.0 && t >= 1.0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let p1 = inputPan.Item(0)
                        let p2 = inputPan.Item(1)
                        let ip = 1.0 / (float(panR))
                        //let it = if tiltR = 1 then 0.5 else 1.0 / (float(tiltR))
                        let newpanC = panC + ip
                        let interpolated = p1 * (1.0-newpanC) + p2 * newpanC
                        let newList = List.append l [V2d(interpolated.X, lastItem.Y)]
                        //new input list
                        let newInputTilt = [inputTilt.Item(1); inputTilt.Item(0)]
                        buildList2 newList inputPan newInputTilt panR originalTiltR originalTiltR newpanC 0.0
            | _,_ -> l 


    let sampling (rover : RoverModel) = 
        
        let fov = rover.fov
        let values = rover.thetaPhiValues
        let li = values |> PList.toList
        let pans = li |> List.map (fun l -> l.X) //list with just pan values
        
        let min = pans |> Seq.indexed |> Seq.min
        let idx = fst min

        let tilts = li |> List.map (fun l -> l.Y) //list with just tilt values

        


        //sort pan values
        let sortedPans = List.sort pans
        
        let minPan = sortedPans.Head
        let maxPan = sortedPans.Item(sortedPans.Length - 1)
        let deltaPan = Math.Abs (maxPan - minPan)
        
        let panningRate = int(Math.Round(deltaPan / fov))
        printfn "pan delta %A rate %A " deltaPan panningRate

        //sort tilt values
        let sortedTilts = List.sort tilts
        let minTilt = sortedTilts.Head
        let maxTilt = sortedTilts.Item(sortedTilts.Length - 1)
        let deltaTilt = maxTilt - minTilt
       
        //bounding box sampling
        let p1 = V2d(minPan, maxTilt) //left bottom
        let p2 = V2d(minPan, minTilt) //left top
        let p3 = V2d(maxPan, minTilt) //right top
        let p4 = V2d(maxPan, maxTilt) //right bottom







        //regarding vertical fov
        //let fovHalf = fov/2.0
        //let deltaBetween = ((Math.Abs(deltaTilt)) - fovHalf)
        //let adjustedTilt =  fovHalf - deltaBetween
        let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / (fov)))
        printfn "tilt delta %A rate %A " deltaTilt tiltingRate

        //generate a sampling list with pan and tilt values
        let firstPair = V2d(minPan, maxTilt) //pair with min pan value and max tilt value (equals left bottom corner of bounding box)
        let samplingValues = [firstPair] //initial list

        let inputPan = [p2;p3]
        let inputTilt = [p1;p2]

        let panC = if panningRate = 1 then 0.5 else 1.0 / (float(panningRate))
        let tiltC = if tiltingRate = 1 then 0.5 else 1.0 / (float(tiltingRate))



        //let samplings = buildList samplingValues panningRate tiltingRate tiltingRate -adjustedTilt fov
        //let samplings = buildList samplingValues panningRate tiltingRate tiltingRate -fov fov

        //sampling with algorithm buildList2
        //let rec buildList2 (l:List<V2d>) (inputPan:List<V2d>) (inputTilt:List<V2d>) (panR:int) (tiltR:int) (originalTiltR:int) (panC:float) (tiltC:float)=
        let samplings = buildList2 samplingValues inputPan inputTilt panningRate tiltingRate tiltingRate 0.0 0.0 //panC tiltC

        samplings
       



    let moveFrustum (m:RoverModel) = 
       
        let v = checkROIFullyInside m m.reg
        v
   

    //takes pan and tilt values and calculates a view matrix for frustum visualisation
    let calculateViewMatrix (rover : RoverModel) (pan : float) (tilt : float) =
        
        let panCurr = rover.pan.current
        let panDelta = panCurr - pan
        let tiltCurr = rover.tilt.current
        let tiltDelta = tiltCurr - tilt

        let forward = rover.camera.view.Forward
        let up = rover.up 
        let right = rover.camera.view.Right

        //panning
        let panRotation = Rot3d(up, panDelta.RadiansFromDegrees())
        let targetWithPan = panRotation.TransformDir(forward)

        //tilting
        let tiltRotation = Rot3d(right, tiltDelta.RadiansFromDegrees())
        let targetWithTilt = tiltRotation.TransformDir(targetWithPan)

        let view = CameraView.look rover.position targetWithTilt.Normalized up

        view



    
    let rotateToPoint (rover:RoverModel) =
        
        
        let values = rover.samplingValues
        let li = values |> PList.toList
        let idx = rover.currIdx
        let pair = values.Item(idx) 

        //values currently rotated to
        printfn "thetaCurrent %A phiCurrent %A"  (pair.X) (pair.Y) 

        let panned = setPan rover pair.X
        let pannedRover = panning panned
        let tilted = setTilt pannedRover (pair.Y)
        let newR = tilting tilted

        //set index
        let lastIdx = li.Length - 1
        let newIdx = 
            if idx = lastIdx then 0 else (idx+1)

        {newR with currIdx = newIdx}

       
    let calculateValues (rover:RoverModel) =
        
        let region = rover.reg
        let newRover = 
         match region with
            | None -> rover
            | Some reg -> 
                let spherePos = rover.projsphere.position
                let l = reg |> PList.toList
                let shiftedPoints = l  |> List.map (fun p -> (p - spherePos).Normalized)
                let rotatedPoints = shiftedPoints  |> List.map (fun p -> rotateIntoCoordinateSystem rover p)

                let projectionPoints = shiftedPoints |> List.map (fun p -> p + spherePos) |> PList.ofList

                let thetaPhiValues = rotatedPoints |> List.map(fun p -> calcThetaPhi p)


                //debugging
                for p in thetaPhiValues do
                    printfn "theta %A phi %A"  (p.X) (p.Y) 


                let plist = thetaPhiValues |> PList.ofList

                

                //point on forward vector
                let referencePoint = rover.target
                let shifted = (referencePoint-spherePos).Normalized
                let r = rotateIntoCoordinateSystem rover shifted
                let thetaPhi = calcThetaPhi r
                printfn "thetaOnForward %A"  thetaPhi.X
                let setR = initializePan rover thetaPhi.X
                let setR2 = initializeTilt setR thetaPhi.Y

                let viewMatrices = thetaPhiValues |> List.map(fun m -> calculateViewMatrix setR2 m.X m.Y) |> PList.ofList
                
               

                {setR2 with thetaPhiValues = plist; projPoints = projectionPoints; viewList = viewMatrices }
        
        let samplings = sampling newRover

        {newRover with samplingValues = samplings |> PList.ofList}
        
       
        
  
      

        




    let changeCam (rover:RoverModel) (camtype:Option<CameraType>)=
        
        match camtype with
            |Some Camera60 -> 
                let fr = Frustum.perspective 60.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera60; fov = 60.0}
            
            |Some Camera30 ->
                let fr = Frustum.perspective 30.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera30; fov = 30.0}
            
            |Some Camera15 ->
                let fr = Frustum.perspective 15.0 0.1 10.0 1.0
                {rover with frustum = fr; currentCamType = Some Camera15; fov = 15.0}
            
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
            
            |MoveToRegion  ->
                moveFrustum rover 
            
            |SwitchCamera cam ->
                changeCam rover cam
            
            |CalculateAngles ->
                calculateValues rover
            
            | RotateToPoint ->
                rotateToPoint rover
                
                
              
    


