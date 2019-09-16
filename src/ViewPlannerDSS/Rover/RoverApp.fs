namespace ViewPlanner.Rover

open System
open Aardvark.Base

module RoverApp =
    open Aardvark.UI
    open Aardvark.UI.Primitives

    let pan (r:RoverModel) (view:CameraView) (pos:V3d)=
        
        let roverPos = r.position
        let forward = view.Forward
        let up = r.up //rotate around global up axis
        let panRotation = Rot3d(up, r.pan.delta.RadiansFromDegrees())
        let rotatedForward = panRotation.TransformDir(forward)
        
        let distanceVec = pos - roverPos
        let rotatedDistanceV = panRotation.TransformDir(distanceVec)
        let newPos = roverPos + rotatedDistanceV

        newPos, CameraView.look newPos rotatedForward.Normalized up
  

    let panning (r:RoverModel) (camType:CameraType) =
        
        let newR = 
            match camType with
            | HighResCam ->
                let view = r.HighResCam.cam.camera.view
                let pos = r.HighResCam.cam.position
                let _, newView = pan r view pos
                {r with HighResCam = {r.HighResCam with cam = {r.HighResCam.cam with camera = {r.HighResCam.cam.camera with view = newView }}}}

            | WACLR -> 
                let camL = r.WACLR.camL
                let camR = r.WACLR.camR
                let newPosL, roverPanLeftCam = pan r camL.camera.view camL.position
                let newPosR, roverPanRightCam = pan r camR.camera.view camR.position
                {r with WACLR = {r.WACLR with camL = {r.WACLR.camL with camera = {r.WACLR.camL.camera with view = roverPanLeftCam }; position = newPosL }; camR = {r.WACLR.camR with camera = {r.WACLR.camR.camera with view = roverPanRightCam }; position = newPosR } } } 

        newR

    let tilt (r:RoverModel) (view:CameraView) (pos:V3d) =
        let forward = view.Forward
        let right = view.Right
        let tiltRotation = Rot3d(right, r.tilt.delta.RadiansFromDegrees())
        let targetWithTilt = tiltRotation.TransformDir(forward).Normalized
        CameraView.look pos targetWithTilt view.Up
  
    
    let tilting (r:RoverModel) (camType:CameraType) =
        
        let newR = 
            match camType with
            | HighResCam ->
                let view = r.HighResCam.cam.camera.view
                let pos = r.HighResCam.cam.position
                let newView = tilt r view pos
                {r with HighResCam = {r.HighResCam with cam = {r.HighResCam.cam with camera = {r.HighResCam.cam.camera with view = newView }}}}

            | WACLR -> 
                let camL = r.WACLR.camL
                let camR = r.WACLR.camR
                let roverPanLeftCam = tilt r camL.camera.view camL.position
                let roverPanRightCam = tilt r camR.camera.view camR.position
                {r with WACLR = {r.WACLR with camL = {r.WACLR.camL with camera = {r.WACLR.camL.camera with view = roverPanLeftCam }}; camR = {r.WACLR.camR with camera = {r.WACLR.camR.camera with view = roverPanRightCam } } } } 

        newR




   
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


        

        



    //let checkROIFullyInside (m:RoverModel) (region:Option<plist<V3d>>) =
        
    //    //calculate center point of region
    //    match region with 
    //        | None -> m
    //        | Some region -> 

    //            let sum = region.Sum()
    //            let c = region |> PList.count
    //            let centerpoint = sum / (float c)

    //    //calculate new view matrix by tilting and panning to center point
    //            let viewM = m.camera.view.ViewTrafo
    //            let iProj = viewM.Forward.TransformPos centerpoint
    //            let tiltAngle = atan2 -iProj.Y -iProj.Z
    //            let panAngle = atan2 iProj.X -iProj.Z
    //            //let rotTrafo = Trafo3d.Rotation(tiltAngle, panAngle, 0.0)
    //            let rotTrafo = Trafo3d.Rotation(0.0, panAngle, 0.0)
    //            let viewM2 = CameraView.ofTrafo (m.camera.view.ViewTrafo * rotTrafo)

    //    //transform region points to projection space
    //            let projM = Frustum.projTrafo(m.frustum)
    //            let viewProj = viewM2.ViewTrafo * projM
    //            let transformedpoints = region |> PList.toList |> List.map (fun p -> viewProj.Forward.TransformPosProj p) 
    //    //let normPoints = transformedpoints |> List.map(fun p -> ((V2d(p.X, p.Y) + V2d.One) * 0.5))
        
    //    //check if all of the points have values between 0 and 1 
    //    //let allInside = List.forall (fun (point:V2d) -> (( point.X > 0.0 && point.X < 1.0) && ( point.Y > 0.0 && point.Y < 1.0)) ) normPoints
    //            let allInside = List.forall (fun (point:V3d) -> (( point.X > -1.0 && point.X  < 1.0) && ( point.Y > -1.0 && point.Y < 1.0)) ) transformedpoints

    //            let points = region |> PList.toList
    //   //if true then ROI fits in frustum
    //            match allInside with
    //                | true -> {m with camera =  {m.camera with view = viewM2} }
    //                | false -> m//calcPanTiltValues m points viewM2
            
    
    //panR = number of required pans; tiltR = number of required tilts per pan
    //TODO: incoorporate possible overlap between pans
    let rec buildList (l:List<V2d>) (panR:int) (tiltR:int) (originalTiltR:int) (deltaPan:float) (deltaTilt:float) =
        match panR,tiltR with 
            | (p,t) when  p = 0 && t = 0 -> l
            | (p,t) when  p >= 0 && t > 0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let newTilt = lastItem.Y + deltaTilt
                        let listItem = [V2d(lastItem.X, newTilt)]
                        let newList = List.append l listItem
                        buildList newList panR (tiltR-1) originalTiltR deltaPan deltaTilt 
            | (p,t) when p > 0 && t = 0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let newPan = lastItem.X + deltaPan
                        let newDeltaTilt = deltaTilt * (-1.0) 
                        let listItem = [V2d(newPan, lastItem.Y)]
                        let newList = List.append l listItem
                        buildList newList (panR-1) originalTiltR originalTiltR deltaPan newDeltaTilt 
            | _,_ -> l //this case should never be reached
                
    
    //test with interpolation
    //panC, tilt C --> interpolation coefficients
    let rec buildList2 (l:List<V2d>) (inputPan:List<V2d>) (inputTilt:List<V2d>) (panR:int) (tiltR:int) (panC:float) (tiltC:float)=
        match panC,tiltC with 
            | (p,t) when  p >= 1.0 && t >= 1.0 -> l
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
                        buildList2 newList inputPan inputTilt panR tiltR panC newTiltC
            | (p,t) when p < 1.0 && t >= 1.0 -> 
                        let lastItem = l.Item(l.Length-1)
                        let p1 = inputPan.Item(0)
                        let p2 = inputPan.Item(1)
                        let ip = 1.0 / (float(panR))
                        let newpanC = panC + ip
                        let interpolated = p1 * (1.0-newpanC) + p2 * newpanC
                        let newList = List.append l [V2d(interpolated.X, lastItem.Y)]
                        //new input list
                        let newInputTilt = [inputTilt.Item(1); inputTilt.Item(0)]
                        buildList2 newList inputPan newInputTilt panR tiltR newpanC 0.0
            | _,_-> l 

    //combination between buildList1 and 2
    //interpolation with indices
    let rec buildList3 (l:List<V2d>) (inputPan:List<V2d>) (inputTilt:List<V2d>) (panR:int) (tiltR:int) (origTilt:int) (panC:float) (tiltC:float) =
        match panR,tiltR with 
            | (p,t) when  p = 0 && t = 0 -> l

            | (p,t) when  p >= 0 && t > 0 -> //decrease tilt
               let lastItem = l.Item(l.Length-1)
               let currPan = lastItem.X
               let ref1 = V2d(currPan, inputTilt.Item(0).Y)
               let ref2 = V2d(currPan, inputTilt.Item(1).Y)
               //calculate interpolation coefficient
               let i = if tiltR = 1 then 0.5 else 1.0 / (float(tiltR))
               let newTiltC = tiltC + i
               let interpolated = ref1 * (1.0-newTiltC) + ref2 * newTiltC                     
               let newList = List.append l [interpolated]
               buildList3 newList inputPan inputTilt panR (tiltR-1) origTilt panC newTiltC         


            | (p,t) when p > 0 && t = 0 -> //decrease pan; reset tilt
               let lastItem = l.Item(l.Length-1)
               let p1 = inputPan.Item(0)
               let p2 = inputPan.Item(1)
               let ip = 1.0 / (float(panR))
               let newpanC = panC + ip
               let interpolated = p1 * (1.0-newpanC) + p2 * newpanC
               let newList = List.append l [V2d(interpolated.X, lastItem.Y)]
               let newInputTilt = [inputTilt.Item(1); inputTilt.Item(0)]
               buildList3 newList inputPan newInputTilt (panR-1) origTilt origTilt newpanC 0.0    

            | _,_ -> l //this case should never be reached


    
    //calculate the required energy and time for performing the pans/tilts for sampling
    let rec calculateOutputVars (rover : RoverModel) (values:List<V2d>) (counter:int) (sum:float) (cost:float)  = 

        match counter with 

        | 0 -> 
            let currPan = rover.pan.current
            let currTilt = rover.tilt.current
            let next = values.Item(0)
            let deltaPan = Math.Abs(next.X - currPan)
            let deltaTilt = Math.Abs(next.Y - currTilt)
            let e = deltaPan * cost + deltaTilt * cost
            (sum + e)
        
        | _ -> 
            let curr = values.Item(counter)
            let prev = values.Item(counter-1)
            let deltaPan = Math.Abs(curr.X - prev.X)
            let deltaTilt = Math.Abs(curr.Y - prev.Y)
            let e = deltaPan * cost + deltaTilt * cost
            calculateOutputVars rover values (counter-1) (sum+e) cost 

       

    //takes pan and tilt values and calculates a view matrix for frustum visualisation
    let calculateViewMatrix (rover : RoverModel) (pan : float) (tilt : float) (cam:CamVariables) =
        
        let panCurr = rover.pan.current

        let signCurr = Math.Sign(panCurr)
        let signTargetValue = Math.Sign(pan)

        let panDelta = 
            match signCurr, signTargetValue with
            | -1, 1 ->                                      //from negative side to positive side
                let sum = Math.Abs(panCurr) + Math.Abs(pan)
                if sum > 180.0 then (sum - 180.0) * (-1.0) else (sum*(-1.0))
                    
            | 1, -1 ->                                      //from positive side to negative side
                let sum = Math.Abs(panCurr) + Math.Abs(pan)
                if sum > 180.0 then (sum - 180.0) else sum
                
                


            | _,_ -> (panCurr - pan)                        //both values on the same side


        let tiltCurr = rover.tilt.current
        let tiltDelta = tiltCurr - tilt

        let forward = cam.camera.view.Forward
        let up = rover.up 

        //panning
        let panRotation = Rot3d(up, panDelta.RadiansFromDegrees())
        let targetWithPan = panRotation.TransformDir(forward)

        let pos = cam.position
        let roverPos = rover.position
        let distanceVec = pos - roverPos
        let rotatedDistanceV = panRotation.TransformDir(distanceVec)
        let newPos = roverPos + rotatedDistanceV

        //tilting
        //new right Vec
        let newView = CameraView.look newPos targetWithPan.Normalized up
        let newRight = newView.Right
        let tiltRotation = Rot3d(newRight, tiltDelta.RadiansFromDegrees())

        let targetWithTilt = tiltRotation.TransformDir(targetWithPan)

        let view = CameraView.look newPos targetWithTilt.Normalized up

        view

    
    let createNewViewPlan (camvars:plist<CamVariables>) (p:Placement) (camtype: string) (rover:RoverModel) =

        let output = 
                let cam = camvars |> PList.first
                let values = cam.samplingValues |> PList.toList
                let numSamples = values.Length
                let energy = 
                    let res = calculateOutputVars rover values (numSamples-1) 0.0 rover.energyForPanTilt 
                    Math.Round(res, 2)

                let time = 
                    let res = calculateOutputVars rover values (numSamples-1) 0.0 rover.timeForPanTilt
                    Math.Round(res,2)
                
                {
                numberOfSamples = numSamples
                energyRequired = energy
                timeRequired = time
                bandwidthRequired = 0.0
                }

        let id = 
            let plans = rover.viewplans |> PList.toList
            plans.Length + 1

        let vp = 
            {
            id = id
            instrument = camtype
            placement = p
            cameraVariables = camvars
            thetaPhiValues = rover.thetaPhiValues
            projPoints = rover.projPoints
            panOverlap = rover.panOverlap
            tiltOverlap = rover.tiltOverlap
            outputParams = output
            }

        vp



    

    let sampling (p : Placement) (rover : RoverModel) = 
        
        let panOverlap = rover.panOverlap
        let tiltOverlap = rover.tiltOverlap
        let currentCamera = rover.camera

        //let fov = rover.fov
        let values = rover.thetaPhiValues
        let li = values |> PList.toList
        let pans = li |> List.map (fun l -> l.X) //list with just pan values
        
        //let min = pans |> Seq.indexed |> Seq.min
        //let idx = fst min

        let tilts = li |> List.map (fun l -> l.Y) //list with just tilt values

      

        //let panRef = fov * (1.0 - (panOverlap/100.0))
        //let tiltRef = fov * (1.0 - (tiltOverlap/100.0))


        //sort pan values
        let sortedPans = List.sort pans
        
        let minPan = sortedPans.Head
        let maxPan = sortedPans.Item(sortedPans.Length - 1)
        let deltaPan = Math.Abs (maxPan - minPan)
        
        //let panningRate = int(Math.Round(deltaPan / panRef))
        //printfn "pan delta %A rate %A " deltaPan panningRate

        //sort tilt values
        let sortedTilts = List.sort tilts
        let minTilt = sortedTilts.Head
        let maxTilt = sortedTilts.Item(sortedTilts.Length - 1)
        let deltaTilt = maxTilt - minTilt
    
    
        //let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / (tiltRef))) 
        //printfn "tilt delta %A rate %A " deltaTilt tiltingRate

        //generate a sampling list with pan and tilt values
        let firstPair = V2d(minPan, maxTilt) //pair with min pan value and max tilt value (equals left bottom corner of bounding box)
        let samplingValues = [firstPair] //initial list
        
        let newRover = 
         match currentCamera with
            | HighResCam -> 
                let camera = rover.HighResCam
                let fov = camera.cam.frustum |> Frustum.horizontalFieldOfViewInDegrees
                let panRef = fov * (1.0 - (panOverlap/100.0))
                let tiltRef = fov * (1.0 - (tiltOverlap/100.0))
                let panningRate = int(Math.Round(deltaPan / panRef))
                let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / (tiltRef)))
                let values = buildList samplingValues panningRate tiltingRate tiltingRate panRef -tiltRef
                let sv = values |> PList.ofList

                //for visualising footprints
                let viewMatrices = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.cam) |> PList.ofList

                let HR = {rover.HighResCam with cam = { rover.HighResCam.cam with samplingValues = sv; viewList = viewMatrices }}

                let camVars = PList.ofList [HR.cam]
                let newVP = createNewViewPlan camVars p "High Resolution Camera" rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with HighResCam = HR; viewplans = newViewPlanList} //numberOfSamples = numberOfSamples; energyRequired = energy; timeRequired = time}

            | WACLR -> 
                let camera = rover.WACLR
                let fov = camera.camL.frustum |> Frustum.horizontalFieldOfViewInDegrees //equal for both left and right cams
                let ol = camera.overlapBetweenCams
                let panRef = ((fov+fov) - ol) * (1.0 - (panOverlap/100.0))
                let tiltRef = fov * (1.0 - (tiltOverlap/100.0))
                let panningRate = int(Math.Round(deltaPan / panRef))
                let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / (tiltRef)))
                let values = buildList samplingValues panningRate tiltingRate tiltingRate panRef -tiltRef
                let sv = values |> PList.ofList

                let viewMatricesLeft = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.camL) |> PList.ofList
                let viewMatricesRight = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.camR) |> PList.ofList

                //leftcam
                let cL = {rover.WACLR.camL with samplingValues = sv; viewList = viewMatricesLeft }
                //rightcam
                let cR = {rover.WACLR.camR with samplingValues = sv; viewList = viewMatricesRight }

                let st = {rover.WACLR with camL = cL; camR = cR}

                //{rover with WACLR = st}

                let camVars = PList.ofList [cL; cR]
                let newVP = createNewViewPlan camVars p "WACLR" rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with WACLR = st; viewplans = newViewPlanList}

                
        newRover




        //let samplings = buildList samplingValues panningRate tiltingRate tiltingRate panRef -tiltRef

        //samplings
       



    let moveFrustum (m:RoverModel) = 
       
        //let v = checkROIFullyInside m m.reg
        //v
        m
   




    
    let rotateToPoint (rover:RoverModel) =
        
        let selectedViewPlan = rover.selectedViewPlan

        match selectedViewPlan with
        | Some plan ->
            let camvar = plan.cameraVariables |> PList.first
            let length = camvar.viewList |> PList.toList |> List.length
            let idx = rover.walkThroughIdx
            let lastIdx = length - 1
            let newIdx = 
                if idx = lastIdx then 0 else (idx+1)

            {rover with walkThroughIdx = newIdx}
            
        | None -> rover


        //let currentCamera = rover.camera

        //let r = 
        // match currentCamera with
        // | HighResCam -> 
        //   let c = rover.HighResCam.cam
        //   let values = c.samplingValues
        //   let li = values |> PList.toList
        //   let idx = rover.HighResCam.currIdx
        //   let pair = values.Item(idx)

        //   let panned = setPan rover pair.X
        //   let pannedRover = panning panned currentCamera
        //   let tilted = setTilt pannedRover pair.Y
        //   let ro = tilting tilted currentCamera

        //   let lastIdx = li.Length - 1
        //   let newIdx = 
        //    if idx = lastIdx then 0 else (idx+1)
           
        //   {ro with HighResCam = { ro.HighResCam with currIdx = newIdx}}

        // | WACLR ->  
        //    let c = rover.WACLR.camL
        //    let values = c.samplingValues
        //    let li = values |> PList.toList
        //    let idx = rover.WACLR.currIdx
        //    let pair = values.Item(idx)

        //    let panned = setPan rover pair.X
        //    let pannedRover = panning panned currentCamera
        //    let tilted = setTilt pannedRover pair.Y
        //    let ro = tilting tilted currentCamera

        //    let lastIdx = li.Length - 1
        //    let newIdx = 
        //     if idx = lastIdx then 0 else (idx+1)
           
        //    {ro with WACLR = { ro.WACLR with currIdx = newIdx}}


        
        //r



     //create new cameraviews for stereo cam
    let updateStereoCam (forward:V3d) (pos:V3d) (r:RoverModel)=

      let rightV = (forward.Normalized).Cross(r.up)
      let shift = rightV * 0.3
      let positionCamL = pos - shift
      let positionCamR = pos + shift
      let forwardL = forward - shift
      let forwardR = forward + shift
      let camViewL = CameraView.look positionCamL forwardL.Normalized r.up
      let camViewR = CameraView.look positionCamR forwardR.Normalized r.up
   
      let camStateL = {r.WACLR.camL.camera with view = camViewL}
      let camVarL = {r.WACLR.camL with camera = camStateL; position = positionCamL }

      let camStateR = {r.WACLR.camR.camera with view = camViewR}
      let camVarR = {r.WACLR.camR with camera = camStateR; position = positionCamR }
       
      (camVarL, camVarR)
    

    let setCamera (rover:RoverModel) =
        
        let currentCam = rover.camera
        let pos = rover.position
        let tar = rover.target
        let up = rover.up

        match currentCam with
            | HighResCam -> 
                let forward = tar - pos
                let cam = CameraView.look pos forward.Normalized up
                let view = {rover.HighResCam.cam.camera with view = cam}
                let vars = {rover.HighResCam.cam with position = pos; camera = view}
                let hr = {rover.HighResCam with cam = vars}
                {rover with HighResCam = hr}

            | WACLR -> 
                let forward = (tar - pos)
                let vars = updateStereoCam forward pos rover
                let camL = fst vars
                let camR = snd vars
                let stc = {rover.WACLR with camL = camL; camR = camR}
                {rover with  WACLR = stc}







       
    let calculateValues (rover:RoverModel) =
        
        let region = rover.reg
        let selectedPos = rover.selectedPosition
        let newRover = 
         match region,selectedPos with
            | Some reg, Some p -> 
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
                printfn "thetaOnForward %A phiOnForward %A"  thetaPhi.X thetaPhi.Y
                let setR = initializePan rover thetaPhi.X
                let setR2 = initializeTilt setR thetaPhi.Y


                //set the camera according to the selected placement
                let roverWithupdatedCam = setCamera setR2

                let r = {roverWithupdatedCam with thetaPhiValues = plist; projPoints = projectionPoints}
                sampling p r

            | _,_ -> rover
        
        newRover
        //let roverWithSampling = sampling newRover
        //roverWithSampling
        
      
    let changeCam (rover:RoverModel) (camtype:Option<CameraType>)=
        
        match camtype with
            | Some cam -> 
                match cam with 
                |HighResCam -> 
                 {rover with currentCamType = Some HighResCam; camera = HighResCam}

                |WACLR ->
                 {rover with currentCamType = Some WACLR; camera = WACLR}

            | None -> rover
            

    

    let changePanOverlap (rover:RoverModel) (overlap:Option<Overlap>) =
        
        match overlap with
        | Some Percent_20 -> {rover with panOverlap = 20.0; currentPanOverlap = Some Percent_20}
        | Some Percent_30 -> {rover with panOverlap = 30.0; currentPanOverlap = Some Percent_30}
        | Some Percent_40 -> {rover with panOverlap = 40.0; currentPanOverlap = Some Percent_40}
        | Some Percent_50 -> {rover with panOverlap = 50.0; currentPanOverlap = Some Percent_50}
        | None -> rover

    let changeTiltOverlap (rover:RoverModel) (overlap:Option<Overlap>) = 
         
         match overlap with
         | Some Percent_20 -> {rover with tiltOverlap = 20.0; currentTiltOverlap = Some Percent_20}
         | Some Percent_30 -> {rover with tiltOverlap = 30.0; currentTiltOverlap = Some Percent_30}
         | Some Percent_40 -> {rover with tiltOverlap = 40.0; currentTiltOverlap = Some Percent_40}
         | Some Percent_50 -> {rover with tiltOverlap = 50.0; currentTiltOverlap = Some Percent_50}
         | None -> rover

    

   



    let setRoverPosAndTarget (id:int) (rover:RoverModel) = 
        
        //let currentCam = rover.camera
        //let up = rover.up
        let positions = rover.positionsList
        let selectedLoc = positions |> Seq.tryFind (fun place -> place.id = id)

        match selectedLoc with
            | Some placement -> 
                let pos = placement.position
                let tar = placement.target
                let projSphere = {rover.projsphere with position = pos}
                {rover with position = pos; target = tar; projsphere = projSphere; selectedPosition = Some placement}
            
            | None -> rover


        //match selectedLoc with
        //| Some placement -> 
        //    let pos = (placement.position + up)
        //    let tar = placement.target

        //    match currentCam with
        //    | HighResCam -> 
        //        let forward = tar - pos
        //        let cam = CameraView.look pos forward.Normalized up
        //        let view = {rover.HighResCam.cam.camera with view = cam}
        //        let vars = {rover.HighResCam.cam with position = pos; camera = view}
        //        let hr = {rover.HighResCam with cam = vars}
        //        let projSphere = {rover.projsphere with position = pos}
        //        {rover with position = pos; target = tar; HighResCam = hr; projsphere = projSphere; selectedPosition = Some placement}

        //    | WACLR -> 
        //        let forward = (tar - pos)
        //        let projSphere = {rover.projsphere with position = pos}
        //        let vars = updateStereoCam forward pos rover
        //        let camL = fst vars
        //        let camR = snd vars
        //        let stc = {rover.WACLR with camL = camL; camR = camR}
        //        {rover with position = pos; target = tar; WACLR = stc; projsphere = projSphere; selectedPosition = Some placement}

        //| None -> rover

    
    //TODO
    let showViewPlan (id:int) (rover:RoverModel) = 
        
        let viewplans = rover.viewplans
        let selectedVP = viewplans |> Seq.tryFind (fun place -> place.id = id)

        match selectedVP with 
        | Some viewplan ->
            
            //let camVars = viewplan.cameraVariables
            //let size = camVars |> PList.toList |> List.length
            //let camtype = if size = 1 then HighResCam else WACLR
            //let placement = viewplan.placement

            //TODO
            {rover with selectedViewPlan = Some viewplan}





        | None -> rover




        








    let update (rover:RoverModel) (action:RoverAction) =
        
        match action with
            | ChangePosition newPos -> {rover with position = newPos} 

            | ChangePan p -> 
                let rover' = setPan rover p
                panning rover' rover.camera

            | ChangeTilt t -> 
                let rover' = setTilt rover t
                tilting rover' rover.camera
            
            | MoveToRegion  ->
                moveFrustum rover 
            
            | SwitchCamera cam ->
                changeCam rover cam
            
            |CalculateAngles ->
                calculateValues rover
            
            | RotateToPoint ->
                rotateToPoint rover
            
            | ChangePanOverlap o ->
                changePanOverlap rover o
            
            | ChangeTiltOverlap o ->
                changeTiltOverlap rover o
            
            | SetRoverPosAndTarget id ->
                setRoverPosAndTarget id rover
            
            | ShowViewPlan id ->
                showViewPlan id rover
                
                
                
                
              
    


