namespace ViewPlanner.Rover

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

module RoverApp =

    open Aardvark.SceneGraph
    open FShade

    open Aardvark.Application

    open Aardvark.UI
    open Aardvark.UI.Primitives

    
    //let pan (r:RoverModel) (view:CameraView) (pos:V3d)=
        
    //    let roverPos = r.position
    //    let forward = view.Forward
    //    let up = r.up //rotate around global up axis
    //    let panRotation = Rot3d(up, r.pan.delta.RadiansFromDegrees())
    //    let rotatedForward = panRotation.TransformDir(forward)
        
    //    let distanceVec = pos - roverPos
    //    let rotatedDistanceV = panRotation.TransformDir(distanceVec)
    //    let newPos = roverPos + rotatedDistanceV

    //    newPos, CameraView.look newPos rotatedForward.Normalized up
  

    //let panning (r:RoverModel) (camType:CameraType) =
        
    //    let newR = 
    //        match camType with
    //        | HighResCam ->
    //            let view = r.HighResCam.cam.camera.view
    //            let pos = r.HighResCam.cam.position
    //            let _, newView = pan r view pos
    //            {r with HighResCam = {r.HighResCam with cam = {r.HighResCam.cam with camera = {r.HighResCam.cam.camera with view = newView }}}}

    //        | WACLR -> 
    //            let camL = r.WACLR.camL
    //            let camR = r.WACLR.camR
    //            let newPosL, roverPanLeftCam = pan r camL.camera.view camL.position
    //            let newPosR, roverPanRightCam = pan r camR.camera.view camR.position
    //            {r with WACLR = {r.WACLR with camL = {r.WACLR.camL with camera = {r.WACLR.camL.camera with view = roverPanLeftCam }; position = newPosL }; camR = {r.WACLR.camR with camera = {r.WACLR.camR.camera with view = roverPanRightCam }; position = newPosR } } } 

    //    newR

    //let tilt (r:RoverModel) (view:CameraView) (pos:V3d) =
    //    let forward = view.Forward
    //    let right = view.Right
    //    let tiltRotation = Rot3d(right, r.tilt.delta.RadiansFromDegrees())
    //    let targetWithTilt = tiltRotation.TransformDir(forward).Normalized
    //    CameraView.look pos targetWithTilt view.Up
  
    
    //let tilting (r:RoverModel) (camType:CameraType) =
        
    //    let newR = 
    //        match camType with
    //        | HighResCam ->
    //            let view = r.HighResCam.cam.camera.view
    //            let pos = r.HighResCam.cam.position
    //            let newView = tilt r view pos
    //            {r with HighResCam = {r.HighResCam with cam = {r.HighResCam.cam with camera = {r.HighResCam.cam.camera with view = newView }}}}

    //        | WACLR -> 
    //            let camL = r.WACLR.camL
    //            let camR = r.WACLR.camR
    //            let roverPanLeftCam = tilt r camL.camera.view camL.position
    //            let roverPanRightCam = tilt r camR.camera.view camR.position
    //            {r with WACLR = {r.WACLR with camL = {r.WACLR.camL with camera = {r.WACLR.camL.camera with view = roverPanLeftCam }}; camR = {r.WACLR.camR with camera = {r.WACLR.camR.camera with view = roverPanRightCam } } } } 

    //    newR

    let initializeTilt (m:RoverModel) (value:float) = 
        {m with tilt = {m.tilt with previous = value; current = value}}
    
    let initializePan (m:RoverModel) (value:float) = 
        {m with pan = {m.pan with previous = value; current = value}}

    //let setPan(m:RoverModel) (value:float) =
    //    let dt = m.pan.previous - value
    //    let prev = value
    //    //let prev = m.pan.current
    //    let curr = value
    //    {m with pan = {m.pan with delta = dt; previous = prev; current = curr}}


    //let setTilt(m:RoverModel) (value:float) =
    //    let dt = m.tilt.previous - value
    //    let prev = value
    //    //let prev = m.tilt.current
    //    let curr = value
    //    {m with tilt = {m.tilt with delta = dt; previous = prev; current = curr}}
    
    
    let rotateIntoCoordinateSystem (m:RoverModel) (vector:V3d) = 
        
        let rotZ = Trafo3d.RotateInto(m.up,V3d.OOI)
        let rotatedbyZ = rotZ.Forward.TransformDir vector
        rotatedbyZ
    
        
    //returns theta, phi values in degrees
    let calcThetaPhi (position:V3d) =

        let theta = (atan2 position.X position.Y)* Constant.DegreesPerRadian 
        let phi = (acos(position.Z)) * Constant.DegreesPerRadian

        //bring theta to interval [0, 360]
        let thetaShifted = 
            if theta < 0.0 then
                theta * (-1.0)
            else 
                let delta = 180.0 - theta
                180.0 + delta

        V2d(thetaShifted,phi)
   

    let rec buildList (l:List<V2d>) (panR:int) (tiltR:int) (originalTiltR:int) (deltaPan:float) (deltaTilt:float) (cross360:bool)=
        match panR,tiltR with 
        | (p,t) when  p = 0 && t = 0 -> l
        | (p,t) when  p >= 0 && t > 0 -> 
                    let lastItem = l.Item(l.Length-1)
                    let newTilt = lastItem.Y + deltaTilt
                    let listItem = [V2d(lastItem.X, newTilt)]
                    let newList = List.append l listItem
                    buildList newList panR (tiltR-1) originalTiltR deltaPan deltaTilt cross360
        | (p,t) when p > 0 && t = 0 -> 
                    let lastItem = l.Item(l.Length-1)
                    let newPan = 
                        if cross360 then                    //if the 0/360 point will be crossed at some point
                            let p = lastItem.X + deltaPan
                            let newP = 
                                if (p < 0.0) then 
                                    360.0 - p
                                else p
                            newP

                        else lastItem.X + deltaPan
                                
                    let newDeltaTilt = deltaTilt * (-1.0) 
                    let listItem = [V2d(newPan, lastItem.Y)]
                    let newList = List.append l listItem
                    buildList newList (panR-1) originalTiltR originalTiltR deltaPan newDeltaTilt cross360
        | _,_ -> l //this case should never be reached
                
    
    
    //calculate the required energy and time for performing the pans/tilts for sampling
    let rec calculateOutputVars (values:List<V2d>) (counter:int) (sum:float) (cost:float) (rover : RoverModel)  = 

        match counter with 
        | 0 -> 
            let next = values.Item(0)
            let deltaPan = 
                let d = Math.Abs(next.X - rover.pan.current)
                if d > 180.0 then 360.0 - d else d

            let deltaTilt = Math.Abs(next.Y - rover.tilt.current)
            let e = deltaPan * cost + deltaTilt * cost
            (sum + e)
        | _ -> 
            let curr = values.Item(counter)
            let prev = values.Item(counter-1)
            let deltaPan = 
                let d = Math.Abs(curr.X - prev.X)
                if d > 180.0 then 360.0 - d else d

            let deltaTilt = Math.Abs(curr.Y - prev.Y)
            let e = deltaPan * cost + deltaTilt * cost
            calculateOutputVars values (counter-1) (sum+e) cost rover


    let calculateDataSize (numberOfSamples:int) (rover:RoverModel) =
  
        let pixel = rover.horzRes * rover.vertRes
        let bitPerPicture = (int)pixel * rover.colorDepth * rover.numberOfColorChannels
        let totalByte = (bitPerPicture * numberOfSamples) / 8        
        let totalKbyte = totalByte / 1024
        let totalMByte = totalKbyte / 1024
        totalMByte



    let calculateViewMatrix  (rover : RoverModel) (pan : float) (tilt : float) (cam:CamVariables) =

        let panDelta = 
            let d = pan - rover.pan.current
            let sign = float (Math.Sign(d))
            let dA = Math.Abs(d)
            let p = if dA > 180.0 then dA - 360.0 else dA
            p * sign
                
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

        CameraView.look newPos targetWithTilt.Normalized up


    
    let createNewViewPlan (camvars:plist<CamVariables>) (p:Placement) (camtype: string) (spatialRes:float) (rover:RoverModel) =

        let output = 
            let cam = camvars |> PList.first
            let values = cam.samplingValues |> PList.toList
            let numSamples = values.Length
                
            let energy = 
                let res = calculateOutputVars values (numSamples-1) 0.0 rover.energyForPanTilt rover
                Math.Round(res, 2)

            let time = 
                let res = calculateOutputVars values (numSamples-1) 0.0 rover.timeForPanTilt rover
                Math.Round(res,2)
                
            let countSamples = 
                let count = numSamples
                if camtype = "WACLR" then count * 2 else count

            let datasize = calculateDataSize countSamples rover

            {
            numberOfSamples = countSamples
            energyRequired = energy
            timeRequired = time
            datasize = datasize
            spatialRes = spatialRes
            }


        {
            id = rover.viewplans.Count + 1
            instrument = camtype
            placement = p
            cameraVariables = camvars
            thetaPhiValues = rover.thetaPhiValues
            projPoints = rover.projPoints
            panOverlap = rover.panOverlap
            tiltOverlap = rover.tiltOverlap
            outputParams = output
        }


    let selectFromList (values:alist<float*int>) (quadrant:int) =
        
        values |> AList.choose (fun v -> 
                                    let q = snd v
                                    let p = fst v
                                    match v with
                                    | v when q = quadrant -> Some p
                                    | _ -> None 

                                )
        |> AList.toList
        |> List.sort


    let calculatePanValues (pans:list<float>) =
        
        let sorting = 
            alist {
                for pan in pans do
                    
                    //check in which quadrant the value lies
                    let quadrant = 
                        match pan with
                        | p when ((p >= 0.0 && p < 90.0) || p = 360.0) -> 1
                        | p when (p >= 90.0 && p < 180.0) -> 2
                        | p when (p >= 180.0 && p < 270.0) -> 3
                        | p when (p >= 270.0 && p < 360.0) -> 4
                        | _ -> failwith "invalid value"
                    
                    yield (pan, quadrant)
            }
        
        //create a list for each quadrant
        let q1 = selectFromList sorting 1 
        let q2 = selectFromList sorting 2
        let q3 = selectFromList sorting 3
        let q4 = selectFromList sorting 4

        //check for problematic scenarios
        //scenario 1: Q1 and Q4
        //scenario 2: Q1 and Q4 and Q3
        //scenario 3: Q1 and Q2 and Q4

        let minPan, maxPan, cross0_360 = 
            
            let q1empty = q1.IsEmpty
            let q2empty = q2.IsEmpty
            let q3empty = q3.IsEmpty
            let q4empty = q4.IsEmpty

            match q1empty,q2empty,q3empty,q4empty with
            | false, true, true, false -> 
                let minPan = q1.Item(q1.Length - 1)
                let maxPan = q4.Head
                (minPan,maxPan, true)
            
            | false, true, false, false -> 
                let minPan = q1.Item(q1.Length - 1)
                let maxPan = q3.Head
                (minPan,maxPan, true)
            
            | false, false, true, false ->
                let minPan = q2.Item(q1.Length - 1)
                let maxPan = q4.Head
                (minPan,maxPan, true)
            
            | _, _, _, _ -> 
                let sortedPans = pans |> List.sort
                let minPan = sortedPans.Head
                let maxPan = sortedPans.Item(sortedPans.Length - 1)
                (minPan,maxPan, false)

        (minPan,maxPan,cross0_360)



    let linearization (value:float32) (zNear:float32) (zFar:float32) = 
        
        let two = float32 2.0
        let one = float32 1.0
        let zn = two * value - one
        two * zNear * zFar / (zFar + zNear - zn * (zFar - zNear))

    
    let pixelSizeCm (plane:float) (horzRes:float) (vertRes:float) (fovH:float) (fovV:float) = 
        
        let rad = fovV.RadiansFromDegrees()
        let sizeWorld = (tan(rad/2.0) * plane)*2.0
        (sizeWorld / vertRes) *100.0      

        //test
        //let fH = Math.Round((fovH), 0)
        //let rad2 = fH.RadiansFromDegrees()
        //let sizeWorld2 = (tan(rad2/2.0) * plane)*2.0
        //let width = (sizeWorld2 / horzRes) *100.0 
        //let test = width

    
    let interpolatePixelSize (min:float) (max:float) (value:float) (sizeNear:float) (sizeFar:float) = 
        
        let normalized = (value - min) / (max-min) // [0,1]
        //let interpolatedSize = Fun.Lerp(normalized, sizeNear, sizeFar)
        sizeNear * (1.0 - normalized) + sizeFar * normalized
   

    let median (arr:float[]) =
         
        let length = arr.Length
        match length with
        | 1 -> arr.[0]
        | 2 -> 
            let v1 = arr.[0]
            let v2 = arr.[1]
            (v1 + v2) / 2.0
        | _ -> 
            let rest = length % 2
            let idx = int (Math.Floor((float length)/2.0))

            match rest with
            | 0 -> 
                let idx2 = idx + 1
                let v1 = arr.[idx]
                let v2 = arr.[idx2]
                (v1 + v2) / 2.0
            | _ -> 
                arr.[idx]


    let calculateDpcm (runtimeInstance: IRuntime) (renderSg :ISg<_>) (frustum:Frustum) (view:CameraView) (rover:RoverModel) =
            
        let horzRes = rover.horzRes
        let vertRes = rover.vertRes
        let size = V2i(horzRes, vertRes)

        let fovH = frustum |> Frustum.horizontalFieldOfViewInDegrees
        let asp = frustum |> Frustum.aspect
        let fovV = Math.Round((fovH / asp), 0)

        let depth = runtimeInstance.CreateTexture(size, TextureFormat.Depth24Stencil8, 1, 1);

        let signature = 
            runtimeInstance.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
            ]

        let fbo = 
            runtimeInstance.CreateFramebuffer(
                signature, 
                Map.ofList [
                    DefaultSemantic.Depth, depth.GetOutputView()
                ]
            )
        
        let projTrafo  = Frustum.projTrafo(frustum);
        let viewTrafo = view.ViewTrafo

        let render2TextureSg =
            renderSg
            |> Sg.viewTrafo (Mod.constant viewTrafo)
            |> Sg.projTrafo (Mod.constant projTrafo)
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo 
                toEffect DefaultSurfaces.diffuseTexture
            ]

        let taskclear = runtimeInstance.CompileClear(signature,Mod.constant C4f.Black,Mod.constant 1.0)
        let task = runtimeInstance.CompileRender(signature, render2TextureSg)
            
        taskclear.Run(null, fbo |> OutputDescription.ofFramebuffer) |> ignore
        task.Run(null, fbo |> OutputDescription.ofFramebuffer) |> ignore

        let mat = Matrix<float32>(int64 size.X, int64 size.Y)
        runtimeInstance.DownloadDepth(depth,0,0,mat)

        let near = float32 frustum.near
        let far = float32 frustum.far
            
        let zView = mat.Data |> Array.map(fun v -> linearization v near far)

        //let pixelSizeCm (plane:float) (horzRes:float) (vertRes:float) (fovH:float) (fovV:float)
        let hR = float (horzRes)
        let vR = float (vertRes)
        let pixelSizeNear = pixelSizeCm frustum.near hR vR fovH fovV
        let pixelSizeFar = pixelSizeCm frustum.far hR vR fovH fovV

        //uncomment to see depth image
        //let pi = PixImage<byte>(Col.Format.RGBA, V2i mat.Size)

        //pi.GetMatrix<C4b>().SetMap(mat, fun v ->
        //    let gray = float v ** 32.0 |> float32
        //    C4f(gray, gray, gray, 1.0f).ToC4b()
        //) |> ignore

        //pi.SaveAsImage(@"C:\Users\schalko\Desktop\depth.png")

        let matPixelSizes = zView |> Array.map(fun v -> interpolatePixelSize frustum.near frustum.far (float v) pixelSizeNear pixelSizeFar)
        let sortedArr = matPixelSizes |> Array.sort |> Array.filter(fun f -> f < frustum.far)
        let median = median sortedArr
        let dpcm = 1.0/median    
        dpcm


        
    let createSamplingList (camera: CamVariables) (values:list<V2d>) (deltaPan:float) (deltaTilt:float) (cross:bool) (rover:RoverModel) = 
        
        let fovH = camera.frustum |> Frustum.horizontalFieldOfViewInDegrees
        let ratio = camera.frustum |> Frustum.aspect
        let fovV = Math.Round((fovH / ratio), 0)

        let panRef = fovH * (1.0 - (rover.panOverlap/100.0))            
        let tiltRef = fovV * (1.0 - (rover.tiltOverlap/100.0))
        let panningRate = int(Math.Round((Math.Abs(deltaPan)) / panRef))
        let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / tiltRef))

        let refPan = if cross then (panRef * -1.0) else panRef

        buildList values panningRate tiltingRate tiltingRate refPan -tiltRef cross


    let sampling (p : Placement) (runtimeInstance: IRuntime) (renderSg : ISg<_>) (rover : RoverModel) = 
        
        let currentCamera = rover.camera
        let li = rover.thetaPhiValues 
        let pans = li |> PList.map (fun l -> l.X) 
        let tilts = li |> PList.map (fun l -> l.Y) 
        let panList = pans |> PList.toList
        let tiltList = tilts |> PList.toList

        let minPan, maxPan, cross360 = calculatePanValues panList

        let deltaPan = 
            match cross360 with
            | true -> 
                let deltaToZero = minPan
                let deltaToMaxFromZero = 360.0 - maxPan
                deltaToZero + deltaToMaxFromZero
            | false -> 
                Math.Abs (maxPan - minPan)

        //sort tilt values
        let sortedTilts = List.sort tiltList
        let minTilt = sortedTilts.Head
        let maxTilt = sortedTilts.Item(sortedTilts.Length - 1)
        let deltaTilt = maxTilt - minTilt
    
        //generate a sampling list with pan and tilt values
        let firstPair = V2d(minPan, maxTilt) //pair with min pan value and max tilt value (equals left bottom corner of bounding box)
        let samplingValues = [firstPair] //initial list
        
        let sampleWithDpi = rover.samplingWithDpi

        let newRover = 
            match currentCamera with
            | HighResCam -> 
                let cam = rover.HighResCam.cam
                let values = createSamplingList cam samplingValues deltaPan deltaTilt cross360 rover |> PList.ofList
                let viewMatrices = values |> PList.map(fun m -> calculateViewMatrix rover m.X m.Y cam)

                let medValue = 
                    if sampleWithDpi then
                        let listOfdpcms = viewMatrices |> PList.map(fun e -> calculateDpcm runtimeInstance renderSg cam.frustum e rover)
                        let median = listOfdpcms |> PList.toArray |> Array.sort |> median 
                        Math.Round(median,2)
                    else 0.0

                let HR = {rover.HighResCam with cam = { rover.HighResCam.cam with samplingValues = values; viewList = viewMatrices }}
                let camVars = PList.ofList [HR.cam]
                let newVP = createNewViewPlan camVars p "High Resolution Camera" medValue rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with HighResCam = HR; viewplans = newViewPlanList} 

            | WACLR -> 
                let cam = rover.WACLR
                let values = createSamplingList cam.camL samplingValues deltaPan deltaTilt cross360 rover |> PList.ofList

                let viewMatricesLeft = values |> PList.map(fun m -> calculateViewMatrix rover m.X m.Y cam.camL) 
                let viewMatricesRight = values |> PList.map(fun m -> calculateViewMatrix rover m.X m.Y cam.camR)

                let cL = {rover.WACLR.camL with samplingValues = values; viewList = viewMatricesLeft }
                let cR = {rover.WACLR.camR with samplingValues = values; viewList = viewMatricesRight }
                let st = {rover.WACLR with camL = cL; camR = cR}

                let medValue = 
                    if sampleWithDpi then
                        let listOfdpcmsLeft = viewMatricesLeft |> PList.map(fun e -> calculateDpcm runtimeInstance renderSg cL.frustum  e rover)
                        let listOfdpcmsRight = viewMatricesRight |> PList.map(fun e -> calculateDpcm runtimeInstance renderSg cR.frustum  e rover)
                        let medianL = listOfdpcmsLeft |> PList.toArray |> Array.sort |> median
                        let medianR = listOfdpcmsRight |> PList.toArray |> Array.sort |> median
                        let medianFinal = (medianL + medianR) / 2.0
                        Math.Round(medianFinal,2)
                      
                    else 0.0

                let camVars = PList.ofList [cL; cR]
                let newVP = createNewViewPlan camVars p "WACLR" medValue rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with WACLR = st; viewplans = newViewPlanList}

                
        newRover


    let rotateToPoint (rover:RoverModel) =
        
        let selectedViewPlan = rover.selectedViewPlan

        match selectedViewPlan with
        | Some plan ->
            let camvar = plan.cameraVariables |> PList.first
            let length = camvar.viewList.Count 
            let idx = rover.walkThroughIdx
            let lastIdx = length - 1
            let newIdx = if idx = lastIdx then 0 else (idx+1)

            {rover with walkThroughIdx = newIdx}
            
        | None -> rover


     //create new cameraviews for stereo cam
    let updateStereoCam (forward:V3d) (pos:V3d) (r:RoverModel)=

        let rightV = (forward.Normalized).Cross(r.up)
        let shift = rightV * 0.2
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

  
    let calculateValues (runtimeInstance:IRuntime) (renderSg : ISg<_>) (rover:RoverModel)=
        
        let region = rover.reg
        let selectedPos = rover.selectedPosition
        let newRover = 
            match region,selectedPos with
            | Some reg, Some p -> 
                let spherePos = p.position
                let shiftedPoints = reg  |> PList.map (fun p -> (p - spherePos).Normalized)
                let rotatedPoints = shiftedPoints  |> PList.map (fun p -> rotateIntoCoordinateSystem rover p)
                let projectionPoints = shiftedPoints |> PList.map (fun p -> p + spherePos)
                let thetaPhiValues = rotatedPoints |> PList.map(fun p -> calcThetaPhi p) 

                //debugging
                for p in thetaPhiValues do
                    printfn "theta %A phi %A"  (p.X) (p.Y) 

                //point on forward vector
                let referencePoint = p.target
                let shifted = (referencePoint-spherePos).Normalized
                let r = rotateIntoCoordinateSystem rover shifted
                let thetaPhi = calcThetaPhi r
                printfn "thetaOnForward %A phiOnForward %A"  thetaPhi.X thetaPhi.Y
                let setR = initializePan rover thetaPhi.X
                let setR2 = initializeTilt setR thetaPhi.Y

                //set the camera according to the selected placement
                let roverWithupdatedCam = setCamera setR2

                let r = {roverWithupdatedCam with thetaPhiValues = thetaPhiValues; projPoints = projectionPoints}
                sampling p runtimeInstance renderSg r

            | _,_ -> rover
        
        newRover
   
        
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
        
        let selectedLoc = rover.positionsList |> Seq.tryFind (fun place -> place.id = id)

        match selectedLoc with
        | Some placement -> 
            {rover with position = placement.position; target = placement.target; selectedPosition = Some placement}
        | None -> rover

     
    let showViewPlan (id:int) (rover:RoverModel) = 
        
        let selectedVP = rover.viewplans |> Seq.tryFind (fun place -> place.id = id)
        match selectedVP with 
        | Some viewplan ->
            {rover with selectedViewPlan = Some viewplan; walkThroughIdx = 0}
        | None -> rover

    
    let toggleDpiSampling (rover:RoverModel) = 
        
        let sampling = rover.samplingWithDpi
        {rover with samplingWithDpi = not sampling}



    let sampleAllCombinations (runtimeInstance:IRuntime) (renderSg : ISg<_>) (rover:RoverModel) =
        
        let positions = rover.positionsList
        let cameras = rover.cameraOptions
        let panoverlaps = rover.panOverlapOptions
        let tiltoverlaps = rover.tiltOverlapOptions
        let mutable id = 1

        let views = 
         alist{
        
            for position in positions do
            
                for cam in cameras do
                    let camtype = fst cam
                    
                    for panOL in panoverlaps do  
                        let o = fst panOL
                        let panO = 
                            match o with
                            | Percent_20 -> 20.0
                            | Percent_30 -> 30.0
                            | Percent_40 -> 40.0
                            | Percent_50 -> 50.0
                        
                        for tiltOL in tiltoverlaps do
                            let o = fst tiltOL
                            let tiltO = 
                                match o with
                                | Percent_20 -> 20.0
                                | Percent_30 -> 30.0
                                | Percent_40 -> 40.0
                                | Percent_50 -> 50.0
                            
                            let sampleRover = {rover with position = position.position; target = position.target; selectedPosition = Some position; camera = camtype; panOverlap = panO; tiltOverlap = tiltO}
                            let result = calculateValues runtimeInstance renderSg sampleRover
                            let newViewPlan = result.viewplans |> PList.first
                            let vpWithId = {newViewPlan with id = id}
                            id <- id + 1
                            yield vpWithId

         }
        
        let list = views |> AList.toPList
        {rover with viewplans = list}

        

    let update (rover:RoverModel) (action:RoverAction) (runtimeInstance : IRuntime) (renderSg : ISg<_>) =
        
        match action with
  
            //| ChangePan p -> 
            //    let rover' = setPan rover p
            //    panning rover' rover.camera

            //| ChangeTilt t -> 
            //    let rover' = setTilt rover t
            //    tilting rover' rover.camera
            
            | SwitchCamera cam ->
                changeCam rover cam
            
            | CalculateAngles ->
                calculateValues runtimeInstance renderSg rover

            | SampleAllCombinations ->
                sampleAllCombinations runtimeInstance renderSg rover
            
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
            
            | ToggleDpiSampling ->
                toggleDpiSampling rover
                
                
                
                
              
    


