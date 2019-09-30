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

        //bring theta to interval [0, 360]
        let thetaShifted = 
            if theta < 0.0 then
                theta * (-1.0)
            else 
                let delta = 180.0 - theta
                180.0 + delta

        V2d(thetaShifted,phi)
        //V2d(theta,phi)


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
    let rec calculateOutputVars (values:List<V2d>) (counter:int) (sum:float) (cost:float) (rover : RoverModel)  = 

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
            calculateOutputVars values (counter-1) (sum+e) cost rover


    let calculateDataSize (numberOfSamples:int) (rover:RoverModel) =
        
        let colordepth = rover.colorDepth
        let channels = rover.numberOfColorChannels
        let horzRes = rover.horzRes
        let vertRes = rover.vertRes
        let pixel = horzRes * vertRes

        let bitPerPicture = (int)pixel * colordepth * channels
        let totalByte = (bitPerPicture * numberOfSamples) / 8        
        let totalKbyte = totalByte / 1024
        let totalMByte = totalKbyte / 1024

        totalMByte




        

    

    //takes pan and tilt values and calculates a view matrix for frustum visualisation

    let calculateViewMatrix  (rover : RoverModel) (pan : float) (tilt : float) (cam:CamVariables) =
        //runtime.create





        let panCurr = rover.pan.current

        let panDelta = 
            let d = pan - panCurr
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

        let view = CameraView.look newPos targetWithTilt.Normalized up

        view

    
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


    let panValues (pans:list<float>) =
        
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
        let v = two * zNear * zFar / (zFar + zNear - zn * (zFar - zNear))
        v
    
    
    let pixelSizeCm (plane:float) (horzRes:float) (vertRes:float) (fovH:float) (fovV:float) = 
        
        let rad = fovV.RadiansFromDegrees()
        let sizeWorld = (tan(rad/2.0) * plane)*2.0
        let height = (sizeWorld / vertRes) *100.0      

        //test
        //let fH = Math.Round((fovH), 0)
        //let rad2 = fH.RadiansFromDegrees()
        //let sizeWorld2 = (tan(rad2/2.0) * plane)*2.0
        //let width = (sizeWorld2 / horzRes) *100.0 
        //let test = width

        height  
    

    let interpolatePixelSize (min:float) (max:float) (value:float) (sizeNear:float) (sizeFar:float) = 
        let normalized = (value - min) / (max-min) // [0,1]
        //let interpolatedSize = Fun.Lerp(normalized, sizeNear, sizeFar)
        let interpolatedSize = sizeNear * (1.0 - normalized) + sizeFar * normalized
        interpolatedSize
   

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

            let arr = mat.Data
            
            let near = float32 frustum.near
            let far = float32 frustum.far
            
            let zView = arr |> Array.map(fun v -> linearization v near far)

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


        







    let sampling (p : Placement) (runtimeInstance: IRuntime) (renderSg : ISg<_>) (rover : RoverModel) = 
        
        let panOverlap = rover.panOverlap
        let tiltOverlap = rover.tiltOverlap
        let currentCamera = rover.camera

        let values = rover.thetaPhiValues
        let li = values |> PList.toList
        let pans = li |> List.map (fun l -> l.X) //list with just pan values
        
        let tilts = li |> List.map (fun l -> l.Y) //list with just tilt values


        let panInfo = panValues pans
        let minPan = 
            match panInfo with
            | min, max, bool -> min

        let maxPan = 
            match panInfo with
            | min, max, bool -> max
        
        let cross360 = 
            match panInfo with
            | min, max, bool -> bool

     
        let deltaPan = 
            
            match cross360 with
            | true -> 
                let deltaToZero = minPan
                let deltaToMaxFromZero = 360.0 - maxPan
                deltaToZero + deltaToMaxFromZero
                
            | false -> Math.Abs (maxPan - minPan)

           
        //sort tilt values
        let sortedTilts = List.sort tilts
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
                let camera = rover.HighResCam
                let fovH = camera.cam.frustum |> Frustum.horizontalFieldOfViewInDegrees
                let ratio = camera.cam.frustum |> Frustum.aspect
                let fovV = Math.Round((fovH / ratio), 0)

                let panRef = fovH * (1.0 - (panOverlap/100.0))            
                let tiltRef = fovV * (1.0 - (tiltOverlap/100.0))
                let panningRate = int(Math.Round((Math.Abs(deltaPan)) / panRef))
                let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / tiltRef))

                let refPan = if cross360 then (panRef * -1.0) else panRef

                let values = buildList samplingValues panningRate tiltingRate tiltingRate refPan -tiltRef cross360 
                let sv = values |> PList.ofList

                //for visualising footprints
                let viewMatrices = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.cam) |> PList.ofList

                let medValue = 
                    if sampleWithDpi then

                        let l = viewMatrices |> PList.toList
                        let listOfdpcms = l |> List.map(fun e -> calculateDpcm runtimeInstance renderSg camera.cam.frustum e rover)
                        let median = listOfdpcms |> Array.ofList |> Array.sort |> median 
                        Math.Round(median,2)
                    
                    else 0.0

                let HR = {rover.HighResCam with cam = { rover.HighResCam.cam with samplingValues = sv; viewList = viewMatrices }}

                let camVars = PList.ofList [HR.cam]
                let newVP = createNewViewPlan camVars p "High Resolution Camera" medValue rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with HighResCam = HR; viewplans = newViewPlanList} //numberOfSamples = numberOfSamples; energyRequired = energy; timeRequired = time}

            | WACLR -> 
                let camera = rover.WACLR
                let fovH = camera.camL.frustum |> Frustum.horizontalFieldOfViewInDegrees 
                let ratio = camera.camL.frustum |> Frustum.aspect
                let fovV = Math.Round((fovH / ratio), 0)


                //let ol = camera.overlapBetweenCams / 100.0
                //let rest = fovH - (fovH * ol)
                //let jointFov = fovH + rest
                //let panRef = (jointFov * (1.0 - (panOverlap/100.0))) 
                let panRef = (fovH * (1.0 - (panOverlap/100.0))) 
                let tiltRef = fovV * (1.0 - (tiltOverlap/100.0))
                let panningRate = int(Math.Round(deltaPan / panRef))
                let tiltingRate = int(Math.Round((Math.Abs(deltaTilt)) / (tiltRef)))

                let refPan = if cross360 then (panRef * -1.0) else panRef

                let values = buildList samplingValues panningRate tiltingRate tiltingRate refPan -tiltRef cross360
                let sv = values |> PList.ofList

                let viewMatricesLeft = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.camL) |> PList.ofList
                let viewMatricesRight = values |> List.map(fun m -> calculateViewMatrix rover m.X m.Y camera.camR) |> PList.ofList

                //leftcam
                let cL = {rover.WACLR.camL with samplingValues = sv; viewList = viewMatricesLeft }
                //rightcam
                let cR = {rover.WACLR.camR with samplingValues = sv; viewList = viewMatricesRight }

                let st = {rover.WACLR with camL = cL; camR = cR}

                let medValue = 
                    if sampleWithDpi then
                        let leftV = viewMatricesLeft |> PList.toList
                        let rightV = viewMatricesRight |> PList.toList
                        let listOfdpcmsLeft = leftV |> List.map(fun e -> calculateDpcm runtimeInstance renderSg cL.frustum  e rover)
                        let listOfdpcmsRight = rightV |> List.map(fun e -> calculateDpcm runtimeInstance renderSg cR.frustum  e rover)

                        let medianL = listOfdpcmsLeft |> Array.ofList |> Array.sort |> median
                        let medianR = listOfdpcmsRight |> Array.ofList |> Array.sort |> median
                        let medianFinal = (medianL + medianR) / 2.0
                        Math.Round(medianFinal,2)
                      
                    else 0.0
                

                let camVars = PList.ofList [cL; cR]

                let newVP = createNewViewPlan camVars p "WACLR" medValue rover
                let newViewPlanList = rover.viewplans.Append newVP

                {rover with WACLR = st; viewplans = newViewPlanList}

                
        newRover



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
                let referencePoint = p.target
                let shifted = (referencePoint-spherePos).Normalized
                let r = rotateIntoCoordinateSystem rover shifted
                let thetaPhi = calcThetaPhi r
                printfn "thetaOnForward %A phiOnForward %A"  thetaPhi.X thetaPhi.Y
                let setR = initializePan rover thetaPhi.X
                let setR2 = initializeTilt setR thetaPhi.Y


                //set the camera according to the selected placement
                let roverWithupdatedCam = setCamera setR2

                let r = {roverWithupdatedCam with thetaPhiValues = plist; projPoints = projectionPoints}
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
        
        let positions = rover.positionsList
        let selectedLoc = positions |> Seq.tryFind (fun place -> place.id = id)

        match selectedLoc with
            | Some placement -> 
                let pos = placement.position
                let tar = placement.target
                let projSphere = {rover.projsphere with position = pos}
                {rover with position = pos; target = tar; projsphere = projSphere; selectedPosition = Some placement}
            
            | None -> rover


     
    let showViewPlan (id:int) (rover:RoverModel) = 
        
        let viewplans = rover.viewplans
        let selectedVP = viewplans |> Seq.tryFind (fun place -> place.id = id)
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
                
                
                
                
              
    


