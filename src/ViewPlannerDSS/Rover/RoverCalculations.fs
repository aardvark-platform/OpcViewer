namespace ViewPlanner.Rover

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

open Aardvark.SceneGraph
open FShade

open Aardvark.Application

open Aardvark.UI


type DpcmVars = 
    {
        depth          : IBackendTexture
        description    : OutputDescription
        pixelSizeNear  : float
        pixelSizeFar   : float
        matrix         : Matrix<float32>
        renderTask     : IRenderTask
    }

module RoverCalculations =

    //returns theta, phi values in degrees
    let calcThetaPhi (position:V3d) =

        let theta = (atan2 position.X position.Y)* Constant.DegreesPerRadian 
        let phi = (acos(position.Z)) * Constant.DegreesPerRadian

        //// Maybe easier... 
        //bring theta to interval [0, 360]... [-180, 180] ...range old + 180 => [0, 360] % 360 -> [0, 360[ 0 => 0; 360 => 0; 370 => 10; 45 => 45
        //V2d((theta+180.0)% 360.0, phi)
        
        //both sphere halfs are independent
        //let thetaShifted = 
        //    if theta < 0.0 then
        //        theta * (-1.0)
        //    else 
        //        let delta = 180.0 - theta
        //        180.0 + delta
        
        let thetaShifted = (theta + 180.0) % 360.0

 
        

        V2d(thetaShifted,phi)


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
    
    
    let calculatePanValues (pans:list<float>) =
        
        let result = 
            pans 
            |> List.groupBy (fun p -> if p = 360.0 then 0 else (int (1.0 + (p/90.0))) % 4)
            |> HMap.ofList
            |> HMap.map (fun _ values -> values |> List.sort)

        let minPan, maxPan, cross0_360 = 
            let q1Values = result |> HMap.tryFind 1
            let q2Values = result |> HMap.tryFind 2
            let q3Values = result |> HMap.tryFind 3
            let q4Values = result |> HMap.tryFind 0 //4 % 4 = 0

            match q1Values, q2Values, q3Values, q4Values with
            | Some q1, None, None, Some q4 -> (q1 |> List.last), (q4 |> List.head), true
            | Some q1, None, Some q3, Some _ -> (q1 |> List.last), (q3 |> List.head), true
            | Some _, Some q2, None, Some q4 -> (q2 |> List.last), (q4 |> List.head), true
            | _ -> (pans |> List.min), (pans |> List.max), false 

        (minPan,maxPan,cross0_360)
    


    let median (arr:float[]) =
         
        let myAvg a b = (a+b)/2.0

        let length = arr.Length
        match length with
        | 0 -> failwith "invalid length"
        | 1 -> arr.[0]
        | 2 -> myAvg arr.[0] arr.[1]
        | _ -> 
            let idx = int (Math.Floor((float length)/2.0))
            match length % 2 with
            | 0 -> myAvg arr.[idx] arr.[idx + 1]
            | _ -> arr.[idx]
    

    
    let linearization (value:float32) (zNear:float32) (zFar:float32) = 
        let zn = 2.0f * value - 1.0f
        2.0f * zNear * zFar / (zFar + zNear - zn * (zFar - zNear))

    
    let pixelSizeCm (plane:float) (vertRes:float) (fovV:float) = 
        
        let rad = fovV.RadiansFromDegrees()
        let sizeWorld = (tan(rad/2.0) * plane) * 2.0
        (sizeWorld / vertRes) *100.0      

    
    let interpolatePixelSize (min:float) (max:float) (value:float) (sizeNear:float) (sizeFar:float) = 
        
        let normalized = (value - min) / (max-min) // [0,1]
        sizeNear * (1.0 - normalized) + sizeFar * normalized



    let initDpcm (runtimeInstance: IRuntime) (frustum:Frustum) (renderSg :ISg<_>) (vT:IMod<Trafo3d>) (res:V2d) =
        let size = V2i(res.X, res.Y)

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
        
        let description = fbo |> OutputDescription.ofFramebuffer
        let projTrafo  = Frustum.projTrafo(frustum);

        let render2TextureSg =
            renderSg
            |> Sg.viewTrafo vT
            |> Sg.projTrafo (Mod.constant projTrafo)
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo 
                toEffect DefaultSurfaces.diffuseTexture
            ]

        let vR = float res.Y
        let pixelSizeNear = pixelSizeCm frustum.near vR fovV
        let pixelSizeFar = pixelSizeCm frustum.far vR fovV

        let mat = Matrix<float32>(int64 size.X, int64 size.Y)

        let task : IRenderTask =  runtimeInstance.CompileRender(signature, render2TextureSg)
        let taskclear : IRenderTask = runtimeInstance.CompileClear(signature,Mod.constant C4f.Black,Mod.constant 1.0)
        let realTask = RenderTask.ofList [taskclear; task]

        let vars = {
            depth = depth
            description = description
            pixelSizeNear = pixelSizeNear
            pixelSizeFar = pixelSizeFar
            matrix = mat
            renderTask = realTask
        }
        vars



    let calculateDpcm (runtimeInstance: IRuntime) (frustum:Frustum) (view:CameraView) 
        (viewTrafo : IModRef<Trafo3d>) (vars : DpcmVars) =

        transact (fun _ -> viewTrafo.Value <- view.ViewTrafo)
        vars.renderTask.Run (null, vars.description)
        
        let mat = vars.matrix
        runtimeInstance.DownloadDepth(vars.depth,0,0,mat)

        // dots-per-cm
        let dpcm = 
            mat.Data 
            |> Array.map(fun v -> linearization v (float32 frustum.near) (float32 frustum.far))
            |> Array.map(fun v -> interpolatePixelSize frustum.near frustum.far (float v) vars.pixelSizeNear vars.pixelSizeFar)
            |> Array.filter(fun f -> f < frustum.far) // distance cut off (clip pixels on far plane)
            |> Array.sort
            |> fun x -> 1.0 / (median x)

        dpcm


    let calculateViewMatrix (pan : float) (tilt : float) (cam:CamVariables) (rover : RoverModel)  =

        let panDelta = 
            //let d = pan - rover.pan.current
            let d = rover.pan.current - pan
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




