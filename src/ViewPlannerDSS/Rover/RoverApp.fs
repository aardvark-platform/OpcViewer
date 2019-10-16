namespace ViewPlanner.Rover

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open FShade

open Aardvark.Application

open Aardvark.UI
open Aardvark.UI.Primitives



module RoverApp =


    let initializeTilt (value:float) (m:RoverModel) = 
        {m with tilt = {m.tilt with previous = value; current = value}}
    
    let initializePan (value:float) (m:RoverModel) = 
        {m with pan = {m.pan with previous = value; current = value}}

  
    let rotateIntoCoordinateSystem (m:RoverModel) (vector:V3d) = 
        
        let rotZ = Trafo3d.RotateInto(m.up,V3d.OOI)
        let rotatedbyZ = rotZ.Forward.TransformDir vector
        rotatedbyZ
    
  
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
                
   
    
    let createNewViewPlan (camvars:plist<CamVariables>) (p:Placement) (camtype: string) (spatialRes:float) (rover:RoverModel) =

        let output = 
            let cam = camvars |> PList.first
            let values = cam.samplingValues |> PList.toList
            let numSamples = values.Length
                
            let energy = 
                let res = RoverCalculations.calculateOutputVars values (numSamples-1) 0.0 rover.energyForPanTilt rover
                Math.Round(res, 2)

            let time = 
                let res = RoverCalculations.calculateOutputVars values (numSamples-1) 0.0 rover.timeForPanTilt rover
                Math.Round(res,2)
                
            let countSamples = 
                let count = numSamples
                if camtype = "WACLR" then count * 2 else count

            let datasize = RoverCalculations.calculateDataSize countSamples rover

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

        let minPan, maxPan, cross360 = RoverCalculations.calculatePanValues panList

        let deltaPan = 
            match cross360 with
            | true -> 
                let deltaToZero = minPan
                let deltaToMaxFromZero = 360.0 - maxPan
                deltaToZero + deltaToMaxFromZero
            | false -> 
                Math.Abs(maxPan - minPan)

        //sort tilt values
        let sortedTilts = List.sort tiltList
        let minTilt = sortedTilts.Head
        let maxTilt = sortedTilts.Item(sortedTilts.Length - 1)
        let deltaTilt = maxTilt - minTilt
    
        //generate a sampling list with pan and tilt values
        let firstPair = V2d(minPan, maxTilt) //pair with min pan value and max tilt value (equals left bottom corner of bounding box)
        let samplingValues = [firstPair] //initial list
        
        let sampleWithDpi = rover.samplingWithDpi
        let res = V2d(rover.horzRes, rover.vertRes)

        let newRover = 
            match currentCamera with
            | HighResCam -> 
                let cam = rover.HighResCam.cam
                let values = createSamplingList cam samplingValues deltaPan deltaTilt cross360 rover |> PList.ofList
                let viewMatrices = values |> PList.map(fun m -> RoverCalculations.calculateViewMatrix  m.X m.Y cam rover)

                let vT = viewMatrices |> PList.first |> CameraView.viewTrafo |> Mod.init
                let dpcmVariables = RoverCalculations.initDpcm runtimeInstance cam.frustum renderSg vT res
  
                let medValue = 
                    if sampleWithDpi then
                        let listOfdpcms = viewMatrices |> PList.map(fun e -> RoverCalculations.calculateDpcm runtimeInstance cam.frustum e vT dpcmVariables)
                        let median = listOfdpcms |> PList.toArray |> Array.sort |> RoverCalculations.median 
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

                let viewMatricesLeft = values |> PList.map(fun m -> RoverCalculations.calculateViewMatrix  m.X m.Y cam.camL rover) 
                let viewMatricesRight = values |> PList.map(fun m -> RoverCalculations.calculateViewMatrix  m.X m.Y cam.camR rover)

                let cL = {rover.WACLR.camL with samplingValues = values; viewList = viewMatricesLeft }
                let cR = {rover.WACLR.camR with samplingValues = values; viewList = viewMatricesRight }
                let st = {rover.WACLR with camL = cL; camR = cR}

                let vT = viewMatricesLeft |> PList.first |> CameraView.viewTrafo |> Mod.init
                
                let dpcmVariables = RoverCalculations.initDpcm runtimeInstance cam.camL.frustum renderSg vT res

                let medValue = 
                    if sampleWithDpi then
                        let listOfdpcmsLeft = viewMatricesLeft |> PList.map(fun e -> RoverCalculations.calculateDpcm runtimeInstance cL.frustum e vT dpcmVariables)
                        let listOfdpcmsRight = viewMatricesRight |> PList.map(fun e -> RoverCalculations.calculateDpcm runtimeInstance cR.frustum e vT dpcmVariables)
                        let medianL = listOfdpcmsLeft |> PList.toArray |> Array.sort |> RoverCalculations.median
                        let medianR = listOfdpcmsRight |> PList.toArray |> Array.sort |> RoverCalculations.median
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
        
        let pos = rover.position
        let forward = rover.target - pos

        match rover.camera with
        | HighResCam -> 
            let cam = CameraView.look pos forward.Normalized rover.up
            let view = {rover.HighResCam.cam.camera with view = cam}
            let vars = {rover.HighResCam.cam with position = pos; camera = view}
            let hr = {rover.HighResCam with cam = vars}
            {rover with HighResCam = hr}

        | WACLR -> 
            let camL, camR = updateStereoCam forward pos rover
            let stc = {rover.WACLR with camL = camL; camR = camR}
            {rover with  WACLR = stc}

  
    let calculateValues (runtimeInstance:IRuntime) (renderSg : ISg<_>) (rover:RoverModel)=
        
        match rover.selectedRegion, rover.selectedPosition with
        | Some reg, Some p -> 
            let spherePos = p.position
            let shiftedPoints = reg  |> PList.map (fun p -> (p - spherePos).Normalized)
            let rotatedPoints = shiftedPoints  |> PList.map (fun p -> rotateIntoCoordinateSystem rover p)
            let projectionPoints = shiftedPoints |> PList.map (fun p -> p + spherePos)
            let thetaPhiValues = rotatedPoints |> PList.map(fun p -> RoverCalculations.calcThetaPhi p) 

            //debugging
            for p in thetaPhiValues do
                printfn "theta %A phi %A"  (p.X) (p.Y) 

            //point on forward vector
            let referencePoint = p.target
            let shifted = (referencePoint-spherePos).Normalized
            let r = rotateIntoCoordinateSystem rover shifted
            let thetaPhi = RoverCalculations.calcThetaPhi r
            printfn "thetaOnForward %A phiOnForward %A"  thetaPhi.X thetaPhi.Y
            let setR = initializePan thetaPhi.X rover
            let setR2 = initializeTilt thetaPhi.Y setR

            //set the camera according to the selected placement
            let roverWithupdatedCam = setCamera setR2

            let r = {roverWithupdatedCam with thetaPhiValues = thetaPhiValues; projPoints = projectionPoints}

            sampling p runtimeInstance renderSg r

        | _ -> rover
        
   
        
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
        {rover with samplingWithDpi = not rover.samplingWithDpi}



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
                
                
                
                
              
    


