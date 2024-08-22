namespace  ElevationProfileViewer

open System
open Aardvark.Base
open OpcViewer.Base
open OpcViewer.Base.Picking
open Rabbyte.Drawing

open FSharp.Data.Adaptive

module SurfaceSampling =

    let caluculateSVGDrawingPositions (model : Model) = 
        let xOffset = model.offsetUIDrawX
        let yOffset = model.offsetUIDrawY
                                            
        let dimensions = model.cutViewDim
        let xWidth = (float) dimensions.X 
        let yWidth = (float) dimensions.Y
        
        let space = " "
        let comma = ","
        
        let sX = (sprintf "%f" (xOffset * xWidth)) 
        
        let wX = (sprintf "%f" ((1.0-xOffset) * xWidth)) 
        let wY = (sprintf "%f" ((1.0-yOffset) * yWidth))
        
        let altitudeList = model.altitudeList
        let errorHitList = model.errorHitList
        
        let maxAltitude = model.maxHeight
        let minAltitude = model.minHeight
        let range = maxAltitude - minAltitude
                                        
        let drawingAvailableCoord, drawingMissingCoord, drawingMissingSurfaceCoord = 
            let mutable currentCorrectPoints = ""
            let mutable currentErrorPoints = ""
            let mutable currentErrorSurfacePoints = ""
        
            for i = 0 to altitudeList.Length-1 do
                let currentX = (100.0/ (float) (altitudeList.Length-1)) * (float i)
                let currentY = (altitudeList.Item(i) - minAltitude) / range
        
                let normalizeX = (sprintf "%f" ((xOffset+ (currentX/100.0) * (1.0-xOffset*2.0)) * xWidth) )
                let normalizeY = (sprintf "%f" ((yOffset+ (1.0-currentY) * (1.0-yOffset*2.0)) * yWidth) )
                currentCorrectPoints <- currentCorrectPoints + normalizeX + comma + normalizeY + space
        
                if errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = 0 then
                    currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space
                    let initY = (sprintf "%f" ((1.0-yOffset) * yWidth))
                    let initialPointsCoord = normalizeX + comma + initY + space + normalizeX + comma + normalizeY + space
                    currentErrorSurfacePoints <- currentErrorSurfacePoints + initialPointsCoord
                elif errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = -1 && errorHitList.Item(i+1) = -1 then
                    currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space
                    currentErrorSurfacePoints <- currentErrorSurfacePoints + normalizeX + comma + normalizeY + space
                elif errorHitList.Item(i) = -1 && errorHitList.Item(i+1) = 0 then
                    currentErrorPoints <- currentErrorPoints + normalizeX + comma + normalizeY + space + space
                    let endY = (sprintf "%f" ((1.0-yOffset) * yWidth))
                    let endPointsCoord = normalizeX + comma + normalizeY + space + normalizeX + comma + endY + space
                    currentErrorSurfacePoints <- currentErrorSurfacePoints + endPointsCoord
        
            currentCorrectPoints, currentErrorPoints, currentErrorSurfacePoints                       
                                        
        let initialPointsCoord = wX + comma + wY + space + sX + comma + wY + space
        let surfaceUnderLineCoord =  initialPointsCoord + drawingAvailableCoord 
        
        let circleSize = 
            let x1, x2 =
                if altitudeList.Length > 2 then
                    ((100.0/ (float) (altitudeList.Length-1)) * 1.0), ((100.0/ (float) (altitudeList.Length-1)) * 2.0)
                else  
                    0.0, 0.0
            x2-x1
        
        
        { model with svgPointsCoord = drawingAvailableCoord; svgSurfaceUnderLineCoord = surfaceUnderLineCoord; svgPointsErrorCoord = drawingMissingCoord; svgSurfaceUnderLineErrorCoord = drawingMissingSurfaceCoord; svgCircleSize = circleSize }
    
    let getNumberOfErrors (errorHitList : int list) (index : int) = 
        let mutable keepCounting = true
        let mutable i = index
        let mutable numErrors = 0
        
        while keepCounting && i < errorHitList.Length do
            if errorHitList.Item(i) = -1 then
                 numErrors <- numErrors + 1
                 i <- i + 1
            else 
                 keepCounting <- false
        numErrors
        
    let createInterpolatedSubList (min : float) (max : float) (amount : int) =
        let mutable subList = []
        for i = 1 to amount do
            subList <- subList @ [min + ((max-min)/float (amount + 1)) * float i]
        subList
    
    let correctSamplingErrors (altitudeList : float list) (errorHitList : int list) =
        let correctedList = 
            let mutable finalList = altitudeList
            let mutable i = 0
            while i < finalList.Length do
                if errorHitList.Item(i) = -1 && errorHitList.Item(i-1) = 0 then
                    let newArray = finalList |> List.toArray                
                    let leftArray, rightArray = newArray |> Array.splitAt i
                    let leftList = leftArray |> Array.toList
                    let rightList = rightArray |> Array.toList
                    
                    let numMissingValues = getNumberOfErrors errorHitList i
                    let interpolatedSubList = createInterpolatedSubList (leftList.Item(leftList.Length-1)) rightList.Head numMissingValues
                    finalList <- (leftList @ interpolatedSubList @ rightList)
                i <- i + 1
            finalList   
                                      
        correctedList
    
    let sampleBetweenTwoPoints (model : Model) (firstPoint : V3d) (secondPoint : V3d) (pointList : V3d list) (altitudeList : float list) (errorHitList: int list) = 
        let initialListLength = pointList.Length
        
        let dir = secondPoint - firstPoint
        let samplingSize = (Math.Round (firstPoint-secondPoint).Length) * model.stepSampleSize.value
        let step = dir / samplingSize  
          
        let sampleAndFillLists ((pointList : V3d list), (altitudeList : float list), (errorHitList: int list)) i =
            let fray = FastRay3d(V3d.Zero, (firstPoint + (step * float i)).Normalized)         
            model.picking.pickingInfos 
            |> HashMap.tryFind model.opcBox
            |> Option.map (fun opcData -> 
                match OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some opcData.kdTree) fray with
                | Some t -> 
                    let hitpoint = fray.Ray.GetPointOnRay t
                    let pointHeight = CooTransformation.getLatLonAlt hitpoint Planet.Mars
        
                    (hitpoint :: pointList), (pointHeight.altitude :: altitudeList), (0 :: errorHitList)                  
                | None -> 
                    (pointList.Head :: pointList), altitudeList, (-1 :: errorHitList)                
                )
            |> Option.defaultValue (pointList, altitudeList, errorHitList)
                             
        let pL, aL, eL = List.fold sampleAndFillLists (pointList, altitudeList, errorHitList) [0.0..samplingSize]
        let numofElemBtw2Points = pL.Length - initialListLength
        
        pL, aL, eL, numofElemBtw2Points
       
    
    let sampleSurfacePointsForCutView (model : Model) (pickingModel : PickingModel) (drawingModel : DrawingModel) = 
        let numIntersectionPoints = pickingModel.intersectionPoints.Count
          
        let rec sampleBetweenMultiplePoints i pL aL eL numElemList=
            if (numIntersectionPoints-1) > i then 
                let pl, aL, eL, numElem = sampleBetweenTwoPoints model (pickingModel.intersectionPoints.Item(i)) (pickingModel.intersectionPoints.Item(i+1) ) pL aL eL 
                sampleBetweenMultiplePoints (i+1) pl aL eL (numElem :: numElemList)
            else 
              pL, aL, eL, numElemList
        
        
        let pointList, altitudeList, errorHitList, numofElemsBtw2PointsList = sampleBetweenMultiplePoints 0 [] [] [] []
        
        let numSampledPoints = altitudeList.Length
        
        let correctedAltitudeList = correctSamplingErrors altitudeList errorHitList  
        
        let rec fillSublineDrawing i drawing =
            if i > 0 then
               fillSublineDrawing (i-1) (DrawingApp.update drawing (DrawingAction.AddPoint (pointList.Item(i), None)))
            else
               DrawingApp.update drawing (DrawingAction.AddPoint (pointList.Item(i), None))
         
        let drawing2 = 
            if pointList.Length > 0 then
                fillSublineDrawing 
                    (pointList.Length-1) 
                    { DrawingModel.initial with 
                        style = 
                            { DrawingModel.initial.style with 
                                thickness = 1.0
                                primary = C4b(255,255,255)
                                secondary = C4b(205,243,255)
                            } 
                    } 
            else
                model.drawing2
        
        let rec linearDistance i acc = 
            if i >= 1 then
                let A = pickingModel.intersectionPoints.Item(i)
                let B = pickingModel.intersectionPoints.Item(i-1)
                let distance = (A-B).Length
                linearDistance (i-1) (acc+distance)
            else 
                acc
        
        let linDist = linearDistance (numIntersectionPoints-1) 0.0
        
        let rec accDistance i acc = 
            if i >= 1 then
                let A = pointList.Item(i)
                let B = pointList.Item(i-1)
                let distance = (A-B).Length
                accDistance (i-1) (acc+distance)
            else 
                acc
        
        let camLoc = model.cameraState.view.Location
        
        let rec createTriangleStrips i arr = 
            if i >= 1 then
                let a = pickingModel.intersectionPoints.Item(i)
                let b = pickingModel.intersectionPoints.Item(i-1)
        
                let aDir = b-a
                let bDir = a-b
        
                let aCorrected = a + bDir / 5.0 
                let bCorrected = b + aDir / 5.0 
        
                let aV = aCorrected - camLoc
                let bV = bCorrected - camLoc
        
                let aDublicated1 = V4d(aCorrected,0.0)
                let aDublicated2 = V4d(aCorrected,1.0)
        
                let bDublicated1 = V4d(bCorrected,0.0)
                let bDublicated2 = V4d(bCorrected,1.0)
        
                let stripWidth = (aV.Length + bV.Length)  / 10.0
        
                let aCross, bCross = 
                    if numIntersectionPoints > 2 then
                        if i > 1 then
                            (Vec.Cross(bDir,b)).Normalized, (Vec.Cross(bDir,b)).Normalized
                        else 
                            (Vec.Cross(aDir,a)).Normalized, (Vec.Cross(aDir,a)).Normalized
                    else
                        (Vec.Cross(bDir,bV)).Normalized, (Vec.Cross(aV,aDir)).Normalized
                  
        
                let a0Moved = aDublicated1.XYZ + aCross * (aDublicated1.W - 0.5) * stripWidth 
                let a1Moved = aDublicated2.XYZ + aCross * (aDublicated2.W - 0.5) * stripWidth 
        
                let b0Moved = bDublicated1.XYZ + bCross * (bDublicated1.W - 0.5) * stripWidth 
                let b1Moved = bDublicated2.XYZ + bCross * (bDublicated2.W - 0.5) * stripWidth 
               
                let triangle1 = Triangle3d(a0Moved,a1Moved,b0Moved)
                let triangle2 = Triangle3d(a1Moved,b0Moved,b1Moved)
        
                createTriangleStrips (i-1) (Array.append [|triangle1;triangle2|] arr)
            else 
                arr 
               
        let triangleStrips = createTriangleStrips (numIntersectionPoints-1) [||]
              
        { model with 
            picking                    = pickingModel 
            drawing                    = drawingModel 
            drawing2                   = drawing2 
            numSampledPoints           = numSampledPoints 
            samplingDistance           = linDist / float (pointList.Length - 1)
            linearDistance             = Math.Round(linDist,2)
            minHeight                  = altitudeList.Min (altitudeList.Item(0))
            maxHeight                  = altitudeList.Max (altitudeList.Item(0))
            accDistance                = Math.Round(accDistance (pointList.Length-1) 0.0,2)
            pointList                  = pointList
            altitudeList               = correctedAltitudeList  
            errorHitList               = errorHitList
            numofPointsinLineList      = numofElemsBtw2PointsList
            hoverTriangles             = triangleStrips     
        }