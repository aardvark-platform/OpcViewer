namespace OpcViewer.Base

open Aardvark.Base.Incremental

open System
open System.Runtime.InteropServices
open Aardvark.Base
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open IPWrappers


module CrackDetection = 
    open System.IO
    open IPWrappers
    
    let init = 0.0

    let initModel = 
        {
            inputPoints  = PList.empty
            outputPoints = PList.empty
        }

    let initCrackDetection () = 
        let logDir = @".\crackDetection" 
        let configDir = @".\crackDetection"                 
        Log.line "[CrackDetection] crackDetection directory %A" (System.IO.Path.GetFullPath(configDir))
        if (Directory.Exists logDir) && (Directory.Exists configDir) then
            let errorCode = CrackDetectionWrappers.Init(configDir, logDir)
            if errorCode > 0 then
                Log.line "[CrackDetection] crackDetection directory %d" errorCode
        else
            Log.error "[CrackDetection] init paths %A or %A not found" logDir configDir

    let deInitCrackDetection () = 
        CrackDetectionWrappers.DeInit()

    let private getInvalidIndices2d (positions : V2d[]) =
        positions 
          |> List.ofArray 
          |> List.mapi (fun i x -> if x.AnyNaN then Some i else None) 
          |> List.choose id

    let loadUVTrianglesFromFileWithIndices (path : string) =
        let coordinates = path |> fromFile<V2f>
    
        let data = coordinates.Data |> Array.map (fun x ->  x.ToV2d() ) //|> kdTree.affine.Forward.TransformPos)
 
        let invalidIndices = getInvalidIndices2d data
        let index = IndexHelper.computeIndexArray (coordinates.Size.XY.ToV2i()) invalidIndices
        
        let uvIndices = 
          index    
            |> Seq.chunkBySize 3

        let uvCoords =         
          uvIndices
            |> Seq.choose(fun x -> 
              if x.Length = 3 then Some [|data.[x.[0]]; data.[x.[1]]; data.[x.[2]]|]
              else None)
            |> Seq.map (fun x -> Triangle2d(x))
            |> Seq.toArray      
    
        (uvCoords, (Seq.toArray uvIndices))

    let getV3d (uvt:Triangle2d) (xyzT:Triangle3d) (p : V2d) =
        let t00 = uvt.P0.X - uvt.P2.X
        let t01 = uvt.P1.X - uvt.P2.X
        let t10 = uvt.P0.Y - uvt.P2.Y
        let t11 = uvt.P1.Y - uvt.P2.Y

        let denom = t00 * t11 - t01 * t10

        let iT00 =  t11 / denom
        let iT01 = -t01 / denom
        let iT10 = -t10 / denom
        let iT11 =  t00 / denom

        let lamb0 = iT00 * (p.X - uvt.P2.X) + iT01 * (p.Y - uvt.P2.Y)
        let lamb1 = iT10 * (p.X - uvt.P2.X) + iT11 * (p.Y - uvt.P2.Y)
        let lamb2 = 1.0 - lamb0 - lamb1

        //let x = xyzT.P0.X * lamb0 + xyzT.P1.X * lamb1 + xyzT.P2.X * lamb2
        //let y = xyzT.P0.Y * lamb0 + xyzT.P1.Y * lamb1 + xyzT.P2.Y * lamb2
        //let z = xyzT.P0.Z * lamb0 + xyzT.P1.Z * lamb1 + xyzT.P2.Z * lamb2

        let newV3d = xyzT.P0 * lamb0 + xyzT.P1 * lamb1 + xyzT.P2 * lamb2
        newV3d


    let insideTriangle (triangle:Triangle2d) (p : V2d) = 
        //  if p lies inside the triangle, then a1 + a2 + a3 must be equal to a.
        let a  = triangle.Area //Triangle2d(triangle.P0.XY, triangle.P1.XY, triangle.P2.XY).Area
        let a1 = Triangle2d(p, triangle.P1, triangle.P2).Area
        let a2 = Triangle2d(triangle.P0, p, triangle.P2).Area
        let a3 = Triangle2d(triangle.P0, triangle.P1, p).Area

        a = (a1 + a2 + a3)

    let calc3dPointFromUV (posPath : string) (coordsPath : string) (position : V2d) (trafo : Trafo3d) =
    //let triangles, triangleIndices = kdTree |> loadTrianglesWithIndices
        let coords, coordsIndices = loadUVTrianglesFromFileWithIndices coordsPath
        let positions = posPath |> fromFile<V3f>
        let data = 
          positions.Data |> Array.map (fun x ->  x.ToV3d() |> trafo.Forward.TransformPos)
    
        let mutable index = -1
        let triangle2d =
            coords
             |> List.ofArray
             |> List.choose( fun t2d ->
                                index <- index + 1
                                if (insideTriangle t2d position) then
                                    Some (t2d,index)
                                else None
                                 )
            |> List.tryHead

        let test =
            match triangle2d with
              | Some t ->
                let t2d, i = t
                let cI = coordsIndices.[i]
                let t3d = Triangle3d(data.[cI.[0]], data.[cI.[1]], data.[cI.[2]])
                let point = getV3d t2d t3d position
                point
              | None -> V3d.NaN

        test //|> kdTree.affine.Forward.TransformPos


    let getWidthAndHeight (ipoints : CrackDetectionWrappers.SPoint2D[]) = // ((ipoints : CrackDetectionWrappers.SPoint2D[]) =
        let widthMin =
            ipoints
                //|> Array.toList
                |> Array.sortBy( fun p -> p.m_dY)
                |> Array.tryHead

        let widthMax =
            ipoints
                //|> Array.toList
                |> Array.sortBy( fun p -> p.m_dY)
                |> Array.rev
                |> Array.tryHead

        let width = 
            match widthMax, widthMin with
             | Some wma, Some wmi -> (abs(wma.m_dY - wmi.m_dY)) |> ceil |> int
             |_,_ -> 0

        let heightMin =
            ipoints
                //|> Array.toList
                |> Array.sortBy( fun p -> p.m_dX)
                |> Array.tryHead

        let heightMax =
            ipoints
                //|> Array.toList
                |> Array.sortBy( fun p -> p.m_dX)
                |> Array.rev
                |> Array.tryHead

        let height = 
            match heightMax, heightMin with
             | Some hma, Some hmi -> (abs(hma.m_dX - hmi.m_dX)) |> ceil |> int 
             |_,_ -> 0

        width, height

    let getCrackUV (ipoints : plist<InputPoint>) (path:string) (texpath:string) = 
        
        let values = path |> fromFile<float>

        let xdim = values.Dim.X
        let ydim = values.Dim.Y
        //let zdim = values.Dim.Z

        let crackCoeffs = 
            ipoints
             |> PList.toArray
             |> Array.map( fun x -> float32 x.coeff )

        let controlPoints =
            ipoints
             |> PList.toArray
             |> Array.map( fun x -> CrackDetectionWrappers.SPoint2D( (x.uv.X * (float)xdim) |> ceil , (x.uv.Y * (float)ydim) |> ceil) )

        // calc nWidth + nHeight
        //let width, height = getWidthAndHeight controlPoints

        let yMin =
            controlPoints
                |> Array.toList
                |> List.sortBy( fun p -> p.m_dY)
                |> List.map( fun p -> p.m_dY )
                |> List.tryHead

        let yMax =
            controlPoints
                |> Array.toList
                |> List.sortBy( fun p -> p.m_dY)
                |> List.rev
                |> List.map( fun p -> p.m_dY )
                |> List.tryHead

        let ydimimg = 
            match yMax, yMin with
             | Some wma, Some wmi -> (abs(wma - wmi) + 4.0) |> ceil |> int
             |_,_ -> 0

        let xMin =
            controlPoints
                |> Array.toList
                |> List.sortBy( fun p -> p.m_dX)
                |> List.map( fun p -> p.m_dX )
                |> List.tryHead

        let xMax =
            controlPoints
                |> Array.toList
                |> List.sortBy( fun p -> p.m_dX)
                |> List.rev
                |> List.map( fun p -> p.m_dX )
                |> List.tryHead

        let xdimimg = 
            match xMin, xMax with
             | Some hma, Some hmi -> (abs(hma - hmi) + 4.0) |> ceil |> int 
             |_,_ -> 0

        let test = values.Data |> Array.map( fun p -> (float32)p)

        //let image = PixImage.Create(texpath).ToPixImage<byte>(Col.Format.RGB)

        let res = V2i(xdimimg, ydimimg)
        let pi = PixImage<byte>(Col.Format.RGBA, res)

        let coeffs =
            match yMin, xMin, yMax, xMax with
             | Some ymin, Some xmin, Some ymax, Some xmax ->
                    
                    let c = 
                        [|
                            for y in 0..(ydimimg-1) do
                                //yield [|
                                    for x in 0..(xdimimg-1) do
                                        let indx = ((int)xmin + x-2) + ((int)ymin + y-2) * (int)xdim
                                        let value = values.Data.[indx]

                                        pi.GetMatrix<C4b>().SetValue(C4b(value/ 255.0, value/ 255.0, value/ 255.0, 1.0), (int64)x, (int64)y) |> ignore
                                        yield (float32)value
                                    //|]
                        |]
                    c
             | _,_,_,_ -> [||]
        
        
        
        //let image = PixImage.Create(coeffs, Col.Format.RGB, (int64)xdimimg, (int64)xdimimg )//.ToPixImage<byte>(Col.Format.RGB)

        pi.SaveAsImage (@".\edge.png")

        let mutable numCrackpoints = 0
        let err = CrackDetectionWrappers.FindCrack( coeffs, (int)xdimimg, (int)ydimimg, controlPoints, ipoints.Count, &numCrackpoints)
        let pointsArray = CrackDetectionWrappers.SPoint2D.CreateEmptyArray(uint32 numCrackpoints)
        let err1 = CrackDetectionWrappers.GetCrack(pointsArray)

        
        

        let points = 
            CrackDetectionWrappers.UnMarshalArray<CrackDetectionWrappers.SPoint2D>(
                pointsArray, 
                (int)numCrackpoints
            ).ToV2ds()
            |> List.ofSeq 
            |> List.map( fun p -> V2d(p.X/(float)xdim, p.Y/(float)ydim))
            |> PList.ofList
        
        Log.line "[CrackDetection]"
        points
        //PList.empty

    let update (model : CrackDetectionModel) (msg : CrackDetectionAction) =   
        match msg with
            | AddCrackPoint (p,uv,cCoeff) -> 
                let ip = { position = p; uv = uv; coeff = cCoeff }
                {model with inputPoints = model.inputPoints |> PList.prepend ip } 
            | FinishCrack (path, texpath) -> 
                let uvPositions = getCrackUV model.inputPoints path texpath // model.inputUV
                //let crack = { uvPoints2d = uvPositions; points3d = plist.Empty }
                //TODO: uv texcoords to v3d points
                { model with outputPoints = uvPositions}

            
    
    let drawPoints (points: alist<V3d>) (near: IMod<float>) (far: IMod<float>) (pointSize: IMod<float>) (depthOffset : IMod<float>) =  

        let color = Mod.constant C4b.DarkMagenta
        let pointsSg = 
            points 
            |> SgUtilities.drawPointList color pointSize depthOffset near far

        pointsSg

    
    
    //let drawCracks (model : MCrackDetectionModel) (near: IMod<float>) (far: IMod<float>) (pointSize: IMod<float>) (depthOffset : IMod<float>) =
        
    //    //aset{
    //            //let! cracks = model.cracks
    //            let inputpts = drawPoints model.inputPoints near far pointSize depthOffset
    //            inputpts |> Sg.noEvents
    //       //}  
            
             