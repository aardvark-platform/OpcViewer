namespace CrackDetection


open System
open System.Runtime.InteropServices
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open IPWrappers
open OpcViewer.Base
open IPWrappers


module V2d =
    let ceil (input : V2d) : V2d =
        V2d(input.X |> ceil, input.Y |> ceil)

    let toV2i (input : V2d) : V2i =
        input.ToV2i()

module CrackDetectionApp = 
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

        if File.Exists @".\crackDetection\CrackDetection.dll" then
            Log.line "blarg"

        if (Directory.Exists logDir) && (Directory.Exists configDir) then
            let errorCode = CrackDetectionWrappers.Init(configDir, logDir)
            CrackDetectionWrappers.EnableDebugLogging(true)
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
        let a  = triangle.Area
        let a1 = Triangle2d(p, triangle.P1, triangle.P2).Area
        let a2 = Triangle2d(triangle.P0, p, triangle.P2).Area
        let a3 = Triangle2d(triangle.P0, triangle.P1, p).Area
                        
        a.ApproximateEquals(a1 + a2 + a3, 1e-6)

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
            |> List.choose(fun t2d ->
                index <- index + 1
                if (insideTriangle t2d position) then
                    Some (t2d,index)
                else 
                    None
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
    
    let getCrackUV (ipoints : plist<InputPoint>) (path:string) (coords2d:string) = 
        
        let edgemap = 
            path |> fromFile<float>

        let fullSize = V2i(edgemap.Dim.X, edgemap.Dim.Y)

        //let crackCoeffs =
        //    ipoints
        //    |> PList.toArray
        //    |> Array.map(fun x -> float32 x.coeff)

        let controlPoints =
            ipoints
            |> PList.toList
            |> List.map(fun x -> 
                V2d(1.0 - x.uv.X, x.uv.Y) * fullSize.ToV2d() |> V2d.ceil
            )
                
        let bbControlPoints = new Box2d(controlPoints)
                
        //not sure if this extension is correct/sufficient
        let windowSize = 
            (bbControlPoints.Size.XY + V2d(4.0, 4.0)) 
            |> V2d.ceil 
            |> V2d.toV2i
        
        let pi =  PixImage<byte>(Col.Format.RGBA, fullSize)

        let coeffsAll =            
            [|
                for y in 0..(windowSize.Y - 1) do
                    for x in 0..(windowSize.X - 1) do
                    
                        let index = x + y * (int)windowSize.X
                        let value = edgemap.Data.[index]

                        pi.GetMatrix<C4b>().SetValue(
                            C4b(value/ 255.0, value/ 255.0, value/ 255.0, 1.0), (int64)x, (int64)y
                        )

                        yield (float32)value
            |]
            
       
        let itest =
            ipoints
            |> PList.toArray
            |> Array.map( fun x ->  
                let changePos = 
                    V2d(1.0 - x.uv.X, x.uv.Y) * fullSize.ToV2d() 
                    |> V2d.ceil 
                    |> V2d.toV2i

                pi.GetMatrix<C3b>().SetCross(changePos, 5, C3b.Red) |> ignore
            )

        let mutable numCrackpoints = 0

        let controlPointsSPoint = 
            controlPoints 
            |> List.map(fun x -> new CrackDetectionWrappers.SPoint2D(x.X, x.Y))
            |> List.toArray

        let err = 
            CrackDetectionWrappers.FindCrack(
                coeffsAll, 
                windowSize.X, 
                windowSize.Y, 
                controlPointsSPoint,
                ipoints.Count, 
                &numCrackpoints
            ) //( coeffs, (int)xdimimg, (int)ydimimg, controlPoints, ipoints.Count, &numCrackpoints)

        let pointsArray = CrackDetectionWrappers.SPoint2D.CreateEmptyArray(uint32 numCrackpoints)
        let err1 = CrackDetectionWrappers.GetCrack(pointsArray)
        
        let points =
            CrackDetectionWrappers.UnMarshalArray<CrackDetectionWrappers.SPoint2D>(
                pointsArray,
                (int)numCrackpoints
            ).ToV2ds()
            |> List.ofSeq 
            |> List.map(fun p -> 
                pi.GetMatrix<C3b>().SetCross(V2i (p.X, p.Y), 5, C3b.Green) |> ignore

                V2d(p.X/(float)windowSize.X, p.Y/(float)windowSize.Y))
            |> PList.ofList
        
        pi.SaveAsImage (@".\edge.png")

        Log.line "[CrackDetection]"
        points
        //PList.empty

    let update (model : CrackDetectionModel) (msg : CrackDetectionAction) =
        match msg with
        | AddCrackPoint (p,uv,cCoeff, indx) -> 
            let ip = { position = p; uv = uv; coeff = cCoeff; index = indx }
            {model with inputPoints = model.inputPoints |> PList.prepend ip } 
        | FinishCrack (path, texpath) -> 
            let uvPositions = getCrackUV model.inputPoints path texpath // model.inputUV
            //let crack = { uvPoints2d = uvPositions; points3d = plist.Empty }
            //TODO: uv texcoords to v3d points
            { model with outputPoints = uvPositions}

    let viewBrush near far (model : MCrackDetectionModel) =
        
        let color = ~~C4b.DarkMagenta
        let pointsSg = 
            model.inputPoints
            |> AList.map(fun x -> x.position)            
            |> SgUtilities.drawPointList color ~~10.0 ~~0.1 near far
        
        pointsSg            
        |> Sg.noEvents
             