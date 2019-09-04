namespace OpcViewer.Base

open Aardvark.Base

module IndexHelper =

    let computeIndexArray (size: V2i) (invalidPoints: seq<int>) : int[] =

        let indexArray = Array.zeroCreate ((size.X - 1) * (size.Y - 1) * 6)

        let mutable k = 0;

        match invalidPoints.IsEmptyOrNull() with
        | true -> 
            // has no invalids
            for y in 0 .. (size.Y-2) do
                for x in 0 .. (size.X-2) do
                    indexArray.[k] <- y * size.X + x
                    indexArray.[k + 1] <- (y + 1) * size.X + x
                    indexArray.[k + 2] <- y * size.X + x + 1

                    indexArray.[k + 3] <- y * size.X + x + 1
                    indexArray.[k + 4] <- (y + 1) * size.X + x
                    indexArray.[k + 5] <- (y + 1) * size.X + x + 1

                    k <- k + 6;
        | false ->
            let invalidDict = invalidPoints |> HashSet.ofSeq
            let mutable counter = 0

            for y in 0 .. (size.Y-2) do
                for x in 0 .. (size.X-2) do
                    let a1 = y * size.X + x
                    let b1 = (y + 1) * size.X + x
                    let c1 = y * size.X + x + 1

                    let a2 = y * size.X + x + 1
                    let b2 = (y + 1) * size.X + x
                    let c2 = (y + 1) * size.X + x + 1

                    let indices = [a1; b1; c1; a2; b2; c2]

                    let invalidFace = indices |> List.filter (fun x -> invalidDict.Contains(x)) |> List.length > 0

                    if invalidFace then
                        counter <- counter + 1
                    else 
                    indexArray.[k] <- a1
                    indexArray.[k + 1] <- b1
                    indexArray.[k + 2] <- c1

                    indexArray.[k + 3] <- a2
                    indexArray.[k + 4] <- b2
                    indexArray.[k + 5] <- c2 

                    k <- k + 6

                if (counter > 0) then
                    Report.Line(5, "Invalid faces found: {0}", counter)

        indexArray

    let isOversizedTriangle (points: V3f[]) (index0: int) (index1: int) (index2: int) (maxTriangleSize: float32) : bool =

        let point0 = points.[index0]
        let point1 = points.[index1]
        let point2 = points.[index2]
 
        [|
            V3f.Distance(point0, point1)
            V3f.Distance(point0, point2)
            V3f.Distance(point1, point2)
        |] 
        |> Array.exists (fun d -> d > maxTriangleSize)