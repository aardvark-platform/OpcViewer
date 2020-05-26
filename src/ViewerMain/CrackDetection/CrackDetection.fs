namespace CrackDetection

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open OpcViewer.Base
open OpcViewer.Base.Picking
open System.Diagnostics

module CrackDetectionApp =

    // Saves the results as an image
    let private saveResultImage (edgeMap : Matrix<float>) (controlPoints : #seq<V2i>) (crackPoints : #seq<V2f>) =
        let resultImage =
            edgeMap
            |> Matrix.map byte
            |> Array.create 3
            |> Matrix.toPixImage

        resultImage.SaveAsImage (@".\cracks_orig.png")

        controlPoints
        |> Seq.iter(fun x ->
            for i in 0 .. 2 do
                resultImage.ChannelArray.[i].SetCross(x, 4, if i = 0 then 255uy else 0uy)
        )

        crackPoints
        |> Seq.map V2d
        |> Seq.iter(fun x ->
            for i in 0 .. 2 do
                resultImage.ChannelArray.[i].SetCross(x, 1.0, if i = 2 then 255uy else 0uy)
        )

        //connect crackpoints
        //crackPoints
        //|> Seq.map V2d
        //|> Seq.pairwise
        //|> Seq.iter (fun (a, b) ->
        //    for i in 0 .. 2 do
        //        resultImage.ChannelArray.[i].SetLine(a, b, if i = 2 then 255uy else 0uy)
        //)

        resultImage.SaveAsImage (@".\cracks.png")

    // Finds a crack given some hit points
    // hierarchies is a sequence of all patch hierarchies
    let private findCrack (hierarchies : #seq<PatchHierarchy>) (points : HitInfo plist) =

        // Find distinct kd trees and load SvBR maps
        let svbrMaps =
            points
            |> Seq.map (fun h -> PatchId.ofHit h, h.kdTree)
            |> Seq.distinctBy fst
            |> Seq.map (fun (id, kd) -> id, KdTree.loadSvBRMap kd)
            |> Map.ofSeq

        // Compute points in global SvB space
        let toGlobal2d (hit : HitInfo) =
            let id = PatchId.ofHit hit
            let map = svbrMaps.[id]
            let trafo = hit.kdTree.positions2dAffine.Forward

            hit.triangle
            |> TriangleHit.getV3d map.Data
            |> Mat.transformPos trafo |> Vec.xy

        let points2d =
            points |> Seq.map toGlobal2d

        // Find patches in all OPC hierarchies that are intersected
        let boundingBox2d =
            points2d.GetBoundingBox2d()

        let leafs =
            hierarchies
            |> Seq.map (fun h ->
                let opcPath = h.opcPaths.Opc_DirAbsPath
                let rootPath = h.opcPaths.Patches_DirAbsPath

                h.tree
                |> QTree.intersect boundingBox2d
                |> List.map (PatchInfo.create opcPath rootPath)
            )
            |> List.concat

        // Create a patch map
        let map =
            leafs |> PatchMap.create Texture

        // Load edge map
        let edgeMap =
            PatchMap.loadEdgeMap map

        let size = edgeMap.Dim.XY

        let coeff =
            edgeMap |> Matrix.map float32

        // Control points
        let controlPoints =
            points |> Seq.choose (PatchMap.hit2map map)

        let controlPointsArr =
            controlPoints
            |> Seq.map V2f
            |> Seq.toArray

        if controlPointsArr.Length > 0 then

            saveResultImage edgeMap controlPoints Seq.empty

            // Find the crack
            let (w, h) = (uint32 size.X, uint32 size.Y)
            let crackPoints = 
                Wrapper.findCrack w h coeff.Data controlPointsArr

            // Save results as image
            saveResultImage edgeMap controlPoints crackPoints

            Process.Start("explorer.exe", @".\cracks.png") |> ignore

            let hasFloatingPart (p : V2f) =
                let x = p.X - p.X.Floor()
                let y = p.Y - p.Y.Floor()
                (x > 0.0f) || (y > 0.0f)

            if crackPoints |> Seq.exists hasFloatingPart then
                failwith "floating!!!"

            // Return points
            crackPoints
            |> Array.choose (V2i >> PatchMap.map2global map)
            |> PList.ofArray
        else
            PList.empty

    let initModel =
        {
            inputPoints  = PList.empty
            outputPoints = PList.empty
        }

    /// Initializes the native library
    let initialize () =
        let logDir = @".\crackDetection"
        let configDir = @".\crackDetection"

        Wrapper.initialize configDir logDir
        Wrapper.setDebugLogging true
        Log.line "[CrackDetection] init dll version %A"  Wrapper.getLibraryVersion

    /// Frees resources of the native library
    let free =
        Wrapper.free

    let update (msg : CrackDetectionAction) (model : CrackDetectionModel) =
        match msg with
        | AddCrackPoint hit ->
            { model with inputPoints = model.inputPoints |> PList.prepend hit }

        | FinishCrack hierarchies ->
            let op = model.inputPoints |> findCrack hierarchies
            { model with outputPoints = op; inputPoints = PList.empty }

    let viewBrush near far (model : MCrackDetectionModel) =
        let points = alist {
            for p in model.inputPoints do
                let! pos = p.position
                yield pos
        }

        let color = ~~C4b.DarkMagenta

        let pointsSg =
            points
            |> SgUtilities.drawPointList color ~~4.0 ~~0.1 near far

        pointsSg
        |> Sg.noEvents