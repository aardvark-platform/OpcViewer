namespace CrackDetection

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open OpcViewer.Base

[<AutoOpen>]
module private Utilities =

    module V2d =
        let ceil (input : V2d) : V2d =
            V2d(input.X |> ceil, input.Y |> ceil)

    module Matrix =
        let fromVolume (v : Volume<'a>) =
            v.SubXYMatrix 0L

        let toPixImage (matrix : Matrix<C3b>) =
            TensorExtensions.ToPixImage<byte> matrix

        let map (map : 'a -> 'b) (input : Matrix<'a>) : Matrix<'b> =        
            let array = input.Array.ToArrayOfT<'a>() |> Array.map map
            Matrix(array, input.Size)

module CrackDetectionApp =

    // Transform the given position in UV coordinate space to
    // the world space position by doing a texture lookup in the
    // positions.aara attribute map.
    let private uv2ws (posPath : string) (trafo : Trafo3d) (position : V2d) =
        let positionMap =
            posPath
            |> fromFile<V3f>
            |> Matrix.fromVolume
            |> Matrix.map (V3d >> trafo.Forward.TransformPos)

        let lerp =
            Func<float, V3d, V3d, V3d>(
                fun t a b -> VecFun.Lerp(t, a, b)
            )

        positionMap.Sample4Clamped(position, lerp, lerp)

    // Saves the results as an image
    let private saveResultImage (edgeMap : Matrix<float>) (controlPoints : V2d seq) (crackPoints : V2f seq) =
        let resultImage =
            edgeMap
            |> Matrix.map (byte >> C3b)
            |> Matrix.toPixImage

        controlPoints
        |> Seq.iter(fun x ->
            resultImage.ChannelArray.[0].SetCross(x, 2.0, 255uy)
        )
        
        crackPoints
        |> Seq.map V2d
        |> Seq.pairwise
        |> Seq.iter (fun (a, b) ->
            resultImage.ChannelArray.[2].SetLine(a, b, 255uy)
        )

        resultImage.SaveAsImage (@".\cracks.png")

    // Finds a crack given some input points
    // edgeMapPath is the location of the edge map
    // posPath is the location of the attribute map containing local coordinates
    // trafo is the transformation to compute world space positions from local coordinates
    let private findCrack (edgeMapPath : string) (posPath : string) (trafo : Trafo3d) (ipoints : InputPoint plist) = 
        
        // Load edge map
        let edgeMap = 
            edgeMapPath
            |> fromFile<float>
            |> Matrix.fromVolume

        let size = edgeMap.Dim.XY

        let coeff = 
            edgeMap |> Matrix.map float32
        
        // Control points
        let controlPoints =
            ipoints
            |> PList.toList
            |> List.map(fun x -> 
                V2d(1.0 - x.uv.X, x.uv.Y) * (size.ToV2d() - V2d.One) |> V2d.ceil
            )

        let controlPointsArr = 
            controlPoints 
            |> List.map V2f
            |> List.toArray

        // Find the crack
        let (w, h) = (uint32 size.X, uint32 size.Y)
        let crackPoints = Wrapper.findCrack w h coeff.Data controlPointsArr

        // Save results as image
        saveResultImage edgeMap controlPoints crackPoints

        // Return points
        crackPoints
        |> Array.map (fun p ->
            let q = V2d p
            { uv = q; position = q |> uv2ws posPath trafo }
        )
        |> PList.ofArray

    let initModel =
        {
            inputPoints  = PList.empty
            outputPoints = PList.empty
            kdTreePath = None
        }

    /// Initializes the native library
    let initialize () =
        let logDir = @".\crackDetection" 
        let configDir = @".\crackDetection"

        Wrapper.initialize configDir logDir

    /// Frees resources of the native library
    let free =
        Wrapper.free

    let update (model : CrackDetectionModel) (msg : CrackDetectionAction) =
        match msg with
        | AddCrackPoint (p, uv, coeff, index) -> 
            let ip = { position = p; uv = uv; coeff = coeff; index = index }
            { model with inputPoints = model.inputPoints |> PList.prepend ip } 

        | FinishCrack (edgeMapPath, posPath, trafo) -> 
            let op = model.inputPoints |> findCrack edgeMapPath posPath trafo
            { model with outputPoints = op }

    let viewBrush near far (model : MCrackDetectionModel) =
        let points = alist {
            for p in model.inputPoints do
                let! pos = p.position
                yield pos
        }
        
        let color = ~~C4b.DarkMagenta

        let pointsSg = 
            points     
            |> SgUtilities.drawPointList color ~~10.0 ~~0.1 near far
        
        pointsSg            
        |> Sg.noEvents
             