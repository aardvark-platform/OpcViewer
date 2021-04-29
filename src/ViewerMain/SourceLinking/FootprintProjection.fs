namespace OpcViewer.SourceLinking

open Aardvark.Base
open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI

open OpcViewer.Base
open OpcViewer.Base.StencilVolumes

module FootprintProjection = 

    let colorAlpha (color:aval<C4b>) (alpha:aval<float>) : aval<V4f> = 
        AVal.map2 (fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) color alpha

    // sequentiel...correct Alphablending
    let drawFootPrints (firstRenderPass: RenderPass) (footprints : alist<CameraShot>) =
    
        let mutable maskPass = firstRenderPass
        let mutable areaPass = RenderPass.after "" RenderPassOrder.Arbitrary maskPass

        let sg = 
            footprints
            |> AList.map (fun x -> 
                let trans = x.info.position|> Trafo3d.Translation 
                let rot = Trafo3d(x.info.rotation)
                let trafo = (x.proj.Inverse * rot * trans)
                
                let points = 
                    Box3d(V3d.NNN, V3d.III).ComputeCorners()
                    |> Array.map(trafo.Forward.TransformPosProj)

                let points = [|
                    points.[4]
                    points.[5]
                    points.[7]
                    points.[6]
                    points.[4]
                |]

                let colorAlpha = colorAlpha ~~C4b.Red ~~0.2
                let sg = 
                    ClippingVolume.clippingVolume colorAlpha ~~20.0 ~~(ClippingVolumeType.Point x.info.position) (points |> AList.ofArray)
                    |> Sg.effect [
                        toEffect DefaultSurfaces.stableTrafo
                        toEffect DefaultSurfaces.vertexColor
                    ]
                let coloredPolygon =
                    sg  |> StencilAreaMasking.stencilAreaGrouped maskPass areaPass colorAlpha

                maskPass <- RenderPass.after "" RenderPassOrder.Arbitrary areaPass
                areaPass <- RenderPass.after "" RenderPassOrder.Arbitrary maskPass
                
                [
                    coloredPolygon
                    ~~false |> AVal.map (fun show -> if show then sg |> ClippingVolume.drawClippingVolumeDebug else Sg.empty) |> Sg.dynamic
                ] |> Sg.ofList)
            |> AList.toASet
            |> Sg.set

        let nextRenderPass = RenderPass.after "" RenderPassOrder.Arbitrary areaPass
        (sg, nextRenderPass)

