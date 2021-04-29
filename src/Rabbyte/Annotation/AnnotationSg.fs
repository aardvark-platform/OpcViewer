namespace Rabbyte.Annotation

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI

open OpcViewer.Base
open OpcViewer.Base.StencilVolumes

module AnnotationSg =    

    // grouped...fast -> alpha broken
    let drawAnnotationsFilledGrouped (firstRenderPass: RenderPass) (model: AdaptiveAnnotationModel) =
    
        let mutable maskPass = firstRenderPass
        let mutable areaPass = RenderPass.after "" RenderPassOrder.Arbitrary maskPass

        let sg = 
            model.annotationsGrouped 
            |> AMap.map (fun groupColor annotations -> 
                let colorAlpha = SgUtilities.colorAlpha (AVal.constant groupColor) (AVal.constant 0.5)
                let groupedSg = 
                    annotations
                    |> AList.map (fun x -> ClippingVolume.clippingVolume colorAlpha model.extrusionOffset x.clippingVolume x.points)
                    |> AList.toASet
                    |> Sg.set
                    |> Sg.effect [
                        Shader.StableTrafo.Effect
                        toEffect DefaultSurfaces.vertexColor
                    ]
                let coloredPolygon =
                    groupedSg
                    |> StencilAreaMasking.stencilAreaGrouped maskPass areaPass colorAlpha   

                maskPass <- RenderPass.after "" RenderPassOrder.Arbitrary areaPass
                areaPass <- RenderPass.after "" RenderPassOrder.Arbitrary maskPass
                
                [
                    coloredPolygon
                    model.showDebug |> AVal.map (fun show -> if show then groupedSg |> ClippingVolume.drawClippingVolumeDebug else Sg.empty) |> Sg.dynamic
                ] |> Sg.ofList)
            |> AMap.toASet
            |> ASet.map snd
            |> Sg.set

        

        let nextRenderPass = RenderPass.after "" RenderPassOrder.Arbitrary areaPass
        (sg, nextRenderPass)

    // sequentiel...correct Alphablending
    let drawAnnotationsFilledSeq (firstRenderPass: RenderPass) (model: AdaptiveAnnotationModel) =
    
        let mutable maskPass = firstRenderPass
        let mutable areaPass = RenderPass.after "" RenderPassOrder.Arbitrary maskPass

        let sg = 
            model.annotations 
            |> AList.map (fun x -> 
                let colorAlpha = SgUtilities.colorAlpha x.style.primary.c (AVal.constant 0.5)
                let sg = 
                    ClippingVolume.clippingVolume colorAlpha model.extrusionOffset x.clippingVolume x.points
                    |> Sg.effect [
                        Shader.StableTrafo.Effect
                        toEffect DefaultSurfaces.vertexColor
                    ]
                let coloredPolygon =
                    sg  |> StencilAreaMasking.stencilAreaGrouped maskPass areaPass colorAlpha

                maskPass <- RenderPass.after "" RenderPassOrder.Arbitrary areaPass
                areaPass <- RenderPass.after "" RenderPassOrder.Arbitrary maskPass
                
                [
                    coloredPolygon
                    model.showDebug |> AVal.map (fun show -> if show then sg |> ClippingVolume.drawClippingVolumeDebug else Sg.empty) |> Sg.dynamic
                ] |> Sg.ofList)
            |> AList.toASet
            |> Sg.set

        let nextRenderPass = RenderPass.after "" RenderPassOrder.Arbitrary areaPass
        (sg, nextRenderPass)