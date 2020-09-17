namespace OpcViewer.SourceLinking

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Ag
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open OpcViewer.Base

module SourceLinkingApp =

    let update (m : SourceLinkingModel) (a : SourceLinkingAction ) : SourceLinkingModel =
        match a with
        | LoadCameras folderPath -> 
            let cameras = 
                Directory.GetDirectories folderPath 
                |> Array.map (fun x -> 
                    (Directory.GetFiles x) 
                    |> Array.filter(fun y -> Path.GetExtension(y) = ".qpos")
                )
                |> Array.collect (fun x -> x |> Array.map CameraShot.fromFileName)
                |> IndexList.ofArray

            { m with cameras = cameras }

    let view (m : AdaptiveSourceLinkingModel) : ISg<SourceLinkingAction> = 
                

        let createFrustumProj (c: CameraShot) =
            let aspectRatio = (float c.imageSize.X) / (float c.imageSize.Y)

            let hFov = (float c.imageSize.X / 2.0) / c.info.focalLength.X |> atan 

            Log.line "hFov %A" (hFov.DegreesFromRadians())

            let vFov = hFov * aspectRatio
            let frustum = Frustum.perspective (vFov.DegreesFromRadians()) 0.01 5.0 aspectRatio
          //  let fullFrustum = Frustum.perspective fov 0.01 1000.0 aspectRatio
            let proj = Frustum.projTrafo(frustum)
            (proj, proj.Inverse)

        let frustra =
            m.cameras 
            |> AList.map(fun x -> 
                let proj, iProj = x |> createFrustumProj
                let trans = x.info.position|> Trafo3d.Translation 
                let rot = Trafo3d(x.info.rotation)
                let trafo = (iProj * rot * trans) |> AVal.constant

                let dir = x.info.rotation.Transform(V3d.OOI * (-1.0))
                let color = C4f(dir.Normalized.Abs()).ToC4b()

                Sg.wireBox (AVal.constant color) (AVal.constant (Box3d(V3d.NNN, V3d.III)))
                |> Sg.noEvents
                |> Sg.trafo trafo
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor                    
            }    
            
            

        let directions =
            m.cameras
            |> AList.map(fun x ->
                let dir = x.info.rotation.Transform(V3d.OOI * (-7.0))
                let color = C4f(dir.Normalized.Abs()).ToC4b()

                let line = Line3d(V3d.Zero, dir)


                Sg.lines (AVal.constant color) (AVal.constant [|line|])
                |> Sg.noEvents               
                |> Sg.trafo (x.info.position|> Trafo3d.Translation |> AVal.constant)            
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.uniform "LineWidth" (AVal.constant 1.0)
            |> Sg.uniform "DepthOffset" (AVal.constant 0.0)
            |> Sg.effect [
                Shader.StableTrafo.Effect
                Shader.ThickLineNew.Effect                
            ]

        let points = 
            m.cameras 
            |> AList.map(fun x -> 
                Sg.sphere 2 (AVal.constant C4b.Red) (AVal.constant 0.03)
                |> Sg.trafo (x.info.position|> Trafo3d.Translation |> AVal.constant)
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.stableHeadlight
            }                    

        [points; frustra] 
        |> Sg.ofList
        