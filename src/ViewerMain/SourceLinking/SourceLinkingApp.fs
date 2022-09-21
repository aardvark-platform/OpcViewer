namespace OpcViewer.SourceLinking

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Ag
open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Operators
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
open OpcViewer.Base

module SourceLinkingApp =

    let update (m : SourceLinkingModel) (a : SourceLinkingAction ) : SourceLinkingModel =
        match a with
        | LoadCameras folderPath ->
            let cameras = 
                Directory.GetDirectories folderPath 
                |> Array.map (fun x -> 
                    (Directory.GetFiles x) 
                    |> Array.filter(fun y -> (Path.GetExtension(y) = ".qpos"))                    
                )
                |> Array.collect (fun x -> 
                    Array.map CameraShot.fromFileName x                    
                )
                |> IndexList.ofArray
            
            { m with cameras = cameras }
            //let cameras = 
            //    Directory.GetDirectories folderPath 
            //    |> Array.head
            //    |> Directory.GetFiles
            //    |> Array.filter(fun y -> (Path.GetExtension(y) = ".qpos"))
            //    |> Array.head |> Array.singleton
            //    |> Array.map (fun x -> 
            //        Log.line "[SourceLinking] file name %A" x
            //        CameraShot.fromFileName x
            //    )
            //    |> IndexList.ofArray             

            //{ m with cameras = cameras }
        | PickQueryPoint pos ->            

            let filteredShots =
                m.cameras 
                |> IndexList.filter(fun shot ->                     

                    let trans = shot.info.position |> Trafo3d.Translation

                    shot.hull.Contains(trans.Backward.TransformPos(pos))
                    //shot.box.Contains(pos)
                )

            let filteredShots =
                filteredShots
                |> IndexList.map(fun x -> 
                    //|> fun p -> V4d(p, 1.0)
                    //|> prod.trafoInv.Forward.Transform
                    //|> fun p -> V3d((p.XY / p.W), p.Z)


                    
                    //let viewTrafo = (rot * trans).Inverse

                    //|> fun p -> V4d(p, 1.0)
                    //|> prod.trafoInv.Forward.Transform
                    //|> fun p -> V3d((p.XY / p.W), p.Z)

                    //let point = viewTrafo.Forward.TransformPos(pos)

                    //Log.line "[blurg2] %A" point
                    let proj = x.proj.Inverse
                    let trans = (x.info.position |> Trafo3d.Translation)
                    let rot = (x.info.rotation) |> Trafo3d
                    
                    let reprojectedQueryPoint = 
                        //(proj * rot * trans).Backward.TransformPos(pos)
                        (proj * rot * trans).Backward.Transform(pos.XYZI)
                        |> fun p -> ((V3d((p.XY / p.W), p.Z).XY) + V2d.One) * 0.5 //

                    Log.line "[blurg2] %A" reprojectedQueryPoint
                                        

                    Log.warn "[SourceLinking] %A" x.tifFile
                    //trans.Forward.TransformPos(reprojectedQueryPoint)
                    x, reprojectedQueryPoint
                )
                |> IndexList.sortBy(fun (_, point) ->
                    let distance = point.DistanceSquared(V2d.One * 0.5)
                    Log.line "[distance] %A %A" point distance
                    distance
                )

            { m with queryPoint = Some pos; filteredCameras = filteredShots; reprojectedQueryPoints = IndexList.empty }

    let view (m : AdaptiveSourceLinkingModel) : ISg<SourceLinkingAction> = 
                        
        let frustra =
            m.filteredCameras 
            |> AList.map(fun (shot, _) -> 
                                
                let dir = shot.info.rotation.Transform(V3d.OOI * (-1.0))
                let color = C4f(dir.Normalized.Abs()).ToC4b()
              
                let trans = shot.info.position|> Trafo3d.Translation 
                let rot = Trafo3d(shot.info.rotation)
                let trafo = (shot.proj.Inverse * rot)
                
                let points = 
                    Box3d(V3d.NNN, V3d.III).ComputeCorners() 
                    |> Array.map(trafo.Forward.TransformPosProj)

                //let a = Line3d(points.[0], points.[1])
                //let b = Line3d(points.[1], points.[2])
                //let c = Line3d(points.[2], points.[3])
                //let d = Line3d(points.[3], points.[4])
                //let e = Line3d(points.[4], points.[5])
                //let f = Line3d(points.[5], points.[6])
                //let g = Line3d(points.[6], points.[7])
                //let h = Line3d(points.[7], points.[0])

                let a = Line3d(points.[4], points.[5])
                let b = Line3d(points.[5], points.[7])
                let c = Line3d(points.[7], points.[6])
                let d = Line3d(points.[6], points.[4])

                Sg.lines ~~C4b.Red ~~[|a;b;c;d|]
                |> Sg.noEvents
                |> Sg.trafo ~~trans
                |> Sg.andAlso(                                                     
                    Sg.wireBox ~~color (~~Box3d(V3d.NNN, V3d.III))
                    |> Sg.noEvents
                    |> Sg.trafo ~~(shot.proj.Inverse * rot * trans)
                )
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.uniform "LineWidth" (AVal.constant 3.0)
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor                    
            }    
                        
        let directions =
            m.filteredCameras
            |> AList.map(fun (x, _) ->
                let dir = x.info.rotation.Transform(V3d.OON)
                let color = C4f(dir.Normalized.Abs()).ToC4b()

                let line = Line3d(V3d.Zero, dir)


                Sg.lines (AVal.constant color) (AVal.constant [|line|])
                |> Sg.noEvents               
                |> Sg.trafo (x.info.position|> Trafo3d.Translation |> AVal.constant)            
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.uniform "LineWidth" (AVal.constant 3.0)
            |> Sg.uniform "DepthOffset" (AVal.constant 0.0)
            |> Sg.effect [
                Shader.StableTrafo.Effect
                Shader.ThickLineNew.Effect                
            ]

        let points = 
            m.filteredCameras 
            |> AList.map(fun (x, _) -> 
                let dir = x.info.rotation.Transform(V3d.OOI)
                let color = C4f(dir.Normalized.Abs()).ToC4b()

                Sg.sphere 2 ~~color ~~0.03
                |> Sg.trafo (x.info.position|> Trafo3d.Translation |> AVal.constant)
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.stableHeadlight
            }       

        //let queryPoints = 
        //    m.reprojectedQueryPoints
        //    |> AList.map(fun x -> 
        //        Sg.sphere 2 (AVal.constant C4b.Yellow) (AVal.constant 0.03)
        //        |> Sg.trafo (x |> Trafo3d.Translation |> AVal.constant)
        //    )
        //    |> AList.toASet
        //    |> Sg.set
        //    |> Sg.shader {
        //        do! DefaultSurfaces.stableTrafo
        //        do! DefaultSurfaces.vertexColor
        //        do! DefaultSurfaces.stableHeadlight
        //    }       
            
        let queryPoint =
            m.queryPoint 
            |> AVal.map(fun x ->
                match x with
                | Some q -> 
                    Sg.sphere 4 (AVal.constant C4b.Crimson) (AVal.constant 0.05)
                    |> Sg.trafo (q |> Trafo3d.Translation |> AVal.constant)
                | None ->
                    Sg.empty            
            ) 
            |> Sg.dynamic
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.stableHeadlight
            }   

        let footprints, nextRenderPass = 
            m.filteredCameras 
            |> AList.map(fst) 
            |> FootprintProjection.drawFootPrints (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)


        let solidSg = [points; frustra; directions; queryPoint] |> Sg.ofList |> Sg.pass nextRenderPass

        [
            footprints
            solidSg
        ]        
        |> Sg.ofList
        |> Sg.fillMode (AVal.constant FillMode.Fill)
        |> Sg.cullMode (AVal.constant CullMode.None)
        
    let cssColor (c: C4b) =
        sprintf "rgba(%d, %d, %d, %f)" c.R c.G c.B c.Opacity

    let viewImagesList (model:AdaptiveSourceLinkingModel) = 
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {                
                for (cam, query) in model.filteredCameras do                                        

                    Log.line "x:%f%% y:%f%%" (query.X * 100.0) (query.Y * 100.0)                    

                    yield Svg.svg [attribute "viewBox" (sprintf "0 0 %d %d" cam.imageSize.Y cam.imageSize.X)] [
                        Svg.image [attribute "href" (Path.ChangeExtension(cam.tifFile, ".png")); attribute "width" "100%"]                        
                        Svg.circle[
                            attribute "cx" (sprintf "%f%%" ((1.0-query.Y) * 100.0))
                            attribute "cy" (sprintf "%f%%" ((1.0-query.X) * 100.0))
                            attribute "r" "100.0"
                            attribute "fill" "transparent"
                            attribute "stroke" (C4b.Red |> cssColor)
                            attribute "stroke-width" "10.0"
                        ]                                        
                    ]
                   // yield img [attribute "src" cam.tifFile]
            }
        )

    