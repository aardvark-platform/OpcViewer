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
            
            //{ m with cameras = cameras }
            //let cameras = 
            //    Directory.GetDirectories folderPath 
            //    |> Array.head
            //    |> Directory.GetFiles
            //    |> Array.filter(fun y -> (Path.GetExtension(y) = ".qpos"))
            //    //|> Array.head |> Array.singleton
            //    |> Array.map (fun x -> 
            //        Log.line "[SourceLinking] file name %A" x
            //        CameraShot.fromFileName x
            //    )
            //    |> IndexList.ofArray             

            { m with cameras = cameras }
        | PickQueryPoint pos ->            

            let filteredShots =
                m.cameras 
                |> IndexList.filter(fun shot ->                     

                    let trans = shot.info.position |> Trafo3d.Translation

                    shot.hull.Contains(trans.Backward.TransformPos(pos))
                    //shot.box.Contains(pos)
                )            

            { m with queryPoint = Some pos; filteredCameras = filteredShots }

    let view (m : AdaptiveSourceLinkingModel) : ISg<SourceLinkingAction> = 
                        
        let frustra =
            m.filteredCameras 
            |> AList.map(fun shot -> 
                
                //let trans = shot.info.position|> Trafo3d.Translation 
                //let rot = Trafo3d(shot.info.rotation)
                //let trafo = (shot.proj.Inverse * rot * trans)               

                let dir = shot.info.rotation.Transform(V3d.OOI * (-1.0))
                let color = C4f(dir.Normalized.Abs()).ToC4b()
              
                let trans = shot.info.position|> Trafo3d.Translation 
                let rot = Trafo3d(shot.info.rotation)
                let trafo = (shot.proj.Inverse * rot)

                //option1
                let points = 
                    Box3d(V3d.NNN, V3d.III).ComputeCorners() 
                    |> Array.map(trafo.Forward.TransformPosProj)

                //option2
                //let hull =
                //    Box3d(V3d.NNN, V3d.III) |> Hull3d.Create

                //let points =
                //    hull.Transformed(trafo).ComputeCorners() 
                //    |> Seq.toArray

                
                //option3
               // let hull = (CameraShot.toHull3d2 points)

             //   Log.warn "[view] shot %A hull computation %A" shot.id hull.PlaneArray

                //let points = 
                //    shot.hull.ComputeCorners() |> Seq.toArray


                //let a = Line3d(points.[0], points.[1])
                //let b = Line3d(points.[1], points.[2])
                //let c = Line3d(points.[2], points.[3])
                //let d = Line3d(points.[3], points.[4])
                //let e = Line3d(points.[4], points.[5])
                //let f = Line3d(points.[5], points.[6])
                //let g = Line3d(points.[6], points.[7])
                //let h = Line3d(points.[7], points.[0])

                //Sg.lines ~~C4b.Red ~~[|a;b;c;d;e;f;g;h|]
                //|> Sg.noEvents
                //|> Sg.trafo ~~trans
                //|> Sg.andAlso(
                   
                //)  
                
                Sg.wireBox ~~color (~~Box3d(V3d.NNN, V3d.III))
                |> Sg.noEvents
                |> Sg.trafo ~~(shot.proj.Inverse * rot * trans)
            )
            |> AList.toASet
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor                    
            }    
                        
        let directions =
            m.filteredCameras
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
            m.filteredCameras 
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


        [points; frustra; queryPoint]
        |> Sg.ofList
        