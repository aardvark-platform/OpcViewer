namespace PRo3D.Minerva

open System.IO
open System.Diagnostics

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.UI

open FShade

module Files = 

    let loadTifAndConvert (access: string) (featureId: string) =
        let filename = featureId.ToLower() + ".tif"
        let imagePath = @".\MinervaData\" + filename
        let targetPath = @".\MinervaData\" + featureId.ToLower() + ".png"
        let path = "https://minerva.eox.at/store/datafile/" + featureId + "/" + filename
        match (File.Exists imagePath) with
        | true -> ()
        | false -> 
            let mutable client = new System.Net.WebClient()
            client.UseDefaultCredentials <- true       
            let credentials = System.Convert.ToBase64String(System.Text.Encoding.ASCII.GetBytes(access))   
            client.Headers.[System.Net.HttpRequestHeader.Authorization] <- "Basic " + credentials  
            try
                client.DownloadFile(path, imagePath) |> ignore
                let targetPath = @".\MinervaData\" + featureId.ToLower() + ".png"
                PixImage.Create(imagePath).ToPixImage<byte>().SaveAsImage(targetPath)
            with e -> ()//Log.line "[Minerva] error: %A" e
    
    let loadTif (featureId:string) (model:MinervaModel) =
        // https://minerva.eox.at/store/datafile/FRB_495137799RADLF0492626FHAZ00323M1/frb_495137799radlf0492626fhaz00323m1.tif
        let filename = featureId.ToLower() + ".tif" //"frb_495137799radlf0492626fhaz00323m1.tif"
        let imagePath = @".\MinervaData\" + filename
        let path = "https://minerva.eox.at/store/datafile/" + featureId + "/" + filename
        match (File.Exists imagePath) with
         | true -> 
            let argument = sprintf "/select, \"%s\"" imagePath
            Process.Start("explorer.exe", argument) |> ignore
            model
         |  false -> 
            let mutable client = new System.Net.WebClient()
            client.UseDefaultCredentials <- true       
            let credentials = System.Convert.ToBase64String(System.Text.Encoding.ASCII.GetBytes("minerva:tai8Ies7"))   
            client.Headers.[System.Net.HttpRequestHeader.Authorization] <- "Basic " + credentials  
            //try takeScreenshot baseAddress sh.col sh.row sh.id sh.folder with e -> printfn "error: %A" e
            try (client.DownloadFile(path, imagePath) |> ignore) with e -> Log.error "[Minerva] error: %A" e
            match (File.Exists imagePath) with
                | true -> let argument = sprintf "/select, \"%s\"" imagePath
                          Process.Start("explorer.exe", argument) |> ignore
                | _ -> Log.error "[Minerva] sth. went wrong with tif file"
            model
             
module Shader = 

    type UniformScope with
        member x.PointSize : float = uniform?PointSize

    type pointVertex =
        {
            [<Position>] pos : V4d
            [<PointSize>] p : float
            [<Color>] c : V4d
            [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2d
            [<SourceVertexIndex>] i : int
        }

    let constantColor (color : V4d) (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = color; p = ps }
        }

    let differentColor (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = v.c; p = ps }
        }

    let pointTrafo (v : pointVertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ProjTrafo * vp
            }
        }

    let pointSpriteFragment (v : pointVertex) =
        fragment {
            let tc = v.tc

            let c = 2.0 * tc - V2d.II
            if c.Length > 1.0 then
                discard()

            return v
        }

    let lines (t : Triangle<pointVertex>) =
        line {
            yield t.P0
            yield t.P1
            restartStrip()
            
            yield t.P1
            yield t.P2
            restartStrip()

            yield t.P2
            yield t.P0
            restartStrip()
        }

module DataStructures =
    type HarriSchirchWrongBlockingCollection<'a>() =
        let sema = new System.Threading.SemaphoreSlim(0)
        let l = obj()
        let queue = System.Collections.Generic.Queue<'a>()
        let mutable finished = false
    
        member x.TakeAsync() =
            async {
                do! sema.WaitAsync() |> Async.AwaitTask
                if finished then return None
                else
                    return 
                        lock l (fun _ -> 
                            queue.Dequeue() |> Some
                        )
            }
    
        member x.Enqueue(v) =
            lock l (fun _ -> 
                queue.Enqueue(v)
            )
            sema.Release() |> ignore
    
        member x.CompleteAdding() =
            finished <- true
            sema.Release()
    
        member x.IsCompleted = finished

module Drawing =

    let drawSingleColorPoints pointsF color pointSize = 
      Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.uniform "PointSize" pointSize
      |> Sg.effect [
            toEffect Shader.pointTrafo
            toEffect (Shader.constantColor color)
            toEffect DefaultSurfaces.pointSprite
            toEffect Shader.pointSpriteFragment
      ]
    
    let drawColoredPoints pointsF colors pointSize = 
      Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.vertexAttribute DefaultSemantic.Colors colors
      |> Sg.uniform "PointSize" pointSize
      |> Sg.effect [
          toEffect Shader.pointTrafo 
          toEffect Shader.differentColor
          toEffect DefaultSurfaces.pointSprite
          toEffect Shader.pointSpriteFragment
        ]

    let stablePoints (sgfeatures : MSgFeatures) =
      sgfeatures.positions 
        |> Mod.map2(fun (t : Trafo3d) x -> 
          x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) sgfeatures.trafo

    let drawSingleColoredFeaturePoints (sgfeatures : MSgFeatures) (pointSize:IMod<float>) (color:C4f) = 
        let pointsF = stablePoints sgfeatures
        drawSingleColorPoints pointsF (color.ToV4d()) pointSize |> Sg.trafo sgfeatures.trafo

    let drawFeaturePoints (sgfeatures : MSgFeatures) (pointSize:IMod<float>) = 
        let pointsF = stablePoints sgfeatures
        drawColoredPoints pointsF sgfeatures.colors pointSize |> Sg.trafo sgfeatures.trafo

    let drawHoveredFeaturePoint hoveredProduct pointSize trafo =
      let hoveredPoint = 
        Mod.map2(fun (x:Option<SelectedProduct>) (t:Trafo3d) ->  
          match x with 
          | None -> [||]
          | Some a ->  [|t.Backward.TransformPos(a.pos)|]) hoveredProduct trafo 

      drawSingleColorPoints hoveredPoint (C4f.Yellow.ToV4d()) (pointSize |> Mod.map(fun x -> x + 5.0)) |> Sg.trafo trafo

    let pass0 = RenderPass.main
    let pass1 = RenderPass.after "outline" RenderPassOrder.Arbitrary pass0

    let drawSelectedFeaturePoints (sgfeatures : MSgFeatures) (pointSize:IMod<float>) =

        let outline =
          drawSingleColoredFeaturePoints sgfeatures (pointSize |> Mod.map(fun x -> x + 4.0)) C4f.VRVisGreen 
          |> Sg.pass pass0

        let inside = 
          drawFeaturePoints sgfeatures pointSize
          |> Sg.pass pass1
          |> Sg.depthTest (Mod.constant(DepthTestMode.Always))

        Sg.ofList [inside; outline]

    let coneISg color radius height trafo =  
        Sg.cone 30 color radius height
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.vertexColor
            //do! Shader.stableLight
        }
        |> Sg.trafo(trafo)

    let featureMousePick (boundingBox : IMod<Box3d>) =
      boundingBox 
        |> Mod.map(fun box ->  
          Sg.empty 
            |> Sg.pickable (PickShape.Box box)
            |> Sg.withEvents [
                SceneEventKind.Click, (fun sceneHit -> true, Seq.ofList[PickProducts sceneHit]) 
                SceneEventKind.Move,  (fun sceneHit -> true, Seq.ofList[HoverProducts sceneHit])
            ])
        |> Sg.dynamic 

    let computeInvariantScale (view : IMod<CameraView>) (near : IMod<float>) (p:V3d) (size:IMod<float>) (hfov:IMod<float>) =
        adaptive {
            //let! p = p
            let! v = view
            let! near = near
            let! size = size
            let! hfov = hfov
            let hfov_rad = Conversion.RadiansFromDegrees(hfov)
         
            let wz = Fun.Tan(hfov_rad / 2.0) * near * size
            let dist = V3d.Distance(p, v.Location)
      
            return ( wz / near ) * dist
        }

    let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
        let right = up.Cross forw
        let rotTrafo = 
            new Trafo3d(
                new M44d(
                    right.X, up.X, forw.X, 0.0,
                    right.Y, up.Y, forw.Y, 0.0,
                    right.Z, up.Z, forw.Z, 0.0,
                    0.0,     0.0,  0.0,    1.0
                ),
                new M44d(
                    right.X, right.Y, right.Z, 0.0,
                    up.X,    up.Y,    up.Z,    0.0,
                    forw.X,  forw.Y,  forw.Z,  0.0,
                    0.0,     0.0,     0.0,     1.0
                )
        )
        rotTrafo * modelt        

    let text (view : IMod<CameraView>) (near : IMod<float>) (hfov:IMod<float>) (pos:V3d) (modelTrafo:Trafo3d) (text:IMod<string>) (size:IMod<float>) =
        let invScaleTrafo = computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale
        //    //computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale //(Mod.constant 0.05)
      
        let billboardTrafo = 
            adaptive {
                let! v = view
                //let! size = size
                let! scaleTrafo = invScaleTrafo
                let modelt = scaleTrafo * modelTrafo
      
                return screenAligned v.Forward v.Up modelt
            }      
      
        Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
        |> Sg.noEvents
        |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
            do! DefaultSurfaces.stableTrafo
        }                  
        |> Sg.trafo billboardTrafo  

    [<AutoOpen>]
    module MissingInBase = 
        type ProcListBuilder with   
            member x.While(predicate : unit -> bool, body : ProcList<'m,unit>) : ProcList<'m,unit> =
                proclist {
                    let p = predicate()
                    if p then 
                        yield! body
                        yield! x.While(predicate,body)
                    else ()
                }

       

module List =
    let take' (n : int) (input : list<'a>) : list<'a> =
        if n >= input.Length then input else input |> List.take n
