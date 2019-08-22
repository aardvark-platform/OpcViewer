namespace PRo3D.Minerva

open System

//open Aardvark.GeoSpatial.Opc
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
//open Aardvark.Application.Utilities
open FShade
open OpcViewer.Base
open Aardvark.UI
open FSharp.Data

module Files = 

    let getProperties (ins : Instrument) (insId:string) (row:CsvRow) : PRo3D.Minerva.Properties = 
        match ins with
            | Instrument.MAHLI ->   
                {
                MAHLI_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.MAHLI
            | Instrument.FrontHazcam | Instrument.FrontHazcamL | Instrument.FrontHazcamR ->
                {
                FrontHazcam_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.FrontHazcam
            | Instrument.Mastcam | Instrument.MastcamL | Instrument.MastcamR ->
                {
                Mastcam_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.Mastcam
            | Instrument.APXS ->
              {
                APXS_Properties.id = insId |> FeatureId
              } |> Properties.APXS       
            | Instrument.ChemLib ->
              {
                ChemCam_Properties.id = insId |> FeatureId           
              } |> Properties.ChemCam       
            | Instrument.ChemRmi ->
              {
                ChemCam_Properties.id = insId |> FeatureId            
              } |> Properties.ChemCam       
            | Instrument.NotImplemented ->
              {
                APXS_Properties.id = insId |> FeatureId
              } |> Properties.APXS               
            | _ -> failwith "encountered invalid instrument from parsing"

    let getFeature (row:CsvRow) : option<Feature> =

        let id' = row.GetColumn "{Key}Product_id"
        match id' with
         | "" -> None
         | _-> 
            let inst = row.GetColumn "{Category}Instrument_id"
            let instrument = inst |> MinervaModel.toInstrument
            let sol' = (row.GetColumn "{Value}{Sol}Planet_day_number").AsInteger()

            let omega = (row.GetColumn "{Angle}Omega").AsFloat()
            let phi = (row.GetColumn "{Angle}Phi").AsFloat()
            let kappa = (row.GetColumn "{Angle}Kappa").AsFloat()

            let x = (row.GetColumn "{CartX}X").AsFloat()
            let y = (row.GetColumn "{CartY}Y").AsFloat()
            let z = (row.GetColumn "{CartZ}Z").AsFloat()

            let tryInt (name: string) =
                try Some ((row.GetColumn name).AsInteger())
                with _ -> None

            let w = (tryInt "{Value}Image_width") |> Option.defaultValue 0
            let h = (tryInt "{Value}Image_height") |> Option.defaultValue 0

            let instName = row.GetColumn "{Category}Instrument_name"

            let props = getProperties instrument inst row

            let geo = 
                {
                    typus = Typus.Point
                    coordinates = V3d(omega, phi, kappa) |> List.singleton
                    positions = V3d(x, y, z) |> List.singleton
                }

            let feature = 
                {
                  id          = id'
                  instrument  = instrument
                  typus       = Feature
                  boundingBox = Box2d.Invalid//feature?bbox |> parseBoundingBox
                  properties  = props
                  geometry    = geo
                  sol         = sol'
                  dimensions  = (w, h)
                } 
            Some feature

    open System.IO   
    open PRo3D.Base

    let loadDataFile dumpFile cacheFile=
        let cachePath = cacheFile
        //let cachePath = @".\MinervaData\dump.cache"
        let path = dumpFile
        //let path = @".\MinervaData\dump.csv"
        match (File.Exists path, File.Exists cachePath) with
         | (true, false) -> 
            let allData = CsvFile.Load(path).Cache()

            let features = 
                allData.Rows
                    |> Seq.choose( fun x -> getFeature x )
                
            {
              name        = "dump"
              typus       = Typus.FeatureCollection    
              boundingBox = Box2d.Invalid
              features    = features |> PList.ofSeq
            } |> Serialization.save cachePath
         | (_, true) -> Serialization.loadAs cachePath
         | _ -> 
            Log.error "[Minerva] sth. went wrong with dump.csv"
            Initial.data        

    let loadTif (access: string) (featureId: string) =
        // https://minerva.eox.at/store/datafile/FRB_495137799RADLF0492626FHAZ00323M1/frb_495137799radlf0492626fhaz00323m1.tif
        let filename = featureId.ToLower() + ".tif" //"frb_495137799radlf0492626fhaz00323m1.tif"
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
            with e -> Log.line "[Minerva] error: %A" e
            
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
        Mod.map2(fun (x:Option<V3d>) (t:Trafo3d) ->  
          match x with 
          | None -> [||]
          | Some a ->  [|t.Backward.TransformPos(a)|]) hoveredProduct trafo 

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
