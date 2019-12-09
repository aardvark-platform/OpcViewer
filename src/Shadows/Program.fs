open System
open Aardvark.Base
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open Suave.WebPart
open OpcViewer.Base
open FShade
open Shadows

type EmbeddedRessource = EmbeddedRessource

[<EntryPoint; STAThread>]
let main argv = 


    let debug = false


    // let normal = g.IndexedAttributes.[DefaultSemantic.Normals] |> unbox<V3f[]>

    if debug then

      // original shader
      let c =
        Effect.compose [
          OpcViewer.Base.Shader.StableTrafo.Effect
          OpcViewer.Base.Shader.StableLight.Effect
          //Effect.ofFunction Shadows.TestShader.Coloring.shader
          //Effect.ofFunction Aardvark.Base.Rendering.DefaultSurfaces.diffuseTexture
        ]


      //let c =
      //  Effect.compose [
      //      Effect.ofFunction TestShader.Transformation.shader
      //      Effect.ofFunction TestShader.Coloring.shader 
      //  ]
      let shaders = [
          c
        ]


      Helpers.printShader c
      let x = System.Console.ReadLine()
      0
    else
      Ag.initialize()
      Aardvark.Init()
      Aardium.init()
      //cootrafo testing
      CooTransformation.initCooTrafo ()
    
      let pos = V3d(10000,1000,10000)
      let sc = CooTransformation.getLatLonAlt pos Planet.Mars
      Log.line "altitude: %f" sc.altitude

      CooTransformation.deInitCooTrafo()

      use app = new OpenGlApplication()

      let noOpcErrorMessage = 
        "ERROR: No opc path set. Set the path to your opc directory as a command line argument \
          to this program. Usage: \n 
          opc=[path] \n
          Use the path of the directory containing the OPC_000_000 directory." 
      //let opcDir = "C:\Users\laura\VRVis\Data\CapeDesire\Surface\Cape_Desire_RGB"

      if Array.isEmpty argv then
        failwith noOpcErrorMessage
      let opcDir = argv.[0];
      let axisFile = None //if argv.Length > 1 then Some(argv.[1]) else None

      let argsList = List.fold(fun (x:string) (y : string)-> x + " " + y) String.Empty (argv |> Array.toList)

      let argsKv = 
        argv 
          |> Array.filter(fun x -> x.Contains "=")
          |> Array.map(fun x -> 
                let kv = x.Split [|'='|]
                kv.[0],kv.[1])
          |> HMap.ofArray

      let opcDir =
        match argsKv |> HMap.tryFind "opc" with
        | Some dir -> dir
        | None -> failwith noOpcErrorMessage

      let axisFile = argsKv |> HMap.tryFind "axis"

      let rotate = argsList.Contains("-rotate")

      let debug = true
    
      let instance =  Shadows.App.app debug app opcDir axisFile rotate |> App.start 
    
      WebPart.startServerLocalhost 4321 [ 
          MutableApp.toWebPart' app.Runtime false instance
          Reflection.assemblyWebPart typeof<EmbeddedRessource>.Assembly
          Reflection.assemblyWebPart typeof<Aardvark.UI.Primitives.EmbeddedResources>.Assembly
          Suave.Files.browseHome
      ] |> ignore

      Aardium.run {
          url "http://localhost:4321/"
          width 1024
          height 768
          debug true
      }

      0