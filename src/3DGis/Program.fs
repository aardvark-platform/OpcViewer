open System
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open OpcViewer.Base

type EmbeddedRessource = EmbeddedRessource

[<EntryPoint; STAThread>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    use app = new OpenGlApplication()
    CooTransformation.initCooTrafo ()
    let argsList = List.fold(fun (x:string) (y : string)-> x + " " + y) String.Empty (argv |> Array.toList)

    let argsKv = 
      argv 
        |> Array.filter(fun x -> x.Contains "=")
        |> Array.map(fun x -> 
              let kv = x.Split [|'='|]
              kv.[0],kv.[1])
        |> HashMap.ofArray

    let opcDir =
      match argsKv |> HashMap.tryFind "opc" with
      | Some dir -> dir
      | None -> failwith "need opc directory ... opc=\"[opcfilepath]\" "

    let axisFile = argsKv |> HashMap.tryFind "axis"

    let rotate = argsList.Contains("-rotate")
    
    let instance =  ElevationProfileViewer.App.app opcDir axisFile rotate |> App.start 

    let resourcesFolder = IO.Directory.GetCurrentDirectory()

    WebPart.startServerLocalhost 4321 [
        Suave.Files.browse resourcesFolder
        MutableApp.toWebPart' app.Runtime false instance
        Reflection.assemblyWebPart typeof<EmbeddedRessource>.Assembly
        Reflection.assemblyWebPart typeof<Aardvark.UI.Primitives.EmbeddedResources>.Assembly
        Suave.Files.browseHome
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321/"
        width 1536
        height 1152
        debug true
    }

    0 // return an integer exit code
