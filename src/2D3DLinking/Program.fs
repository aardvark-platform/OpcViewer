
open System
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open PRo3D.Minerva
open PRo3D.Base

open LinkingView

open FSharp.Data.Adaptive

type EmbeddedRessource = EmbeddedRessource

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    use app = new OpenGlApplication()

    let argsList = List.fold(fun (x: string) (y: string)-> x + " " + y) String.Empty (argv |> Array.toList)

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

    Serialization.init()
    // loading dump file, later replace with database connection
    let dumpFile =
        match argsKv |> HashMap.tryFind "dump" with
        | Some file -> file
        | None -> failwith "need dump file ... dump=\"[dumpfilepath]\" "

    let cacheFile =
        match argsKv |> HashMap.tryFind "cache" with
        | Some file -> file
        | None -> failwith "need cache file ... cache=\"[cachefilepath]\" "

    let access =
        match argsKv |> HashMap.tryFind "access" with
        | Some file -> file
        | None -> failwith "need minerva access ... access=\"minervaaccount:pw\" "

    ////let dumpData = Files.loadDataFile dumpFile cacheFile
    ////Log.line "%A" dumpData

    let rotate = argsList.Contains("-rotate")
    
    let instance =  LinkingView.App.app opcDir rotate dumpFile cacheFile access |> App.start 

    Log.line ">>>> %s" (System.IO.Path.GetFullPath("./resources"))

    //let resourcesFolder = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "resources") 
    let resourcesFolder = IO.Directory.GetCurrentDirectory()

    let webPart = choose [
        Suave.Files.browse resourcesFolder
        //Suave.Files.browseHome
        MutableApp.toWebPart' app.Runtime false instance
    ]

    //let bindings = [ HttpBinding.createSimple Protocol.HTTP "127.0.0.1" 4321 ]
    //let config = { Web.defaultConfig with bindings = bindings}

    //tartWebServer config webPart |> ignore

    //Aardium.run {
    //    url "http://127.0.0.1:4321/"
    //    width 1024
    //    height 768
    //    debug true
    //}

    // let config = { Suave.Web.defaultConfig with homeFolder = Some @"blablabla"
    WebPart.startServerLocalhost 4321 [ 
        webPart
        
        //Reflection.assemblyWebPart typeof<EmbeddedRessource>.Assembly
        //Reflection.assemblyWebPart typeof<Aardvark.UI.Primitives.EmbeddedResources>.Assembly
        
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321/"
        width 1024
        height 768
        debug true
    }






    0 
