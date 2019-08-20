
open System
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open PRo3D.Minerva
open PRo3D.Base

open LinkingView

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
        |> HMap.ofArray

    let opcDir =
      match argsKv |> HMap.tryFind "opc" with
      | Some dir -> dir
      | None -> failwith "need opc directory ... opc=\"[opcfilepath]\" "

    Serialization.init()
    // loading dump file, later replace with database connection
    let dumpFile =
        match argsKv |> HMap.tryFind "dump" with
        | Some file -> file
        | None -> failwith "need dump file ... dump=\"[dumpfilepath]\" "

    let cacheFile =
        match argsKv |> HMap.tryFind "cache" with
        | Some file -> file
        | None -> failwith "need cache file ... cache=\"[cachefilepath]\" "

    let access =
        match argsKv |> HMap.tryFind "access" with
        | Some file -> file
        | None -> failwith "need minerva access ... access=\"minervaaccount:pw\" "

    ////let dumpData = Files.loadDataFile dumpFile cacheFile
    ////Log.line "%A" dumpData

    let rotate = argsList.Contains("-rotate")
    
    let instance =  LinkingView.App.app opcDir rotate dumpFile cacheFile access |> App.start 

    WebPart.startServerLocalhost 4321 [ 
        MutableApp.toWebPart' app.Runtime false instance
        Suave.Files.browseHome
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321/"
        width 1024
        height 768
        debug true
    }






    0 
