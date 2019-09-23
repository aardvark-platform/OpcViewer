
open System
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    use app = new OpenGlApplication()

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
      | None -> failwith "need opc directory ... opc=\"[opcfilepath]\" "


    let rotate = argsList.Contains("-rotate")
    
    let instance =  ViewPlanner.App.app opcDir rotate |> App.start 

    WebPart.startServerLocalhost 4321 [ 
        MutableApp.toWebPart' app.Runtime false instance
        Suave.Files.browseHome
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321/"
        width 1500
        height 1000
        debug true
    }






    0 
