open System
open Aardvark.Base
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open Suave.WebPart

type EmbeddedRessource = EmbeddedRessource

[<EntryPoint; STAThread>]
let main argv = 
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    use app = new OpenGlApplication()
    //let opcDir = "C:\Users\laura\VRVis\Data\CapeDesire\Surface\Cape_Desire_RGB"
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
      | None -> failwith "need opc directory ... opc=\"[opcfilepath]\" "

    let axisFile = argsKv |> HMap.tryFind "axis"

    let rotate = argsList.Contains("-rotate")
    
    let instance =  OpcSelectionViewer.App.app opcDir axisFile rotate |> App.start 
    //let instance = OpcOutlineTest.OutlineApp.appOutlines opcDir |> App.start 

    // use can use whatever suave server to start you mutable app. 
    // startServerLocalhost is one of the convinience functions which sets up 
    // a server without much boilerplate.
    // there is also WebPart.startServer and WebPart.runServer. 
    // look at their implementation here: https://github.com/aardvark-platform/aardvark.media/blob/master/src/Aardvark.Service/Suave.fs#L10
    // if you are unhappy with them, you can always use your own server config.
    // the localhost variant does not require to allow the port through your firewall.
    // the non localhost variant runs in 127.0.0.1 which enables remote acces (e.g. via your mobile phone)
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

    //use ctrl = new AardvarkCefBrowser()
    //ctrl.Dock <- DockStyle.Fill
    //form.Controls.Add ctrl
    //ctrl.StartUrl <- "http://localhost:4321/"
    //ctrl.ShowDevTools()
    //form.Text <- "Examples"
    //form.Icon <- Icons.aardvark 

    //Application.Run form
    0 
