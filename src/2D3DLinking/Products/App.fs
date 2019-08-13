namespace PRo3D.Minerva

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 

open PRo3D.Base

open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open KdTreeHelper

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
        
module MinervaApp =
  open System.Net.Sockets
  open Aardvark.SceneGraph.Opc
  open System.Threading
  open Aardvark.Geometry
  
  let instrumentText (instr : Instrument) =
    match instr with
    | Instrument.MAHLI        -> "MAHLI"
    | Instrument.FrontHazcam  -> "FrontHazcam"
    | Instrument.Mastcam      -> "Mastcam"
    | Instrument.APXS         -> "APXS"
    | Instrument.FrontHazcamR -> "FrontHazcamR"
    | Instrument.FrontHazcamL -> "FrontHazcamL"
    | Instrument.MastcamR     -> "MastcamR"    
    | Instrument.MastcamL     -> "MastcamL"
    | Instrument.ChemLib      -> "ChemLib"    
    | Instrument.ChemRmi      -> "ChemRmi"
    | Instrument.NotImplemented -> "not impl. yet"
    | _ -> instr |> sprintf "unknown instrument identifier %A" |> failwith

  let writeLinesToFile path (contents : list<string>) =
        System.IO.File.WriteAllLines(path, contents)

  let sendMessage2Visplore (address) (port) (message : string) =
    let client = new TcpClient(address, port);
    let data = System.Text.Encoding.ASCII.GetBytes(message)
    let stream = client.GetStream()
    stream.Write(data, 0, data.Length)
    
    printfn "Sending message: %A" message
        
    stream.Close()
    client.Close()
  
  let idTestList = [ "271092ML0047930090500286E01_DRCX"; "1092MR0047900090600628I01_DRCX"]
    
  let take' (n : int) (input : list<'a>) : list<'a> =
       if n >= input.Length then input else input |> List.take n

  let updateSolLabels (features:plist<Feature>) (position : V3d) = 
    features 
      |> PList.toList      
      |> List.map(fun x -> x.sol |> string, x.geometry.positions.Head) 
      |> List.sortBy(fun (_,p) -> V3d.DistanceSquared(position, p))
      |> take' 50
      |> HMap.ofList

  let updateSgFeatures (features:plist<Feature>) =
    
    let array = features |> PList.toArray
    
    let names     = array |> Array.map(fun f -> f.id)            
    let positions = array |> Array.map(fun f -> f.geometry.positions.Head)
    let coordinates = array |> Array.map(fun f -> f.geometry.coordinates.Head)    
    let colors    = array |> Array.map(fun f -> f.instrument |> MinervaModel.instrumentColor )
    
    let trafo =
      match positions |> Array.tryHead with
        | Some p -> Trafo3d.Translation p
        | _ -> Trafo3d.Identity
                     
    {
        names = names
        positions = positions
        coordinates = coordinates
        colors = colors
        trafo = trafo
    }

  let updateSelectedSgFeature (features:plist<Feature>) (selected:hset<string>) =
    features
        |> PList.filter( fun x -> HSet.contains x.id selected)
        |> updateSgFeatures
     
  let updateSelectionToggle (names:list<string>) (model: MinervaModel) =
    let newSelection = 
      List.fold(fun set name -> 
        if set |> HSet.contains name then
          set |> HSet.remove name
        else 
          set |> HSet.add name) model.selection.selectedProducts names

    let selectedSgs = updateSelectedSgFeature model.filteredFeatures newSelection
    { model with selection = { model.selection with selectedProducts = newSelection}; selectedSgFeatures = selectedSgs}

  let updateFeaturesForRendering (pos: V3d) (model: MinervaModel) =
    Log.startTimed "[Minerva] building sgs"
    let solLabels = updateSolLabels model.filteredFeatures pos //view frustum culling AND distance culling
    let sgFeatures = updateSgFeatures model.filteredFeatures
    Log.line "[Minerva] showing %d labels and %d products" solLabels.Count sgFeatures.positions.Length
    Log.stop()
    { model with solLabels = solLabels; sgFeatures = sgFeatures }

  let queryClosestPoint model hit = 
    //Report.BeginTimed("kdTreeQuery") |> ignore
    let viewProj = hit.event.evtView * hit.event.evtProj
    let viewPort = V2d hit.event.evtViewport
    let size = 5.0 * 2.0 / viewPort
    let centerNDC = 
      let t = V2d hit.event.evtPixel / viewPort
      V2d(2.0 * t.X - 1.0, 1.0 - 2.0 * t.Y)

    let ellipse = Ellipse2d(centerNDC, V2d.IO * size, V2d.OI * size)
    let closestPoints = KdTreeQuery.FindPoints(model.selection.kdTree, model.kdTreeBounds, model.selection.flatPos, viewProj, ellipse)
    //Report.EndTimed() |> ignore
    closestPoints

  let update (view:CameraView) (model : MinervaModel) (msg : MinervaAction) : MinervaModel =
      match msg with     
        | SendScreenSpaceCoordinates -> 
            //let viewProj = view.ViewTrafo * Frustum.projTrafo frustum
            //let coords = 
            //  model.filteredFeatures
            //    |> PList.toList 
            //    |> List.map(fun feat -> (feat.id, feat.geometry.positions.Head))
            //    |> List.map(fun (id,p) ->  
            //      let coord = (viewProj.Forward.TransformPosProj p)
            //      let coord = (V2d(coord.X, coord.Y) + V2d.One) * 0.5
            //      (id, coord))
        
            //let width = 1920
            //let height = ((float)width / (frustum |> Frustum.aspect)) |> int

            //let docPath = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
            //let imagePathTmp = @"visplore\Minerva"
            //let imagePath = Path.combine[docPath; imagePathTmp]
            //let filename = "overview.jpg"

            //Log.startTimed "[Minerva taking] Screenshot %A" (V2i(width, height))
            //PRo3D.Base.Utilities.takeScreenshot "http://localhost:54321" width height filename imagePath
            //Log.stop()
      
            model
          | PerformQueries -> failwith "[Minerva] not implemented"
            //try
            //  //let data = model.queries |> MinervaGeoJSON.loadMultiple        
            //  Log.startTimed "[Minerva] Fetching full dataset from Server"
            //  let data = idTestList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
            //  //let features = data.features |> PList.sortBy(fun x -> x.sol)    
            //  Log.stop()

            //  let queryM = QueryApp.updateFeaturesForRendering model.queryFilter data.features
            //  { model with data = data; queryFilter = queryM }

            //with e ->
            //  Log.error "%A" e.Message
            //  model
          | UpdateSelection selectionIds ->
            let selection = selectionIds |> HSet.ofList
            let selectedSgs = updateSelectedSgFeature model.filteredFeatures selection
            { model with selection = { model.selection with selectedProducts = selection}; selectedSgFeatures = selectedSgs} 
          | UpdateFiltering idList -> //failwith "[Minerva] not implemented"
            //Log.startTimed "fetching %d entries by ids" idList.Length
            //let data = idList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
            //Log.stop()

        
            Log.line "[Minerva] filtering data to set of %d" idList.Length

            let filterSet = idList |> HSet.ofList
            let filtered = model.data.features |> PList.filter(fun x -> x.id |> filterSet.Contains)
        
            let blarg = filtered |> PList.toList |> List.map (fun x -> sprintf "%A %A %A %A" x.id x.instrument x.sol x.geometry.positions.[0])

            System.IO.File.WriteAllLines(@".\minervaIds",blarg)

            { model with filteredFeatures = filtered } |> updateFeaturesForRendering view.Location

          | FlyToProduct _ -> model //handled in higher level app
          | OpenTif id -> Files.loadTif id model     
          | Reset ->
            { model with filteredFeatures = model.data.features; queryFilter = Initial.queryFilter }   
          | LoadProducts (dumpFile, cacheFile) ->                            
            Log.startTimed "[Minerva] Fetching full dataset from data file"


            let data = Files.loadDataFile dumpFile cacheFile
            Log.stop()        

            Log.line "[Minerva] found %d entries" data.features.Count   
            let flatList = 
              data.features 
                |> PList.map(fun x -> x.geometry.positions |> List.head, x.id) 
                |> PList.toArray

            let input = flatList |> Array.map fst
            let flatId = flatList |> Array.map snd
            let kdTree = PointKdTreeExtensions.CreateKdTree(input, Metric.Euclidean, 1e-5)
            let kdTreeBounds = Box3d(input)
            { 
              model with
                data = data
                filteredFeatures = data.features
                kdTreeBounds = kdTreeBounds
                selection = 
                  { model.selection with
                      kdTree = kdTree
                      flatPos = input
                      flatID = flatId
                  }
            } |> updateFeaturesForRendering view.Location
          | SetPointSize s ->
            let size = Numeric.update model.featureProperties.pointSize s
            { model with featureProperties = { model.featureProperties with pointSize = size }}
          | SetTextSize s ->
            let size = Numeric.update model.featureProperties.textSize s
            { model with featureProperties = { model.featureProperties with textSize = size }}   
          | SingleSelectProduct name ->
            { model with selection = { model.selection with singleSelectProduct = Some name }}
          | ClearSelection ->
            { model with selection = { model.selection with singleSelectProduct = None; selectedProducts = HSet.empty}; selectedSgFeatures = updateSgFeatures PList.empty}
          | AddProductToSelection name ->
            updateSelectionToggle [name] model
          | PickProducts hit -> 
              let closestPoints = queryClosestPoint model hit
              match closestPoints with
              | emptySeq when Seq.isEmpty emptySeq -> model
              | seq -> 
                let index = seq |> Seq.map (fun (depth, pos, index) -> index) |> Seq.head
                let closestID = model.selection.flatID.[index]
                updateSelectionToggle [closestID] model
            
              //Report.BeginTimed("bruteforce") |> ignore
              //let updateModel = 
              //  let ray = hit.globalRay.Ray.Ray
              //  let minDist = model.selection.selectionMinDist
              //  Array.zip model.selection.flatPos model.selection.flatID
              //    |> Seq.choose(fun (pos, id) -> 
              //      let dist = ray.GetMinimalDistanceTo pos
              //      if dist < minDist then
              //        Some (dist, id)
              //      else 
              //        None)
              //    |> fun withinRange -> 
              //      if withinRange.IsEmpty() then
              //        model
              //      else
              //        let closestID = 
              //          withinRange
              //            |> Seq.sortBy fst
              //            |> Seq.map snd
              //            |> Seq.head
              //        updateSelectionToggle [closestID] model
              //Report.EndTimed() |> ignore
              //updateModel

          | HoverProducts hit ->
              //Report.BeginTimed("hover-update") |> ignore
          
              let closestPoints = queryClosestPoint model hit

              let updateModel = 
                match closestPoints with
                | emptySeq when Seq.isEmpty emptySeq -> 
                  match model.hoveredProduct with
                  | None -> model
                  | Some _ -> { model with hoveredProduct = None}
                | seq -> 
                  let depth, pos, index = seq |> Seq.head
                  { model with hoveredProduct = Some pos }
             // Report.EndTimed() |> ignore

              updateModel

  let viewFeaturesGui (model: MMinervaModel) =
     
    let viewFeatures (instr : Instrument) (features : list<Feature>) = 

      let selectionColor (model: MMinervaModel) (feature : Feature) =
        model.selection.selectedProducts
          |> ASet.map(fun x -> x = feature.id)
          |> ASet.contains true
          |> Mod.map (function x -> if x then C4b.VRVisGreen else C4b.White)

      let ac = sprintf "color: %s" (Html.ofC4b C4b.White)

      features |> List.map(fun f -> 
        let headerAttributes =
            amap {
                //yield onClick (fun _ -> SingleSelectProduct f.id)
                yield  onClick(fun _ -> AddProductToSelection f.id)
            } |> AttributeMap.ofAMap

        //let iconAttr = 
        //  [
        //    clazz "ui map pin inverted middle aligned icon"; 
        //    style (sprintf "color: %s" (f.instrument |> Model.instrumentColor |> Html.ofC4b))
        //    onClick(fun _ -> AddProductToSelection f.id)
        //    //onClick(fun _ -> FlyToProduct f.geometry.positions.Head)
        //  ]  
          
        let iconAttributes =
          amap {                  
            yield clazz "ui map pin inverted middle aligned icon"
            //yield  onClick(fun _ -> AddProductToSelection f.id)

            yield style (sprintf "color: %s" (Html.ofC4b (f.instrument |> MinervaModel.instrumentColor)))
          } |> AttributeMap.ofAMap

        let headerAttr : AttributeMap<_> = [clazz "ui header small"] |> AttributeMap.ofList
                                                
        div [clazz "ui inverted item"][
          Incremental.i iconAttributes AList.empty //i iconAttributes []
          div [clazz "ui content"] [
            Incremental.div (AttributeMap.ofList [style ac]) (
              alist {
                let! hc = selectionColor model f
                let c = hc |> Html.ofC4b |> sprintf "color: %s"
                yield div[clazz "header"; style c][
                      Incremental.div (headerAttributes) (AList.single (instr |> instrumentText |> text))
                ]
                yield div [clazz "ui description"] [
                        f.sol |> sprintf "Sol: %A" |> text
                        i [clazz "binoculars icon"; onClick (fun _ -> FlyToProduct f.geometry.positions.Head)][]
                        i [clazz "download icon"; onClick (fun _ -> OpenTif f.id)][]
                        ]
                //yield i [clazz "binoculars icon"; onClick (fun _ -> FlyToProduct f.geometry.positions.Head)][] //|> UI.wrapToolTip "FlyTo"       
                         
              } )
            ]            
        ])

    let accordion text' icon active content' =
       let title = if active then "title active inverted" else "title inverted"
       let content = if active then "content active" else "content"     
                             
       onBoot "$('#__ID__').accordion();" (
           div [clazz "ui inverted segment"] [
               div [clazz "ui inverted accordion fluid"] [
                   div [clazz title; style "background-color: #282828"] [
                           i [clazz ("dropdown icon")][]
                           text text'                                
                           div[style "float:right"][i [clazz (icon + " icon")][]]     
                   ]
                   div [clazz content;  style "overflow-y : auto; "] content' //max-height: 35%
               ]
           ]
       )
     
    let propertiesGui =
        require Html.semui ( 
            Html.table [ 
                Html.row "point size:" [Numeric.view' [NumericInputType.Slider] model.featureProperties.pointSize |> UI.map (fun x -> SetPointSize x)] 
                Html.row "text size:" [Numeric.view' [NumericInputType.InputBox] model.featureProperties.textSize |> UI.map (fun x -> SetTextSize x)] 
            ]        
        )      
       
    let listOfFeatures =
        alist {           
          let! features = model.filteredFeatures |> AList.toMod
          let groupedFeatures = features |> PList.toList |> List.groupBy(fun x -> x.instrument)
          let! pos = model.queryFilter.filterLocation

          for (instr, group) in groupedFeatures do

            let header = sprintf "%s (%d)" (instr |> instrumentText) group.Length
            let g = 
              group 
                |> List.sortBy(fun x -> V3d.DistanceSquared(pos, x.geometry.coordinates.Head)) 
                |> take'(20)

            yield div [clazz "ui inverted item"][
                yield accordion header "Content" false [                    
                  div [clazz "ui list"] (viewFeatures instr g)
                ]
            ]
        }

    div [clazz "ui inverted segment"] [
       h4 [clazz "ui"] [text "Comm:"]
       div [clazz "ui buttons"] [
         button [clazz "ui button"; onClick (fun _ -> ConnectVisplore)][text "Connect"]
         button [clazz "ui button"; onClick (fun _ -> SendSelection)][text "Send Selection"]
         button [clazz "ui button"; onClick (fun _ -> SendScreenSpaceCoordinates)][text "Send Coords"]
         //button [
         //  clazz "ui button"; 
         //  onEvent "onGetRenderId" [] (fun args -> Reset)
         //  clientEvent "onclick" "aardvark.processEvent(__ID__,'onGetRenderId', document.getElementsByClassName('mainrendercontrol')[0].id)"
         //] [text "SCREAM SHOT"]
       ]

       h4 [clazz "ui"] [text "Data:"]
       div [clazz "ui buttons"] [         
         //button [clazz "ui button"; onClick (fun _ -> LoadProducts)][text "Load"]
         button [clazz "ui button"; onClick (fun _ -> Reset)][text "Reset"]         
         button [clazz "ui button"; onClick (fun _ -> ClearSelection)][text "Clear Selection"]         
         button [clazz "ui button"; onClick (fun _ -> ApplyFilters)][text "Filter"]         
       ]
       
       propertiesGui          
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) listOfFeatures //AList.empty  
     ]

  let viewWrapped pos (model : MMinervaModel) =
    require Html.semui (
      body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
        div [clazz "ui inverted segment"] [viewFeaturesGui model]
      ])

  // SG  
  let getSolBillboards (model : MMinervaModel) (view:IMod<CameraView>) (near:IMod<float>) =
    let lables =
      model.solLabels
        |> AMap.map(fun txt pos ->
           Drawing.text view near (Mod.constant 60.0) pos (Trafo3d.Translation(pos)) (Mod.constant txt) (model.featureProperties.textSize.value)
           ) 
        |> AMap.toASet  
        |> ASet.map(fun x -> snd x)
        |> Sg.set
    lables
          
  /// Why not save sg directly in model, or build in view?
  let viewFeaturesSg (model : MMinervaModel) =
    let pointSize = model.featureProperties.pointSize.value

    Sg.ofList [
      Drawing.featureMousePick model.kdTreeBounds
      Drawing.drawFeaturePoints model.sgFeatures pointSize
      Drawing.drawFeatureDirections model.sgFeatures
      Drawing.drawSelectedFeaturePoints model.selectedSgFeatures pointSize
      Drawing.drawHoveredFeaturePoint model.hoveredProduct pointSize model.sgFeatures.trafo
    ]
  
  let threads (m : MinervaModel) = m.vplMessages
 
  //let start =
    
    //App.start {
    //    unpersist = Unpersist.instance
    //    threads   = fun m -> m.vplMessages
    //    view      = viewWrapped (Mod.constant V3d.Zero) //localhost
    //    update    = update (CameraView.lookAt V3d.Zero V3d.One V3d.OOI) (Frustum.perspective 90.0 0.001 100000.0 1.0)
    //    initial   = Initial.model
    //}