namespace PRo3D.Minerva

open System

open Aardvark.Base
open Aardvark.Rendering.Text 
open Aardvark.Rendering

open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open KdTreeHelper
open FSharp.Data.Adaptive

module MinervaApp =
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
    
  let take' (n : int) (input : list<'a>) : list<'a> =
       if n >= input.Length then input else input |> List.take n

  let updateSgFeatures (features:IndexList<Feature>) =
    
    let array = features |> IndexList.toArray
    
    let names     = array |> Array.map(fun f -> f.id)            
    let positions = array |> Array.map(fun f -> f.geometry.positions.Head)
    let coordinates = array |> Array.map(fun f -> f.geometry.coordinates.Head)    
    let colors    = array |> Array.map(fun f -> f.instrument |> MinervaModel.instrumentColor )
    let instruments = array |> Array.map(fun f -> f.instrument)
    
    let trafo =
      match positions |> Array.tryHead with
        | Some p -> Trafo3d.Translation p
        | _ -> Trafo3d.Identity
                     
    {
        names = names
        positions = positions
        colors = colors
        trafo = trafo
    }

  let updateSelectedSgFeature (features:IndexList<Feature>) (selected:HashSet<string>) =
    features
        |> IndexList.filter( fun x -> HashSet.contains x.id selected)
        |> updateSgFeatures
     
  let updateSelectionToggle (names:list<string>) (model: MinervaModel) =
    let newSelection = 
      List.fold(fun set name -> 
        if set |> HashSet.contains name then
          set |> HashSet.remove name
        else 
          set |> HashSet.add name) model.selection.selectedProducts names

    let selectedSgs = updateSelectedSgFeature model.filteredFeatures newSelection
    { model with selection = { model.selection with selectedProducts = newSelection}; selectedSgFeatures = selectedSgs}

  let updateFeaturesForRendering (pos: V3d) (model: MinervaModel) =
    Log.startTimed "[Minerva] building sgs"
    let sgFeatures = updateSgFeatures model.filteredFeatures
    Log.line "[Minerva] showing %d products" sgFeatures.positions.Length
    Log.stop()
    { model with sgFeatures = sgFeatures }

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
        | SendScreenSpaceCoordinates ->  failwith "[2D3DLiniking] not implemented"
          | PerformQueries -> failwith "[Minerva] not implemented"
          | UpdateSelection selectionIds ->
            let selection = selectionIds |> HashSet.ofList
            let selectedSgs = updateSelectedSgFeature model.filteredFeatures selection
            { model with selection = { model.selection with selectedProducts = selection}; selectedSgFeatures = selectedSgs} 
          | UpdateFiltering idList ->
            Log.line "[Minerva] filtering data to set of %d" idList.Length

            let filterSet = idList |> HashSet.ofList
            let filtered = model.data.features |> IndexList.filter(fun x -> x.id |> filterSet.Contains)
        
            let blarg = filtered |> IndexList.toList |> List.map (fun x -> sprintf "%A %A %A %A" x.id x.instrument x.sol x.geometry.positions.[0])

            System.IO.File.WriteAllLines(@".\minervaIds",blarg)

            { model with filteredFeatures = filtered } |> updateFeaturesForRendering view.Location

          | FlyToProduct _ -> model //handled in higher level app
          | OpenTif (access, id) -> 
            Files.loadTif access id |> ignore
            model
          | LoadTifs access ->
            Log.startTimed "[Minerva] Fetching all TIFs from data file"
            model.data.features.ForEach(fun f ->
                Files.loadTif access f.id
            ) |> ignore
            Log.stop()  
            model
          | Reset ->
            { model with filteredFeatures = model.data.features; queryFilter = Initial.queryFilter }   
          | LoadProducts (dumpFile, cacheFile) ->                            
            Log.startTimed "[Minerva] Fetching full dataset from data file"


            let data = Files.loadDataFile dumpFile cacheFile
            Log.stop()        

            Log.line "[Minerva] found %d entries" data.features.Count   
            let flatList = 
              data.features 
                |> IndexList.map(fun x -> x.geometry.positions |> List.head, x.id) 
                |> IndexList.toArray

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
            { model with selection = { model.selection with singleSelectProduct = None; selectedProducts = HashSet.empty}; selectedSgFeatures = updateSgFeatures IndexList.empty}
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
            | _ -> failwith "wrong action"
          
  let viewFeaturesSg (model : AdaptiveMinervaModel) =
    let pointSize = model.featureProperties.pointSize.value

    Sg.ofList [
      Drawing.featureMousePick model.kdTreeBounds
      Drawing.drawFeaturePoints model.sgFeatures pointSize
      Drawing.drawSelectedFeaturePoints model.selectedSgFeatures pointSize
      Drawing.drawHoveredFeaturePoint model.hoveredProduct pointSize model.sgFeatures.trafo
    ]
 