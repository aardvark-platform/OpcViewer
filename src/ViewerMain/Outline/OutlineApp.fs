namespace OpcOutlineTest

open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Data.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open FShade
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcSelectionViewer
open OpcViewer.Base.KdTrees

module GuiEx =
    let iconToggle (dings : aval<bool>) onIcon offIcon action =
      let toggleIcon = dings |> AVal.map(fun isOn -> if isOn then onIcon else offIcon)

      let attributes = 
        amap {
            let! icon = toggleIcon
            yield clazz icon
            yield onClick (fun _ -> action)
        } |> AttributeMap.ofAMap

      Incremental.i attributes AList.empty

    let iconCheckBox (dings : aval<bool>) action =
      iconToggle dings "check square outline icon" "square icon" action

module OutlineApp =   
  open Aardvark.Application
  open Aardvark.Base.DynamicLinkerTypes  
  
  let update (model : OutlineModel) (msg : OutlineMessage) =   
    match msg with
      | OutlineMessage.Camera m -> 
        { model with cameraState = FreeFlyController.update model.cameraState m; }
      | OutlineMessage.UpdateDockConfig cfg ->
        { model with dockConfig = cfg }
      | SetLineThickness th ->
        { model with lineThickness = Numeric.update model.lineThickness th }
      | UseOutlines ->
        { model with useOutlines = not model.useOutlines }      
                    
  let outlineView (m : AdaptiveOutlineModel) : DomNode<OutlineMessage> =

      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> OutlineSg.createOutlineOpcSg m info)
          |> Sg.set

      let scene = [ opcs ] |> Sg.ofList

      let renderControl =
       FreeFlyController.controlledControl m.cameraState OutlineMessage.Camera (Frustum.perspective 60.0 0.01 1000.0 1.0 |> AVal.constant) 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "false";       // optional, default is false
           attribute "useMapping" "true"
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
         ]) 
         (scene) 

      let outlineUI (model:AdaptiveOutlineModel) =
          Html.table [
            Html.row "Outlines:"      [GuiEx.iconCheckBox model.useOutlines UseOutlines ]  
            Html.row "Thickness:"     [Numeric.view' [NumericInputType.Slider]   model.lineThickness  |> UI.map SetLineThickness ]
        ]
                         
      page (fun request -> 
        match Map.tryFind "page" request.queryParams with
        | Some "render" ->
          require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
              div [clazz "ui"; style "background: #1B1C1E"] [renderControl]
          )
        | Some "controls" -> 
          require Html.semui (
            body [style "width: 100%; height:100%; background: transparent";] [
               div[style "color:white"][outlineUI m]
            ]
          )
        | Some other -> 
          let msg = sprintf "Unknown page: %A" other
          body [] [
              div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
          ]  
        | None -> 
          m.dockConfig
            |> docking [
              style "width:100%; height:100%; background:#F00"
              onLayoutChanged OutlineMessage.UpdateDockConfig ]
        )

  let appOutlines dir =
      Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)

      let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton

      let patchHierarchies =
        [ 
          for h in phDirs do
            yield PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle (h |> OpcPaths)
        ]    

      let box = 
        patchHierarchies 
          |> List.map(fun x -> x.tree |> QTree.getRoot) 
          |> List.map(fun x -> x.info.GlobalBoundingBox)
          |> List.fold (fun a b -> Box3d(a, b)) Box3d.Invalid
      
      let opcInfos = 
        [
          for h in patchHierarchies do
            
            let rootTree = h.tree |> QTree.getRoot
            let kd = KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ Serialization.binarySerializer false false (fun _ -> failwith "no function for creating triangle sets") false false KdTreeParameters.legacyDefault

            yield {
              patchHierarchy = h
              kdTree         = kd
              localBB        = rootTree.info.LocalBoundingBox 
              globalBB       = rootTree.info.GlobalBoundingBox
              neighborMap    = HashMap.empty
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HashMap.ofList      
                      
      let camState = { FreeFlyController.initial with view = CameraView.lookAt (box.Center) V3d.OOO V3d.OOI; }

      let initialModel : OutlineModel = 
        { 
          cameraState        = camState          
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies          
                    
          threads            = FreeFlyController.threads camState |> ThreadPool.map OutlineMessage.Camera
          boxes              = List.empty //kdTrees |> HashMap.toList |> List.map fst
          opcInfos           = opcInfos
          dockConfig         =
            config {
                content (
                    horizontal 10.0 [
                        element { id "render"; title "Render View"; weight 7.0 }                        
                        element { id "controls"; title "Controls"; weight 3.0 }                    ]
                )
                appName "OpcOutlineTest"
                useCachedConfig true
            }
          lineThickness =
                    {
                        value   = 3.0
                        min     = 1.0
                        max     = 8.0
                        step    = 1.0
                        format  = "{0:0}"
                    }
          useOutlines = true
        }

      {
          initial = initialModel             
          update = update
          view   =  outlineView          
          threads = fun m -> m.threads
          unpersist = Unpersist.instance<OutlineModel, AdaptiveOutlineModel>
      }
       