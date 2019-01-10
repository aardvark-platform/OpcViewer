namespace OpcOutlineTest

open System
open System.IO
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Rendering.Text
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open FShade
open Aardvark.Base.Geometry
open Aardvark.Geometry
open ``F# Sg``

open OpcSelectionViewer
open OpcSelectionViewer.Picking

module GuiEx =
    let iconToggle (dings : IMod<bool>) onIcon offIcon action =
      let toggleIcon = dings |> Mod.map(fun isOn -> if isOn then onIcon else offIcon)

      let attributes = 
        amap {
            let! icon = toggleIcon
            yield clazz icon
            yield onClick (fun _ -> action)
        } |> AttributeMap.ofAMap

      Incremental.i attributes AList.empty

    let iconCheckBox (dings : IMod<bool>) action =
      iconToggle dings "check square outline icon" "square icon" action

module OutlineApp = 
  open SceneObjectHandling
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
                    
  let outlineView (m : MOutlineModel) : DomNode<OutlineMessage> =

      let opcs = 
        m.opcInfos
          |> AMap.toASet
          |> ASet.map(fun info -> SceneObjectHandling.createOutlineOpcSg m info)
          |> Sg.set

      let scene = [ opcs ] |> Sg.ofList

      let renderControl =
       FreeFlyController.controlledControl m.cameraState OutlineMessage.Camera (Frustum.perspective 60.0 0.01 1000.0 1.0 |> Mod.constant) 
         (AttributeMap.ofList [ 
           style "width: 100%; height:100%"; 
           attribute "showFPS" "false";       // optional, default is false
           attribute "useMapping" "true"
           attribute "data-renderalways" "false"
           attribute "data-samples" "4"
         ]) 
         (scene) 

      let outlineUI (model:MOutlineModel) =
          Html.table [
            Html.row "Outlines:"      [GuiEx.iconCheckBox model.useOutlines UseOutlines ]  
            Html.row "Thickness:"     [Numeric.view' [NumericInputType.Slider]   model.lineThickness  |> UI.map SetLineThickness ]
        ]
             
            
      let frustum = Frustum.perspective 60.0 0.1 50000.0 1.0 |> Mod.constant  

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
          |> List.fold (fun a b -> Box3d.Union(a, b)) Box3d.Invalid
      
      let opcInfos = 
        [
          for h in patchHierarchies do
            
            let rootTree = h.tree |> QTree.getRoot

            yield {
              patchHierarchy = h
              kdTree         = KdTrees.loadKdTrees' h Trafo3d.Identity true ViewerModality.XYZ Serialization.binarySerializer
              localBB        = rootTree.info.LocalBoundingBox 
              globalBB       = rootTree.info.GlobalBoundingBox
              globalBB2d     = rootTree.info.GlobalBoundingBox2d
            }
        ]
        |> List.map (fun info -> info.globalBB, info)
        |> HMap.ofList      
                      
      let camState = { FreeFlyController.initial with view = CameraView.lookAt (box.Center) V3d.OOO V3d.OOI; }

      let initialModel : OutlineModel = 
        { 
          cameraState        = camState          
          fillMode           = FillMode.Fill                    
          patchHierarchies   = patchHierarchies          
                    
          threads            = FreeFlyController.threads camState |> ThreadPool.map OutlineMessage.Camera
          boxes              = List.empty //kdTrees |> HMap.toList |> List.map fst
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
          unpersist = Unpersist.instance<OutlineModel, MOutlineModel>
      }
       