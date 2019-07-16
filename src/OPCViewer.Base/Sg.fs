namespace OpcViewer.Base

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open FShade
open Aardvark.UI.``F# Sg``
open Aardvark.UI.Trafos
open Aardvark.SceneGraph.Opc

//open OpcSelectionViewer.Picking
//open OpcOutlineTest

module Sg = 
  open Aardvark.UI
  open OpcViewer.Base.Picking

  //open Aardvark.Physics.Sky
  let transparent = RenderPass.after "transparent" RenderPassOrder.BackToFront RenderPass.main 

  let font = Font("Consolas")
  let border = { left = 0.01; right = 0.01; top = 0.01; bottom = 0.01 }
  
  let pickable' (pick :IMod<Pickable>) (sg: ISg) =
    Sg.PickableApplicator (pick, Mod.constant sg)

  let opcSg loadedHierarchies (picking : IMod<bool>) (bb : Box3d) = 
    
    let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
    let sg = 
      loadedHierarchies
        |> List.map (fun (g,dir,info) ->         
          let texPath = Patch.extractTexturePath (OpcPaths dir) info 0
          let tex = FileTexture(texPath,config) :> ITexture
                    
          Sg.ofIndexedGeometry g
              |> Sg.trafo (Mod.constant info.Local2Global)
              |> Sg.diffuseTexture (Mod.constant tex)       
              //|> Sg.andAlso(Sg.wireBox (Mod.constant C4b.VRVisGreen) (Mod.constant info.GlobalBoundingBox) |> Sg.noEvents)
          )
        |> Sg.ofList   
    
    let pickable = 
      adaptive {
       // let! bb = opcData.globalBB
        return { shape = PickShape.Box bb; trafo = Trafo3d.Identity }
      }       
    
    sg      
      |> pickable' pickable
      |> Sg.noEvents      
      |> Sg.withEvents [
          SceneEventKind.Down, (
            fun sceneHit -> 
              let intersect = picking |> Mod.force
              if intersect then              
                Log.error "hit an opc? %A" bb
                true, Seq.ofList[(HitSurface (bb,sceneHit, fun a -> a))]
              else 
                false, Seq.ofList[]
          )      
      ]

  let boxSg loadedHierarchies =
    let sg = 
      loadedHierarchies
        |> List.map (fun (_,_,info) -> Sg.wireBox (Mod.constant C4b.VRVisGreen) (Mod.constant info.GlobalBoundingBox) |> Sg.noEvents)
        |> Sg.ofList
        |> Sg.effect [ 
            toEffect Shader.stableTrafo
            toEffect DefaultSurfaces.vertexColor       
        ]    
    sg

  ///probably move to a shader
  let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
     let right = up.Cross forw
     let rotTrafo = 
         new Trafo3d(
             new M44d(
                 right.X, up.X, forw.X, 0.0,
                 right.Y, up.Y, forw.Y, 0.0,
                 right.Z, up.Z, forw.Z, 0.0,
                 0.0,     0.0,  0.0,    1.0
             ),
             new M44d(
                 right.X, right.Y, right.Z, 0.0,
                 up.X,    up.Y,    up.Z,    0.0,
                 forw.X,  forw.Y,  forw.Z,  0.0,
                 0.0,     0.0,     0.0,     1.0
             )
     )
     rotTrafo * modelt
    
  let linePass = RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main

  let billboardText (view : IMod<CameraView>) modelTrafo text =
                     
      let billboardTrafo = 
          adaptive {
              let! v = view
              let! modelt = modelTrafo
      
              return screenAligned v.Forward v.Up modelt
          }           
      Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
          |> Sg.noEvents
          |> Sg.effect [
            Shader.stableTrafo |> toEffect
          ]         
          |> Sg.trafo (0.05 |> Trafo3d.Scale |> Mod.constant )
          |> Sg.trafo billboardTrafo  

  let textSg loadedHierarchies (c:IMod<CameraView>) =
    let sg = 
      loadedHierarchies
        |> List.map (fun (_,_,info) -> 
           billboardText c (info.GlobalBoundingBox.Center |> Trafo3d.Translation |> Mod.constant) (info.Name |> Mod.constant)
         )
        |> Sg.ofList        
    sg
    
  let createSingleOpcSg (picking : IMod<bool>) (view : IMod<CameraView>) (data : Box3d*MOpcData) =
    let boundingBox, opcData = data
    
    let leaves = 
      opcData.patchHierarchy.tree
        |> QTree.getLeaves 
        |> Seq.toList 
        |> List.map(fun y -> (opcData.patchHierarchy.opcPaths.Opc_DirAbsPath, y))
            
    let loadedPatches = 
        leaves 
          |> List.map(fun (dir,patch) -> (Patch.load (OpcPaths dir) ViewerModality.XYZ patch.info,dir, patch.info)) 
          |> List.map(fun ((a,_),c,d) -> (a,c,d)) //|> List.skip 2 |> List.take 1

    //let globalBB = 
    //  Sg.wireBox (Mod.constant C4b.Red) (Mod.constant boundingBox) 
    //    |> Sg.noEvents 
    //    |> Sg.effect [ 
    //      toEffect Shader.stableTrafo
    //      toEffect DefaultSurfaces.vertexColor       
    //    ]  

    [
      opcSg loadedPatches picking boundingBox
      //boxSg  loadedPatches m boundingBox;
      textSg loadedPatches view
      //globalBB
    ] |> Sg.ofList         
      