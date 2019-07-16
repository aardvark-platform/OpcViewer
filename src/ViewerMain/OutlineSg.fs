namespace OpcSelectionViewer

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

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcOutlineTest

module OutlineSg =
   
  let read a =
        StencilMode(StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

  let write a =
        StencilMode(StencilOperationFunction.Replace, StencilOperationFunction.Replace, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)
    
  let pass0 = RenderPass.main
  let pass1 = RenderPass.after "outline" RenderPassOrder.Arbitrary pass0

  let createOutlineOpcSg (m : MOutlineModel) (data : Box3d*MOpcData) =
    let boundingBox, opcData = data
    
    let leaves = 
      opcData.patchHierarchy.tree
        |> QTree.getLeaves 
        |> Seq.toList 
        |> List.map(fun y -> (opcData.patchHierarchy.opcPaths.Opc_DirAbsPath, y))
    
    let sg = 
      let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
    
      leaves 
        |> List.map(fun (dir,patch) -> (Patch.load (OpcPaths dir) ViewerModality.XYZ patch.info,dir, patch.info)) 
        |> List.map(fun ((a,_),c,d) -> (a,c,d))
        |> List.map (fun (g,dir,info) -> 
        
          let texPath = Patch.extractTexturePath (OpcPaths dir) info 0
          let tex = FileTexture(texPath,config) :> ITexture
                    
          Sg.ofIndexedGeometry g
              |> Sg.trafo (Mod.constant info.Local2Global)             
              |> Sg.diffuseTexture (Mod.constant tex)             
          )
        |> Sg.ofList   
    
    let test = 
         aset {
            let! useOut = m.useOutlines
            let regular = 
                sg
                    |> Sg.effect [ 
                        toEffect Shader.stableTrafo
                        toEffect DefaultSurfaces.diffuseTexture       
                        ]
                    |> Sg.pass pass0

            match useOut with
             | true ->
                yield regular
               
                let mask = 
                    sg
                        |> Sg.noEvents 
                        |> Sg.pass pass0
                        |> Sg.stencilMode (Mod.constant (write 1))
                        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.diffuseTexture       
                            ]

                let outline = 
                    sg
                        |> Sg.noEvents
                        |> Sg.stencilMode (Mod.constant (read 1))
                        |> Sg.depthTest (Mod.constant DepthTestMode.None)
                        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])
                        |> Sg.pass pass1
                        |> Sg.effect [
                            Shader.stableTrafo |> toEffect
                            Shader.lines |> toEffect
                            DefaultSurfaces.thickLine |> toEffect
                            DefaultSurfaces.constantColor C4f.VRVisGreen |> toEffect
                        ]
                        |> Sg.uniform "LineWidth" m.lineThickness.value

                yield mask
                yield outline
             | _ -> yield regular
   
                //Sg.ofSeq [regular; mask; outline] |> Sg.noEvents 
         } |> Sg.set
    test

