namespace OpcSelectionViewer

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open FShade
open Aardvark.Rendering
open Aardvark.UI.Trafos
open Aardvark.SceneGraph.Opc

open Aardvark.UI
open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcOutlineTest

module OutlineSg =
   
  let read a =
    { StencilMode.None with 
        Comparison = ComparisonFunction.Greater
        CompareMask = StencilMask 0xff
        Reference = a
    }
    //StencilMode(StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

  let write a =
    { StencilMode.None with 
        Comparison = ComparisonFunction.Greater
        CompareMask = StencilMask 0xff
        Reference = a
        Pass = StencilOperation.Replace
        DepthFail = StencilOperation.Replace
        Fail = StencilOperation.Keep
    }
    //StencilMode(StencilOperationFunction.Replace, StencilOperationFunction.Replace, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)
    
  let pass0 = RenderPass.main
  let pass1 = RenderPass.after "outline" RenderPassOrder.Arbitrary pass0

  let createOutlineOpcSg (m : AdaptiveOutlineModel) (data : Box3d*AdaptiveOpcData) =
    let boundingBox, opcData = data
    
    let leaves = 
      opcData.patchHierarchy.tree
        |> QTree.getLeaves 
        |> Seq.toList 
        |> List.map(fun y -> (opcData.patchHierarchy.opcPaths.Opc_DirAbsPath, y))
    
    let sg = 
      let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
    
      leaves 
        |> List.map(fun (dir,patch) -> (Aardvark.SceneGraph.Opc.Patch.load (OpcPaths dir) ViewerModality.XYZ patch.info,dir, patch.info)) 
        |> List.map(fun ((a,_),c,d) -> (a,c,d))
        |> List.map (fun (g,dir,info) -> 
        
          let texPath = Patch.extractTexturePath (OpcPaths dir) info 0
          let tex = FileTexture(texPath,config) :> ITexture
                    
          Sg.ofIndexedGeometry g
              |> Sg.trafo (AVal.constant info.Local2Global)             
              |> Sg.diffuseTexture (AVal.constant tex)             
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
                        |> Sg.stencilMode (AVal.constant (write 1))
                        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])
                        |> Sg.effect [ 
                            toEffect Shader.stableTrafo
                            toEffect DefaultSurfaces.diffuseTexture       
                            ]

                let outline = 
                    sg
                        |> Sg.noEvents
                        |> Sg.stencilMode (AVal.constant (read 1))
                        |> Sg.depthTest (AVal.constant DepthTest.None)
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

