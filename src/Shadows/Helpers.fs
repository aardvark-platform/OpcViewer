namespace Shadows
open Aardvark.Base

module Helpers =
  open FShade

  let printShader (effect : Effect) =

    let lastShader = 
      match effect.LastShader with
        | Some s -> s
        | None -> 
          match effect.LastPrimShader with
            | Some t -> t
            | None -> failwith "boo"

    // FSahde Examples print
    let mutable index = 0
    let id () =
        let i = index
        index <- i + 1
        i
          
    let outputs = lastShader.shaderOutputs |> Map.remove Intrinsics.SourceVertexIndex |> Map.map (fun name desc -> desc.paramType, id())

    let glShader = 
      effect
        |> Effect.toModule {
            EffectConfig.empty with 
                lastStage = lastShader.shaderStage
                outputs = outputs
            }
        |> ModuleCompiler.compileGLSL410
     
    
    
    printfn "%s" glShader.code
    printfn "------------------------------------------------" 

  let printShaders (effects : List<Effect>) =
    for e in effects do
      printShader e