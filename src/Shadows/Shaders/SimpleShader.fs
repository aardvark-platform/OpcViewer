namespace Shadows
open FShade
open Aardvark.Base
open Aardvark.Base.Rendering
// module SimpleShader

module TestShader = 
  module Transformation = 
      type Vertex =
          {
              // [<Position>] is a shorthand for [<Semantic("Positions")>]
              [<Position>]                pos : V4d 
              [<Semantic("Normal")>]      normal : V3d

          }

      let shader (v : Vertex) =
          vertex {
              return {
                  pos = v.pos
                  normal = v.normal
                  
              }
          }
  
  module TestShaders =
      type UniformScope with
          member x.lightLocation : V3d = x?lightLocation

      //type Vertex = { 
      //  [<Position>] p : V4d
      //  [<WorldPosition>] wp : V4d
      //  [<Color>] c : V4d
      //  [<Normal>] n : V3d
      //}

      let vertexShader (v : Aardvark.Base.Rendering.Effects.Vertex) =
        vertex {
          let c = V4d.IIII
          let pos = uniform.ProjTrafo * (uniform.ModelViewTrafo * v.pos)
          let wp = uniform.ModelTrafo * pos
          return {v with c = c
                         pos = pos
                         wp = wp
                 }
          

        }

      
      let calcNormals (t : Triangle<Aardvark.Base.Rendering.Effects.Vertex>) =
        FShade.ShaderBuilders.triangle {
            let v1 = t.P0.pos - t.P1.pos
            let v2 = t.P2.pos - t.P1.pos
            let m4 = uniform.ModelTrafo.Transposed.Inverse
            let m3 = M33d.FromCols(m4.C0.XYZ, m4.C1.XYZ, m4.C2.XYZ)
            let n = m3 * (v1.XYZ.Cross v2.XYZ).Normalized 
            
            yield {t.P0 with n = n}
            yield {t.P1 with n = n}
            yield {t.P2 with n = n}
        }

      let fragmentShader (v : Aardvark.Base.Rendering.Effects.Vertex) =

        fragment {
              

            let n = v.n |> Vec.normalize
            let dir = uniform.lightLocation - v.wp.XYZ |> Vec.normalize

            let ambient = 0.2
            let diffuse = Vec.dot (uniform.ViewTrafo * V4d(dir,0.0)).XYZ n |> max 0.0

            let l = ambient + (1.0 - ambient) * diffuse

            return V4d(v.c.XYZ * diffuse, v.c.W)
                                //V4d.IIII}//
          }


  let simpleOpcColourShader =
    Effect.compose [
     // Effect.ofFunction Aardvark.UI.Trafos.Shader.stableTrafo
      Effect.ofFunction TestShaders.vertexShader
      Effect.ofFunction TestShaders.calcNormals
      Effect.ofFunction TestShaders.fragmentShader
          
    ]


