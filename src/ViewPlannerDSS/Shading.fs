namespace ViewPlanner
open Aardvark.Base
open Aardvark.Rendering
open FShade


module Shading =

     type Vertex =
        {
            [<Position>]                pos     : V4d            
            [<TexCoord>]                tc      : V2d
            [<Color>]                   color   : V4d
            [<SourceVertexIndex>]       sv      : int
            [<Semantic("Tex0")>]        tc0     : V4d

        }
    
     type UniformScope with
        member x.FootprintMVP = uniform?FootprintMVP

     let vert (v:Vertex) =
        
        vertex {
            let mvp : M44d = uniform.FootprintMVP

            return 
                { v 
                    
                    with 
                        tc0 = mvp * v.pos 
                        pos = uniform.ModelViewProjTrafo * v.pos
                }
            }

        
    
     let frag (v:Vertex) =

        fragment {
            
            let clip = v.tc0 
            let low = -clip.W
            let upp = clip.W
            let col = 
                if (clip.X > low && clip.X < upp && clip.Y > low && clip.Y < upp && clip.Z > low && clip.Z < upp) then
                    V4d(0.0, 0.0, 1.0, 1.0)
                else
                    v.color

            return col
        }
