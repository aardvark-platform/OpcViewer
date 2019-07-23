namespace OpcViewer.Base

module Shader =
  open Aardvark.Base.Rendering.Effects
  open Aardvark.Base
  open Aardvark.Base.Rendering
  open FShade

  type SuperVertex = 
        {
            [<Position>] pos :  V4d
            [<SourceVertexIndex>] i : int
        }

  let lines (t : Triangle<SuperVertex>) =
        line {
            yield t.P0
            yield t.P1
            restartStrip()
            
            yield t.P1
            yield t.P2
            restartStrip()

            yield t.P2
            yield t.P0
            restartStrip()
        }

  module DebugColor = 
    let internal debugCol (p : Vertex) =
      let c : C4b = uniform?Color
      let c1 = c.ToC4d().ToV4d().XYZ
      fragment {
        let useDebugColor : bool = uniform?UseDebugColor 
        if useDebugColor then
          return V4d.IOOI
        else
          return V4d(c1, 0.3) 
      }

    let Effect = 
        toEffect debugCol

  module VertexCameraShift = 
    let internal toCameraShift (p : Vertex) =
      vertex {
        let (offset : float) = (uniform?depthOffset)
        
        let wp = p.wp
        let viewVec = ((wp.XYZ - uniform.CameraLocation).Normalized)
        let viewVec = V4d(viewVec.X, viewVec.Y, viewVec.Z, 0.0)
        let wpShift = wp + viewVec * offset
        let posShift = uniform.ViewProjTrafo * wpShift

      return { p with pos = posShift; wp = wpShift }
      }

    let Effect = 
      toEffect toCameraShift

  module PointSprite = 
    let internal pointSprite (p : Point<Vertex>) =
      triangle {
        let s = uniform.PointSize / V2d uniform.ViewportSize
        let pos = p.Value.pos
        let pxyz = pos.XYZ / pos.W

        let p00 = V3d(pxyz + V3d( -s.X*0.33, -s.Y, 0.0 ))
        let p01 = V3d(pxyz + V3d(  s.X*0.33, -s.Y, 0.0 ))
        let p10 = V3d(pxyz + V3d( -s.X,      -s.Y*0.33, 0.0 ))
        let p11 = V3d(pxyz + V3d(  s.X,      -s.Y*0.33, 0.0 ))
        let p20 = V3d(pxyz + V3d( -s.X,       s.Y*0.33, 0.0 ))
        let p21 = V3d(pxyz + V3d(  s.X,       s.Y*0.33, 0.0 ))
        let p30 = V3d(pxyz + V3d( -s.X*0.33,  s.Y, 0.0 ))
        let p31 = V3d(pxyz + V3d(  s.X*0.33,  s.Y, 0.0 ))

        yield { p.Value with pos = V4d(p00 * pos.W, pos.W); tc = V2d (0.33, 0.00); }
        yield { p.Value with pos = V4d(p01 * pos.W, pos.W); tc = V2d (0.66, 0.00); }
        yield { p.Value with pos = V4d(p10 * pos.W, pos.W); tc = V2d (0.00, 0.33); }
        yield { p.Value with pos = V4d(p11 * pos.W, pos.W); tc = V2d (1.00, 0.33); }
        yield { p.Value with pos = V4d(p20 * pos.W, pos.W); tc = V2d (0.00, 0.66); }
        yield { p.Value with pos = V4d(p21 * pos.W, pos.W); tc = V2d (1.00, 0.66); }
        yield { p.Value with pos = V4d(p30 * pos.W, pos.W); tc = V2d (0.33, 1.00); }
        yield { p.Value with pos = V4d(p31 * pos.W, pos.W); tc = V2d (0.66, 1.00); }
      }

    let Effect = 
        toEffect pointSprite

  module PointSpriteQuad =       
    let internal pointSpriteQuad (p : Point<Vertex>) =
      triangle {
        let s = (uniform.PointSize / V2d uniform.ViewportSize)
        let pos = p.Value.pos
        let pxyz = pos.XYZ / pos.W

        let p00 = V3d(pxyz + V3d( -s.X, -s.Y, 0.0 ))
        let p01 = V3d(pxyz + V3d(  s.X, -s.Y, 0.0 ))
        let p10 = V3d(pxyz + V3d(  s.X,  s.Y, 0.0 ))
        let p11 = V3d(pxyz + V3d( -s.X,  s.Y, 0.0 ))

        yield { p.Value with pos = V4d(p00 * pos.W, pos.W); tc = V2d (0.00, 0.00); }
        yield { p.Value with pos = V4d(p01 * pos.W, pos.W); tc = V2d (1.00, 0.00); }
        yield { p.Value with pos = V4d(p11 * pos.W, pos.W); tc = V2d (0.00, 1.00); }          
        yield { p.Value with pos = V4d(p10 * pos.W, pos.W); tc = V2d (1.00, 1.00); }
      }
    
    let Effect = 
      toEffect pointSpriteQuad



  module AttributeShader = 

      type AttrVertex =
            {
                [<Position>]                pos     : V4d            
                [<WorldPosition>]           wp      : V4d
                [<TexCoord>]                tc      : V2d
                [<Color>]                   c       : V4d
                [<Normal>]                  n       : V3d
                [<Semantic("Scalar")>]      scalar  : float
                [<Semantic("LightDir")>]    ldir    : V3d
            }
  
      let falseColorLegendGray (v : AttrVertex) =
          fragment {   
              if (uniform?falseColors) 
              then
                 let fcUpperBound     = uniform?upperBound
                 let fcLowerBound     = uniform?lowerBound
                 let k = (v.scalar - fcLowerBound) / (fcUpperBound-fcLowerBound) 
                 let value = clamp 0.0 1.0 k
                 return V4d(value, value, value, 1.0) 
              else
                  return v.c
          }