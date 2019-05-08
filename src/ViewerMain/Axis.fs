namespace OpcSelectionViewer

module AxisFunctions = 
  open Aardvark.Base
  open Aardvark.UI
  open Aardvark.UI.Trafos 
  open Aardvark.Base.Rendering
  open Aardvark.Base.Incremental
 

  let createDomainAxis (ops : OrientedPoint[]) : OpcSelectionViewer.Axis = 
      let length = 
        ops
          |> List.ofArray 
          |> List.pairwise 
          |> List.fold (fun l (a,b) -> l + (b.position - a.position).Length) 0.0
        
      {
          positions        = ops |> Array.map (fun x -> x.position) |> List.ofArray
          pointList        = ops |> PList.ofArray             
          length           = length
          rangeSv          = if length > 1.0 then 
                               ops |> Array.map(fun x -> x.stationing.sv) |> Range1d
                             else Range1d.Invalid
      }
    
  let loadAxis (path : string) : OpcSelectionViewer.Axis = 
    let orientedPoints = OpcSelectionViewer.Serialization.loadAsType<OrientedPoint[]> path
    
    createDomainAxis orientedPoints

  let toColoredEdges (offset:V3d) (color : C4b) (points : array<V3d>) =
    points
      |> Array.map (fun x -> x-offset)
      |> Array.pairwise
      |> Array.map (fun (a,b) -> (new Line3d(a,b), color))
    
  let drawColoredEdges width edges = 
    edges
      |> IndexedGeometryPrimitives.lines
      |> Sg.ofIndexedGeometry
      |> Sg.effect [
        toEffect Shader.stableTrafo
        toEffect DefaultSurfaces.vertexColor
        toEffect DefaultSurfaces.thickLine
        
      ]
      |> Sg.uniform "LineWidth" (Mod.constant width)
      |> Sg.uniform "depthOffset" (Mod.constant 0.1)
  
  let lines (color : C4b) (width : double)  (points : V3d[]) =
      let offset =
        match points |> Array.tryHead with
        | Some h -> h
        | None -> V3d.Zero

      points 
        |> toColoredEdges offset color
        |> drawColoredEdges width
        |> Sg.trafo (offset |> Trafo3d.Translation |> Mod.constant)