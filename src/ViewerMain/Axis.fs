namespace OpcSelectionViewer

module AxisFunctions = 
  open Aardvark.Base
  open Aardvark.Base.Incremental

  let createDomainAxis (ops : OrientedPoint[]) : Axis = 
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
    
  let loadAxis (path : string) : Axis = 
    let orientedPoints = OpcSelectionViewer.Serialization.loadAsType<OrientedPoint[]> path
    
    createDomainAxis orientedPoints
