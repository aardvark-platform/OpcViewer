namespace OpcSelectionViewer

module Axis = 
  open Aardvark.Base
  open Aardvark.Base.Incremental

  type Stationing = {
      sh : double
      sv : double
  }

  type OrientedPoint = {
      direction             : V3d
      offsetToMainAxisPoint : V3d
      position              : V3d
      stationing            : Stationing
  }

  [<DomainType;ReferenceEquality>]
  type Axis = {
      [<NonIncremental>]
      positions       : list<V3d>
      pointList       : plist<OrientedPoint>
      length          : float
      rangeSv         : Range1d
  }

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
    let orientedPoints = OpcSelectionViewer.Serialization.loadAs path
    
    createDomainAxis orientedPoints
