namespace OpcSelectionViewer

open Aardvark.Base  
open Aardvark.Base.Incremental
open Aardvark.UI

module AxisSg =

  let createAxisSg (positions : List<V3d>) =
    positions |> Array.ofList |> AxisFunctions.lines C4b.VRVisGreen 2.0
    
  let addDebuggingAxisPointSphere (selectionPos : Option<V3d>) = 
    selectionPos |> 
      Option.map(fun pos -> Mod.constant pos |> AxisFunctions.sphere C4b.VRVisGreen 0.1)
      |> Option.defaultValue Sg.empty

  let axisSgs (model : MModel) = 
    aset {
      let! axis = model.axis
      
      match axis with
        | Some a -> 
          let! positions = a.positions
          yield createAxisSg positions
          
          let! selectionPos = a.selectionOnAxis
          yield addDebuggingAxisPointSphere selectionPos
        | None -> yield Sg.empty
    } |> Sg.set

