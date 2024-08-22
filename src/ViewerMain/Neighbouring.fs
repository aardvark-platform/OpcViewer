namespace OpcSelectionViewer

open Aardvark.Base
open Aardvark.Data.Opc
open OpcViewer.Base.Picking
open FSharp.Data.Adaptive


module Neighbouring = 

  let rec neighborPatches (neighborMap : HashMap<Box3d,BoxNeighbors>) (patchElements : QTree<Patch>[]) (patchTree : QTree<Patch>) : HashMap<Box3d,BoxNeighbors> = 
    let returnMap = 
      match patchTree with 
        | QTree.Node (n,f) -> 
          f 
            |> Array.map(fun node -> neighborPatches neighborMap f node)
            |> Array.fold(fun unified current -> HashMap.union unified current) neighborMap
          
        | QTree.Leaf l -> 
          let blubb = 
            seq [
              for node in patchElements do
                match node with
                  | QTree.Node (n, f) ->
                    yield n.info.GlobalBoundingBox
                  | QTree.Leaf l ->
                    yield l.info.GlobalBoundingBox
            ]
          
          neighborMap            
    returnMap

  let calculateNeighbors (pH : PatchHierarchy) =
    let leaves = QTree.getLeaves pH.tree
    
    match pH.tree with
      | QTree.Node (n,f) -> failwith ""
      | QTree.Leaf l -> failwith ""      
      

    for leaf in leaves do
      leaf.info.GlobalBoundingBox |> ignore

    failwith "neighborcalculation failed"