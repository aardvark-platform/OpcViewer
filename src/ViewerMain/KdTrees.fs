namespace Aardvark.VRVis.Opc

open Aardvark.Geometry
open Aardvark.Base
open System.IO


module KdTrees = 
  type LazyKdTree = {
      kdTree          : option<ConcreteKdIntersectionTree>
      affine          : Trafo3d
      boundingBox     : Box3d        
      kdtreePath      : string
      objectSetPath   : string
      coordinatesPath : string
      texturePath     : string
    }
  
  type InCoreKdTree = {
      kdTree      : ConcreteKdIntersectionTree
      boundingBox : Box3d
    }
  
  type Level0KdTree = 
      | LazyKdTree   of LazyKdTree
      | InCoreKdTree of InCoreKdTree
      
  let relativePath (remaining : int) (path : string)  =
      path.Split(Path.DirectorySeparatorChar) 
      |> List.ofArray 
      |> List.rev 
      |> List.take remaining 
      |> List.rev 
      |> Path.combine

  let relativePath' (path : string) =
    relativePath 3 path

  let expandKdTreePaths basePath kd = 
    kd 
      |> HMap.map(fun _ k ->
        match k with 
          | Level0KdTree.LazyKdTree lkt -> 
            let kdTreeSub   = lkt.kdtreePath    |> relativePath'
            let triangleSub = lkt.objectSetPath |> relativePath'
            let texturePath = lkt.texturePath   |> relativePath'
            let coordinatePath = lkt.coordinatesPath |> relativePath'

            LazyKdTree { 
              lkt with 
                  kdtreePath    = Path.Combine(basePath, kdTreeSub)
                  objectSetPath = Path.Combine(basePath, triangleSub)
                  texturePath   = Path.Combine(basePath, texturePath)
                  coordinatesPath   = Path.Combine(basePath, coordinatePath)
            }
          | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik
        )