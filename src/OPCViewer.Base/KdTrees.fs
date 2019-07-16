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

  let relativePath (path : string) (remaining : int) =
    path.Split(Path.DirectorySeparatorChar) 
    |> List.ofArray 
    |> List.rev 
    |> List.take remaining 
    |> List.rev 
    |> Path.combine

  let relativePath' (path : string) =
    relativePath path 3

  let expandKdTreePaths basePath kd = 
    kd 
      |> HMap.map(fun _ k ->
        match k with 
          | Level0KdTree.LazyKdTree lkt -> 
            let kdTreeSub   = lkt.kdtreePath    |> relativePath'
            let triangleSub = lkt.objectSetPath |> relativePath'
      
            LazyKdTree { 
              lkt with 
                  kdtreePath    = Path.Combine(basePath, kdTreeSub)
                  objectSetPath = Path.Combine(basePath, triangleSub)
            }
          | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik
        )