namespace CrackDetection

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.VRVis.Opc.KdTrees

type InputPoint =
    {
        index           : int
        //level0KdTree    : Option<LazyKdTree>
        position        : V3d      // picking position
        uv              : V2d      // texture coordinates at picking position
        coeff           : float    // value in edgemap at picking position 
    }

[<DomainType>]
type CrackDetectionModel =
    {
        inputPoints  : plist<InputPoint>
        outputPoints : plist<V2d>
    }


type CrackDetectionAction =
| AddCrackPoint     of V3d*V2d*float*int
| FinishCrack       of string*string

    

   