namespace CrackDetection

open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type InputPoint =
    {
        index           : int
        //level0KdTree    : Option<LazyKdTree>
        position        : V3d      // picking position
        uv              : V2d      // texture coordinates at picking position
        coeff           : float    // value in edgemap at picking position 
    }

[<DomainType>]
type OutputPoint =
    {
        uv : V2d
        position : V3d
    }

[<DomainType>]
type CrackDetectionModel =
    {
        inputPoints  : plist<InputPoint>
        outputPoints : plist<OutputPoint>
        kdTreePath   : Option<string>
    }

type CrackDetectionAction =
    | AddCrackPoint of position : V3d * uv : V2d * coeff : float * index : int
    | FinishCrack   of edgeMapPath : string * posPath : string * trafo : Trafo3d