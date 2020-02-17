namespace OpcViewer.Base

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open OpcViewer.Base.FalseColors


[<DomainType>]
type InputPoint =
    {
        position : V3d      // picking position
        uv       : V2d      // texture coordinates at picking position
        coeff    : float    // value in edgemap at picking position
    }

[<DomainType>]
//type Crack =
type CrackDetectionModel =
    {
        inputPoints  : plist<InputPoint>
        outputPoints : plist<V2d>
    }
//type Crack =
//    {
//        uvPoints2d : plist<V2d> //plist<V3d>
//        points3d   : plist<V3d>
//    }

//[<DomainType>]
//type CrackDetectionModel =
//    {
//        cracks     : plist<Crack>
//    }
    

type CrackDetectionAction =
  | AddCrackPoint     of V3d*V2d*float
  | FinishCrack       of string*string

    

   