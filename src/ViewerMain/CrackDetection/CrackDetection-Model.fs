namespace CrackDetection

open Aardvark.Base
open Aardvark.Base.Incremental
open OpcViewer.Base.Picking

[<DomainType>]
type CrackDetectionModel =
    {
        inputPoints  : HitInfo plist
        outputPoints : V3d plist
    }

type CrackDetectionAction =
    | AddCrackPoint of HitInfo
    | FinishCrack