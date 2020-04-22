namespace CrackDetection

open Aardvark.Base
open Aardvark.Base.Incremental
open OpcViewer.Base.Picking
open Aardvark.SceneGraph.Opc

[<DomainType>]
type CrackDetectionModel =
    {
        inputPoints  : HitInfo plist
        outputPoints : V3d plist
    }

type CrackDetectionAction =
    | AddCrackPoint of HitInfo
    | FinishCrack of PatchHierarchy seq