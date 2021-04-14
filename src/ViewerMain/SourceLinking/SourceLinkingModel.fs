namespace OpcViewer.SourceLinking

open System
open System.IO
open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Base

type CameraShotId = CameraShotId of Guid

type CameraInfo = {
    principalPoint : V2d
    focalLength    : V2d
    position       : V3d
    rotation       : Rot3d
}

type CameraShot = {
    id        : CameraShotId
    info      : CameraInfo
    imageSize : V2i
    proj      : Trafo3d    
    hull      : Hull3d
    box       : Box3d
    qposFile  : string
    tifFile   : string
}

[<ModelType>]
type SourceLinkingModel = {
    cameras         : IndexList<CameraShot>
    filteredCameras : IndexList<CameraShot>
    queryPoint      : option<V3d>
}

type SourceLinkingAction =
| LoadCameras      of string
| PickQueryPoint   of V3d
    




