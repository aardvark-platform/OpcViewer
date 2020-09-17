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
}

[<ModelType>]
type SourceLinkingModel = {
    cameras : IndexList<CameraShot>
}

type SourceLinkingAction =
    | LoadCameras of string

module CameraInfo =
    let init() = {
        principalPoint = V2d.NaN
        focalLength    = V2d.NaN
        position       = V3d.NaN
        rotation       = Rot3d.Identity    
    }

    let private doubleParse s =
        Double.Parse(s, Globalization.CultureInfo.InvariantCulture)

    let fromQpos (fileName : string) =
        
        let lines = File.readAllLines fileName

        let first = lines.[0] |> String.split ' '        
        let pp = (
            first.[0] |> doubleParse, 
            first.[1] |> doubleParse) |> V2d

        let fl = (
            first.[2] |> doubleParse, 
            first.[3] |> doubleParse) |> V2d

        let second = lines.[1] |> String.split ' '
        let pos = (
            second.[0] |> doubleParse, 
            second.[1] |> doubleParse,
            second.[2] |> doubleParse) |> V3d

        let third = lines.[2] |> String.split ' '
        let rot = (
            third.[0] |> doubleParse, 
            third.[1] |> doubleParse,
            third.[2] |> doubleParse,
            third.[3] |> doubleParse) |> QuaternionD |> Rot3d                

        {            
            principalPoint = pp
            focalLength    = fl
            position       = pos
            rotation       = rot
        }

module CameraShot =

    let init() = {
        id        = Guid.NewGuid() |> CameraShotId
        info      = CameraInfo.init()
        imageSize = V2i.Zero
    }
    
    let fromFileName (path : string) =

        let tifFile = Path.ChangeExtension(path, ".tif")
        let qposFile = Path.ChangeExtension(path, ".qpos")

        //let img = PixImage.Create(tifFile) //.ToPixImage<byte>()
        //img.Size |> Log.line "%A"

        let info = CameraInfo.fromQpos qposFile

        {
            init() with
                info = info
                imageSize = V2i(4104,3006)
        }
    

    

module SourceLinkingModel =
    let init() =
        {
            cameras = IndexList.empty        
        }
