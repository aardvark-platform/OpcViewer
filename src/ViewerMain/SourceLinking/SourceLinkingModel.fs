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
        proj      = Trafo3d.Identity        
    }

    // in Aardvark.Base.FSharp/Datastructures/Geometry/Boundable.fs as well as KdTreeFinds.fs (in both cases private)
    let toHull3d (viewProj : Trafo3d) =
        let r0 = viewProj.Forward.R0
        let r1 = viewProj.Forward.R1
        let r2 = viewProj.Forward.R2
        let r3 = viewProj.Forward.R3

        let inline toPlane (v : V4d) =
            Plane3d(-v.XYZ, v.W)

        Hull3d [|
            r3 - r0 |> toPlane  // right
            r3 + r0 |> toPlane  // left
            r3 + r1 |> toPlane  // bottom
            r3 - r1 |> toPlane  // top
            r3 + r2 |> toPlane  // near
            //r3 - r2 |> toPlane  // far
        |]

    let createFrustumProj (c: CameraShot) =
        let aspectRatio = (float c.imageSize.Y) / (float c.imageSize.X)

        let hFov = (float c.imageSize.X ) / c.info.focalLength.X |> atan 

        Log.line "hFov %A" (hFov.DegreesFromRadians())

        let vFov = hFov * aspectRatio
        let frustum = Frustum.perspective (hFov.DegreesFromRadians()) 0.01 5.0 aspectRatio
      
        Frustum.projTrafo(frustum)
    
    let fromFileName (path : string) =

        let tifFile = Path.ChangeExtension(path, ".tif")
        let qposFile = Path.ChangeExtension(path, ".qpos")

        //let img = PixImage.Create(tifFile) //.ToPixImage<byte>()
        //img.Size |> Log.line "%A"

        let info = CameraInfo.fromQpos qposFile
        
        let shot =
            {
                init() with
                    info = info
                    imageSize = V2i(4104,3006)
            }

        let proj = shot |> createFrustumProj

        { shot with proj = proj }
                        
module SourceLinkingModel =
    let init() =
        {
            cameras = IndexList.empty
            filteredCameras = IndexList.empty
            queryPoint = None
        }
