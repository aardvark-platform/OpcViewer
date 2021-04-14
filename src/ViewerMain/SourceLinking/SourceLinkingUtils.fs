namespace OpcViewer.SourceLinking

open System
open System.IO
open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Base
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
        hull      = Hull3d.Invalid
        box       = Box3d.Invalid
        qposFile  = String.Empty
        tifFile   = String.Empty
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

    let toHull3d2 (cs : V3d[]) =
        //let t = viewProj
        //let cs = canonicalViewVolume.ComputeCorners() |> Array.map(fun x -> t.Forward.TransformPosProj x)
        Hull3d [|
            new Plane3d(cs.[0], cs.[1], cs.[2]) // near
            new Plane3d(cs.[5], cs.[4], cs.[7]) // far
            new Plane3d(cs.[0], cs.[4], cs.[1]) // bottom
            new Plane3d(cs.[1], cs.[5], cs.[3]) // left
            new Plane3d(cs.[4], cs.[0], cs.[6]) // right
            new Plane3d(cs.[3], cs.[7], cs.[2]) // top
        |]

    let createFrustumProj (c: CameraShot) =
        let aspectRatio = (float c.imageSize.X) / (float c.imageSize.Y)

        let hFov = (float c.imageSize.X ) / c.info.focalLength.X |> atan 

       // Log.line "hFov %A" (hFov.DegreesFromRadians())

        let vFov = hFov * aspectRatio
        let frustum = Frustum.perspective (hFov.DegreesFromRadians()) 0.01 5.0 aspectRatio
      
        Frustum.projTrafo(frustum)
    
    let fromFileName (path : string) =

        let tifFile = Path.ChangeExtension(path, ".tif")
        let qposFile = Path.ChangeExtension(path, ".qpos")
        
        let info = CameraInfo.fromQpos qposFile
        
        let shot =
            {
                init() with
                    info = info
                    imageSize = V2i(4104,3006)
            }

        let proj = shot |> createFrustumProj        

        let trans = shot.info.position|> Trafo3d.Translation 
        let rot = Trafo3d(shot.info.rotation)
        let trafo = (proj.Inverse * rot)

        //option1
        let points = 
            Box3d(V3d.NNN, V3d.III).ComputeCorners() 
            |> Array.map(trafo.Forward.TransformPosProj)

        let hull = (toHull3d2 points)                

        { shot with proj = proj; hull = hull; tifFile = tifFile; qposFile = qposFile }
                        
module SourceLinkingModel =
    let init() =
        {
            cameras = IndexList.empty
            filteredCameras = IndexList.empty
            queryPoint = None
        }