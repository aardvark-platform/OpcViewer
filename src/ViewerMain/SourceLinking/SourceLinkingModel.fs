namespace OpcViewer.SourceLinking

open System
open System.IO
open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Base
open Chiron

#nowarn "0686"

type Ext = Ext

type Ext with
    //Hull3d
    static member FromJson1(ext : Ext, _ : Hull3d) =
        json {                    
            let! planeStrings = Json.read "planes"

            let planes = planeStrings |> Array.map(Plane3d.Parse)

            return new Hull3d(planes)
        }

    static member ToJson1 (ext : Ext, v : Hull3d) =         
        json {            
            do! Json.write "planes" (v.PlaneArray |> Array.map(fun x -> x.ToString()))
        }

    //V3d
    static member FromJson1(ext : Ext, _ : option<V3d>) =
        json {                    
            let! vector = Json.read "vector"
                 
            match vector with
            | "" -> return None
            | _ ->
                return vector |> V3d.Parse |> Some
        }

    static member ToJson1 (ext : Ext, x : option<V3d>) = 
        json {            
            match x with
            | Some v -> 
                do! Json.write "vector" (v.ToString())
            | None -> 
                do! Json.write "vector" ""   
        }    

module Ext =
    let inline fromJsonDefaults (a: ^a, _: ^b) =
        ((^a or ^b or ^e) : (static member FromJson1: ^e * ^a -> ^a Json)(Unchecked.defaultof<_>,a))
    
    let inline fromJson x =
        fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)
    
    let inline toJsonDefaults (a: ^a, _: ^b) =
        ((^a or ^b or ^e) : (static member ToJson1: ^e * ^a -> unit Json)(Unchecked.defaultof<_>,a))
    
    let inline toJson (x: 'a) =
        snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))


type CameraShotId = CameraShotId of Guid

type CameraInfo = {
    principalPoint : V2d
    focalLength    : V2d
    position       : V3d
    rotation       : Rot3d
}
with 
    static member FromJson (_ : CameraInfo) =
        json {
            let! principalPoint = Json.read "principalPoint"
            let! focalLength    = Json.read "focalLength"
            let! position       = Json.read "position"
            let! rotation       = Json.read "rotation"

            return {
                principalPoint = principalPoint |> V2d.Parse
                focalLength    = focalLength    |> V2d.Parse
                position       = position       |> V3d.Parse
                rotation       = rotation       |> Rot3d.Parse
            }
        }

    static  member ToJson(x : CameraInfo) =

        json {
            do! Json.write "principalPoint" (x.principalPoint.ToString())
            do! Json.write "focalLength"    (x.focalLength.ToString())
            do! Json.write "position"       (x.position.ToString())
            do! Json.write "rotation"       (x.rotation.ToString())
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
with 
    static member FromJson (_ : CameraShot) =
        json  {
            let! id        =  Json.read "id"
            let! info      =  Json.read "info"
            let! imageSize =  Json.read "imageSize"
            let! proj      =  Json.read "proj"
            let! hull      =  Json.readWith Ext.fromJson<Hull3d,Ext> "hull"
            let! box       =  Json.read "box"
            let! qposFile  =  Json.read "qposFile"
            let! tifFile   =  Json.read "tifFile"

            return {
                id        = id |> CameraShotId
                info      = info    
                imageSize = imageSize |> V2i.Parse
                proj      = proj |> Trafo3d.Parse
                hull      = hull
                box       = box |> Box3d.Parse
                qposFile  = qposFile
                tifFile   = tifFile 
            }
        }

    static member ToJson (x : CameraShot) =
        json {
            let (CameraShotId id) = x.id
            do!  Json.write  "id"         id
            do!  Json.write  "info"       x.info
            do!  Json.write  "imageSize" (x.imageSize.ToString())
            do!  Json.write  "proj"      (x.proj.ToString())
            do!  Json.writeWith (Ext.toJson<Hull3d, Ext>) "hull" x.hull
            do!  Json.write  "box"       (x.box.ToString())
            do!  Json.write  "qposFile"   x.qposFile
            do!  Json.write  "tifFile"    x.tifFile
        }

[<ModelType>]
type SourceLinkingModel = {
    cameras                : IndexList<CameraShot>
    filteredCameras        : IndexList<CameraShot *V2d>
    reprojectedQueryPoints : IndexList<V2d>
    queryPoint             : option<V3d>
}
with 
    static member FromJson (_ : SourceLinkingModel) =
        json  {
            let! cameras         =  Json.read "cameras"
            //let! filteredCameras =  Json.read "filteredCameras"
            let! queryPoint      =  Json.readWith Ext.fromJson<option<V3d>,Ext> "queryPoint"

            return {
                cameras         = cameras |> IndexList.ofList
                filteredCameras = IndexList.empty//filteredCameras |> IndexList.ofList
                queryPoint      = queryPoint     
                reprojectedQueryPoints = IndexList.empty
            }
        }

    static member ToJson (x : SourceLinkingModel) =
        json {            
            do!  Json.write  "cameras"          (x.cameras |> IndexList.toList)
            //do!  Json.write  "filteredCameras"  (x.filteredCameras |> IndexList.toList)
            do!  Json.writeWith Ext.toJson<option<V3d>,Ext>  "queryPoint" x.queryPoint
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

    let toHull3d1 (cs : V3d[]) =        
        Hull3d [|
            new Plane3d(cs.[0], cs.[2], cs.[1]) // near
            new Plane3d(cs.[5], cs.[7], cs.[4]) // far
            new Plane3d(cs.[0], cs.[1], cs.[4]) // bottom
            new Plane3d(cs.[1], cs.[3], cs.[5]) // left
            new Plane3d(cs.[4], cs.[6], cs.[0]) // right
            new Plane3d(cs.[3], cs.[2], cs.[7]) // top
        |]

    let toHull3d2 (cs : V3d[]) =        
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
        let pngFile = Path.ChangeExtension(path, ".png")

        if pngFile |> File.Exists |> not then
            Log.startTimed "[SourceLinking] creating png"
            try                
                PixImage.Create(tifFile).ToPixImage<byte>().SaveAsImage(pngFile)
            with
                | e -> Log.line "[SourceLinking] png conversion failed: %A" e
            Log.stop()
        
        let info = CameraInfo.fromQpos qposFile
        
        let shot =
            {
                init() with
                    info = info
                    imageSize = V2i(4104, 3006)
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
    let initial =
        {
            cameras = IndexList.empty
            filteredCameras = IndexList.empty
            queryPoint = None
            reprojectedQueryPoints = IndexList.empty
        }


