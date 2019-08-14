namespace Linking

open Aardvark.Base

open System
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application

open PRo3D.Minerva
open FShade
open OpcViewer.Base
open Aardvark.UI

module LinkingApp =

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

    let loadFrustums (features: plist<Feature>) : hmap<string, LinkingFeature> * Trafo3d =
        
        // only interested in MastcamL and MastcamR products
        let reducedFeatures = features.Filter (fun _ f -> 
            match f.instrument with
            | Instrument.MastcamL | Instrument.MastcamR -> true
            | Instrument.Mastcam -> false // TODO: dunno?
            | _ -> false
        )

        // creating frustums by specifying fov
        let createFrustumProj (fov : float) =
            let frustum = Frustum.perspective fov 0.01 15.0 1.0
            let proj = Frustum.projTrafo(frustum)
            (proj, proj.Inverse)

        let cam34Frustum, cam34Inv = createFrustumProj 16.370
        let cam100Frustum, cam100Inv = createFrustumProj 5.67

        let angleToRad = V3d(Math.PI / 180.0) * V3d(1.0,1.0,2.0)
        
        let originTrafo = 
            match reducedFeatures.TryGet 0 with
            | Some v -> Trafo3d.Translation v.geometry.positions.Head
            | None -> Trafo3d.Identity

        let originTrafoInv = originTrafo.Backward
        
        //|> Mod.map2 (fun (t: Trafo3d) x -> 
        //    x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) trafo

        // map minerva features to linking features
        let linkingFeatures : hmap<string, LinkingFeature> = 
            reducedFeatures.Map (fun _ f ->

                let position = originTrafoInv.TransformPos(f.geometry.positions.Head)
                let angles = f.geometry.coordinates.Head

                let color = f.instrument |> MinervaModel.instrumentColor

                let frustumTrafo =
                    match f.instrument with
                    | Instrument.MastcamL -> cam34Inv
                    | Instrument.MastcamR -> cam100Inv
                    | Instrument.Mastcam -> (Trafo3d.Scale 0.0) // TODO: dunno?
                    | _ -> (Trafo3d.Scale 0.0) // TODO: discard!

                let rotation = Rot3d.FromAngleAxis(angles * angleToRad)
                let translation = Trafo3d.Translation position

                let innerRot = Trafo3d.Rotation(V3d.OOI, -angles.Z * Math.PI / 180.0)

                let rotTranslateTrafo = innerRot * Trafo3d(rotation) * translation
                let trafo = frustumTrafo * rotTranslateTrafo

                let hull = trafo |> toHull3d 

                (f.id, {
                    id = f.id
                    hull = hull
                    position = position
                    rotation = rotation
                    trafo = trafo
                    color = color
                })
            )
            |> PList.toList
            |> HMap.ofList

        (linkingFeatures, originTrafo)


    let update (view: CameraView) (m: LinkingModel) (msg: LinkingAction) : LinkingModel =
        match msg with
        | MinervaAction a ->
            match a with

            | MinervaAction.LoadProducts _ -> 
                let minervaModel = MinervaApp.update view m.minervaModel a
                let (frustums, trafo) = loadFrustums minervaModel.data.features
                { m with minervaModel = minervaModel; frustums = frustums; trafo = trafo }

            | MinervaAction.HoverProducts hit ->
                let minervaModel = MinervaApp.update view m.minervaModel a
                let closestPoints = MinervaApp.queryClosestPoint m.minervaModel hit
                match closestPoints with
                | emptySeq when Seq.isEmpty emptySeq -> { m with minervaModel = minervaModel }
                | seq -> 
                    let index = seq |> Seq.map (fun (depth, pos, index) -> index) |> Seq.head
                    let closestID = m.minervaModel.selection.flatID.[index]

                    let hoveredFrustrum = (m.frustums.TryFind closestID)
                    {  m with minervaModel = minervaModel; hoveredFrustrum = hoveredFrustrum }

            | _ -> { m with minervaModel = MinervaApp.update view m.minervaModel a}

        | _ -> failwith "Not implemented yet"


    let view (m: MLinkingModel) =

        let sgFrustum' (f: LinkingFeature) =
            Sg.wireBox' f.color (Box3d(V3d.NNN,V3d.III))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.transform f.trafo

        let sgFrustum (f: IMod<LinkingFeature>) =
            Sg.wireBox (f |> Mod.map(fun f -> f.color)) (Mod.constant(Box3d(V3d.NNN,V3d.III)))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.trafo (f |> Mod.map(fun f -> f.trafo))

        let hoverFrustum =
            m.hoveredFrustrum
            |> Mod.map (fun f -> f |> (Option.defaultValue LinkingFeature.initial))
            |> sgFrustum

        let frustra =
            m.selectedFrustums
            |> AList.map sgFrustum'
            |> AList.toList
            |> Sg.ofList

        let scene = 
            Sg.ofArray [|
                frustra
                hoverFrustum
            |]
            |> Sg.trafo m.trafo

        Sg.ofArray [|
            scene
            MinervaApp.viewFeaturesSg m.minervaModel |> Sg.map MinervaAction
        |]