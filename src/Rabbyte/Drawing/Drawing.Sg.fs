﻿namespace Rabbyte.Drawing
 
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI

open OpcViewer.Base

open DrawingModel

module DrawingSg = 

    let convertLines close points = 
        points 
        |> AList.toMod
        |> Mod.map(fun k -> 
        let k = k |> PList.toList 
        match k  with
            | h::_ -> 
                let start = if close then k @ [h] else k
                start 
                    |> List.pairwise 
                    |> List.map (fun (a,b) -> new Line3d(a,b)) 
                    |> Array.ofList                                                        
            | _ -> [||])

    let discISg color size height trafo =
        Sg.cylinder 30 color size height              
            |> Sg.noEvents
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            |> Sg.uniform "Size" size
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.StableLight.Effect
            ]
            |> Sg.trafo(trafo)

    let coneISg color radius height trafo =  
        Sg.cone 30 color radius height
            |> Sg.noEvents
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.StableLight.Effect
            ]                   
            |> Sg.trafo(trafo)

    let lineISg color lineWidth trafo segments = 
        Sg.lines color segments
            |> Sg.noEvents
            |> Sg.uniform "LineWidth" lineWidth 
            |> Sg.uniform "depthOffset" (Mod.constant 1.0)
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.ThickLineNew.Effect
            ]
            |> Sg.trafo trafo

    let sphereISg color radius position =
        let trafo = Mod.constant (Trafo3d.Translation position)
        Sg.sphere 3 color radius 
            |> Sg.noEvents
            |> Sg.trafo trafo
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            //|> Sg.uniform "Size" radius // 5.0
            //|> Sg.effect [
            //    Shader.ScreenSpaceScale.Effect
            //    Shader.StableTrafo.Effect
            //    toEffect DefaultSurfaces.vertexColor
            //]

    let drawVertices (m : MDrawingModel) = 
        alist {
            for p in m.points do
                yield sphereISg (m.style.primary.c) (Mod.constant 1.0) p
            for s in m.segments do
                for p in s.points do
                    yield sphereISg (m.style.secondary.c) (Mod.constant 0.5) p
        }
        |> ASet.ofAList 
        |> Sg.set
        |> Sg.uniform "Size" (Mod.constant(5.0))
        |> Sg.effect [
            Shader.ScreenSpaceScale.Effect
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
        ]

    let drawSegment (m :MDrawingModel) = 
        // CAREFUL! duplicated vertices!!! most likely additional edges between segments (startNode is also Endnode)
        let segments = m.segments |> AList.map (fun x -> x.points |> AList.ofPList) |> AList.concat 
        let lines = convertLines false segments
        
        let color = m.style.secondary.c
        let lineWidth = m.style.thickness |> Mod.map (fun x -> x * 0.8)
        let trafo = Mod.constant (Trafo3d.Identity)
        lineISg color lineWidth trafo lines

    let drawLines (m : MDrawingModel) = 
        let lines = convertLines false m.points

        let color = m.style.primary.c
        let lineWidth = m.style.thickness
        let trafo = Mod.constant (Trafo3d.Identity)
        lineISg color lineWidth trafo lines