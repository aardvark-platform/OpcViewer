namespace OpcSelectionViewer

open Aardvark.Base  
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.UI
open OpcViewer.Base

open Adaptify.FSharp.Core

module AxisSg =

    let toColoredEdges (offset:V3d) (color : C4b) (points : array<V3d>) =
        points
          |> Array.map (fun x -> x-offset)
          |> Array.pairwise
          |> Array.map (fun (a,b) -> (new Line3d(a,b), color))
    
    let drawColoredEdges width edges = 
        edges
            |> IndexedGeometryPrimitives.lines
            |> Sg.ofIndexedGeometry
            |> Sg.effect [
                Shader.StableTrafo.Effect
                toEffect DefaultSurfaces.vertexColor
                Shader.ThickLineNew.Effect //toEffect DefaultSurfaces.thickLine
            ]
            |> Sg.uniform "LineWidth" (AVal.constant width)
            |> Sg.uniform "DepthOffset" (AVal.constant 0.1)  // TODO refactor this whole file....using the OPCViewer.Base.SgUtilities

    let lines (color : C4b) (width : double)  (points : V3d[]) =
        let offset =
            match points |> Array.tryHead with
            | Some h -> h
            | None -> V3d.Zero

        points 
            |> toColoredEdges offset color
            |> drawColoredEdges width
            |> Sg.trafo (offset |> Trafo3d.Translation |> AVal.constant)
  
    let sphere color size pos =
        let trafo = 
          pos |> AVal.map (fun (x : V3d) -> Trafo3d.Translation x)

        Sg.sphere 3 (AVal.constant color) (AVal.constant size)
          |> Sg.noEvents
          |> Sg.trafo trafo
          |> Sg.uniform "WorldPos" (trafo |> AVal.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
          |> Sg.uniform "Size" (AVal.constant(size))
          |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
          ]

    let createAxisSg (positions : List<V3d>) =
        positions |> Array.ofList |> lines C4b.VRVisGreen 2.0
    
    let addDebuggingAxisPointSphere (selectionPos : Option<V3d>) = 
        selectionPos |> 
          Option.map(fun pos -> AVal.constant pos |> sphere C4b.VRVisGreen 0.1)
            |> Option.defaultValue Sg.empty

    let axisSgs (model : AdaptiveModel) = 
        aset {
            let! axis = model.axis
      
            match axis with
            | AdaptiveSome a -> 
                let! positions = a.positions
                yield createAxisSg positions
          
                let! selectionPos = a.selectionOnAxis
                yield addDebuggingAxisPointSphere selectionPos
            | AdaptiveNone -> yield Sg.empty
        } |> Sg.set
