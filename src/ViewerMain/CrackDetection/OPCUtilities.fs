namespace CrackDetection

open Aardvark.Base
open Aardvark.VRVis.Opc.KdTrees
open Aardvark.SceneGraph.Opc
open OpcViewer.Base
open OpcViewer.Base.Picking

#nowarn "0686"

[<AutoOpen>]
module OPCUtilities =

    module TriangleHit =

        let inline private transform (f : int -> ^a) (g : ^a -> float -> ^a) (hit : TriangleHit) =
            let bc = hit.barycentricCoords
            let idx = hit.indices

            g (f idx.X) bc.X +
            g (f idx.Y) bc.Y +
            g (f idx.Z) bc.Z

        /// Uses indices and barycentric coordiantes to lookup
        /// a V3d from an attribute array
        let getV3d (data : V3d []) =
            transform (fun i -> data.[i]) (*)

        /// Converts the hit point into a 2D coordinate
        /// in the range of [0, size].
        let toV2i (size : V2i) (hit : TriangleHit) =
            let v =
                hit |> transform (fun index ->
                    V2d (index % size.X, index / size.X)
                ) (*)

            V2i (v.Round ())

    module KdTree =

        /// Loads the SvBR map of the given kd tree
        let loadSvBRMap (kd : LazyKdTree) =
            kd.positions2dPath |> fromFile<V3f> |> Matrix.ofVolume |> Matrix.map V3d

    module Patch =

        let intersects (b : Box2d) (p : Patch) =
            b.Intersects (p.info.GlobalBoundingBox2d)

        let loadMap (rootPath : string) (fileName : string) (patch : Patch) =
            let path = rootPath +/ patch.info.Name +/ fileName
            path |> fromFile<'a> |> Matrix.ofVolume

        let loadPositionsMap (rootPath : string) (patch : Patch) =
            patch
            |> loadMap<V3f> rootPath patch.info.Positions
            |> Matrix.map (V3d >> patch.info.Local2Global.Forward.TransformPos)

        let loadEdgeMap (rootPath : string) (patch : Patch) =
            try
                patch
                |> loadMap<float> rootPath "EdgeMap.aara"
            with
            | _ ->
                Log.error "Could not find edge map, generating random data..."
                let rnd = RandomSystem()
                let posMap = patch |> loadPositionsMap rootPath
                let edgeMap = Matrix<float> posMap.Dim
                edgeMap.SetByIndex(fun _ -> rnd.UniformDouble() * 255.0)

    module QTree =

        type private Patch with
            member x.Intersects (b : Box2d) =
                x |> Patch.intersects b

        /// Finds leaf patches that intersect with
        /// the given bounding box
        let intersect (bb : Box2d) (tree : QTree<Patch>) =
            let rec check (t : QTree<Patch>) =
                match t with
                | QTree.Node (p, c) when p.Intersects bb ->
                    c |> Array.map check |> List.concat
                | QTree.Leaf p when p.Intersects bb ->
                    [ p ]
                | _ -> []

            check tree
