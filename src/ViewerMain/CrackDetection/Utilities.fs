namespace CrackDetection

open System
open Aardvark.Base

[<AutoOpen>]
module Utilties =

    [<AutoOpen>]
    module BoxExtensions =

        type Box2d with
            static member ofBox3d (b : Box3d) =
                Box2d (b.Min.XY, b.Max.XY)

            member x.Intersects (b : Box3d) =
                b |> Box2d.ofBox3d |> x.Intersects

    module Matrix =
        let private lerp =
            Func<float, V3d, V3d, V3d>(
                fun t a b -> VecFun.Lerp(t, a, b)
            )

        let size (m : Matrix<'a>) =
            m.Dim |> V2i

        let ofVolume (v : Volume<'a>) =
            v.SubXYMatrix 0L

        let subMatrix (origin : V2i) (size : V2i) (m : Matrix<'a>) =
            m.SubMatrix(origin, size)

        let toPixImage (matrix : Matrix<C3b>) =
            TensorExtensions.ToPixImage<byte> matrix

        let map (map : 'a -> 'b) (input : Matrix<'a>) : Matrix<'b> =
            let array = input.Array.ToArrayOfT<'a>() |> Array.map map
            Matrix (array, input.Size)

        let get (position : V2i) (matrix : Matrix<V3d>) =
            matrix.[position]

        let sample (uv : V2d) (matrix : Matrix<V3d>) =
            let size = V2d (matrix.Dim.XY - V2l.II)
            let texel = V2d (uv.X, uv.Y) * size

            matrix.Sample4Clamped(texel, lerp, lerp)

    module Array =

        /// Clusters an array so that for each cluster, the
        /// supplied function returns true for each pair of elements
        let cluster (f : 'a -> 'a -> bool) (s : 'a []) =
            let single x =
                [|[| x |]|]

            let add (clusters : 'a [][]) (value : 'a) =
                let ret = clusters |> Array.tryFindIndex (Array.forall (f value))
                match ret with
                | Some i ->
                    clusters.[i] <- [| value |] |> Array.append clusters.[i]
                    clusters
                | None ->
                    value |> single |> Array.append clusters

            s |> Array.fold add Array.empty

        /// Converts an array of arrays to a 2D array.
        /// Missing elements (i.e. when the arrays are not of equal length) are set to None.
        let toArray2d (arr : 'a [][]) =
            let w = arr.Length
            let h = arr |> Array.fold (fun l x -> x.Length |> max l) 0

            Array2D.init w h (fun x y ->
                if y < h then
                    Some arr.[x].[y]
                else
                    None
            )

    module Array2D =

        let toSeq (arr : 'a [,]) =
            seq {
                for x in 0 .. (Array2D.length1 arr) - 1 do
                    for y in 0 .. (Array2D.length2 arr) - 1 do
                        yield arr.[x, y]
            }

        let row (i : int) (arr : 'a [,]) =
            seq { for x in 0 .. (Array2D.length1 arr) - 1 do arr.[x, i] }

        let col (i : int) (arr : 'a [,]) =
            seq { for y in 0 .. (Array2D.length2 arr) - 1 do arr.[i, y] }

        let rows (arr : 'a [,]) =
            seq { for i in 0 .. (Array2D.length2 arr) - 1 do arr |> row i }

        let cols (arr : 'a [,]) =
            seq { for i in 0 .. (Array2D.length1 arr) - 1 do arr |> col i }

        let get (p : V2i) (arr : 'a [,]) =
            arr.[p.X, p.Y]

        let size (arr : 'a [,]) =
            V2i (Array2D.length1 arr, Array2D.length2 arr)

        let iteri' (f : V2i -> 'a -> unit) =
            Array2D.iteri (fun x y elem -> elem |> f (V2i (x, y)))

        let subarray (min : V2i) (max : V2i) (arr : 'a [,]) =
            let size = max - min + V2i.One
            let rs = Array2D.zeroCreate size.X size.Y
            Array2D.blit arr min.X min.Y rs 0 0 size.X size.Y
            rs

        let tryFindIndexi (f : V2i -> 'a -> bool) (arr : 'a [,]) =
            let size = size arr

            let rec find (p : V2i) =
                if arr |> get p |> f p then
                    Some p
                else
                    if p.X < size.X - 1 then
                        find <| V2i (p.X + 1, p.Y)
                    else if p.Y < size.Y - 1 then
                        find <| V2i (0, p.Y + 1)
                    else
                        None

            find V2i.Zero

        /// Scans rows and columns of array
        let scan (fx : 's -> 'a -> 's) (fy : 's -> 'a -> 's) (combine : 's -> 's -> 's)
                 (state : 's) (arr : 'a [,]) =

            let rs = Array2D.zeroCreate<'s> (Array2D.length1 arr) (Array2D.length2 arr)

            for y in 0 .. (Array2D.length2 arr) - 1 do
                let mutable curr = state
                for x in 0 .. (Array2D.length1 arr) - 1 do
                    rs.[x, y] <- curr
                    curr <- fx curr arr.[x, y]

            for x in 0 .. (Array2D.length1 arr) - 1 do
                let mutable curr = combine rs.[x, 0] state
                for y in 0 .. (Array2D.length2 arr) - 1 do
                    rs.[x, y] <- curr
                    curr <- combine rs.[x, y] (fy curr arr.[x, y])

            rs

        /// Computes partial sums
        let partialSums (f : 'a -> V2i) (arr : 'a [,]) =
            let combine (a : V2i) (b : V2i) =
                V2i (a.X, b.Y)

            let fx (s : V2i) (x : 'a) =
                V2i (s.X + (f x).X, s.Y)

            let fy (s : V2i) (x : 'a) =
                V2i (s.X, s.Y + (f x).Y)

            arr |> scan fx fy combine V2i.Zero