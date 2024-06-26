﻿namespace OpcViewer.Base

open Aardvark.Base

module PlaneFitting =
    open Uncodium

    let private calculateEigenVector (points: seq<V3d>) = 
        let length = points |> Seq.length |> float

        let c = 
            let sum = points |> Seq.sum
            sum / length

        let pDiffAvg = points |> Seq.map (fun x -> x - c)
        
        //let mutable matrix = M33d.Zero
        //pDiffAvg |> Seq.iter(fun x -> matrix.AddOuterProduct(&x))
        let mutable matrix = M33d.Zero
        pDiffAvg |> Seq.iter(fun x -> 
            let mutable y = x //.ToV3f()
            matrix.AddOuterProduct(&y))

        matrix <- matrix / length
         
        let mutable q = M33d.Zero
        let mutable w = V3d.Zero
        let passed = Eigensystems.Dsyevh3(&matrix, &q, &w)
        
        let n = 
            if w.X < w.Y then
                if w.X < w.Z then q.C0
                else q.C2
            else if w.Y < w.Z then q.C1
            else q.C2

        (n,c)

    let planeFit (points: seq<V3d>) : Plane3d =
        let n,c = points |> calculateEigenVector 
        Plane3d(n, c)

    let lineFit (points: seq<V3d>) : Line3d =
        let n,c = points |> calculateEigenVector 
        Line3d(c - n, c + n)