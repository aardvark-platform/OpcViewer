namespace OpcViewer.Base

open Aardvark.Base

module PlaneFitting =
    open Uncodium

    let planeFit (points:seq<V3d>) : Plane3d =
        let length = points |> Seq.length |> float

        let c = 
            let sum = points |> Seq.reduce (fun x y -> V3d.Add(x,y))
            sum / length

        let pDiffAvg = points |> Seq.map(fun x -> x - c)
        
        //let mutable matrix = M33d.Zero
        //pDiffAvg |> Seq.iter(fun x -> matrix.AddOuterProduct(&x))
        let mutable matrix = M33d.Zero
        pDiffAvg |> Seq.iter(fun x -> 
          let mutable y = x //.ToV3f()
          (&matrix).AddOuterProduct(&y))

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

        Plane3d(n, c)