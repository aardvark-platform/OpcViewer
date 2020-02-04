namespace OpcViewer.Base

module SgUtilities = 
    
    open Aardvark.Base
    open FSharp.Data.Adaptive
    open Aardvark.Base.Rendering
    open Aardvark.SceneGraph

    // Color
    let colorAlpha (color:aval<C4b>) (alpha:aval<float>) : aval<V4f> = 
        AVal.map2 (fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) color alpha

    let createSecondaryColor (c: C4b) : C4b = 
        let primary = c.ToC3f().ToHSVf()
           
        let v = 
          if (primary.V > 0.5f) then 
            primary.V - 0.25f * primary.V
          else 
            primary.V + 0.25f * primary.V
        
        let secondary = HSVf(primary.H, primary.S, v).ToC3f().ToC3b()
        let secondary = C4b(secondary, c.A)
                                   
        secondary

    // Improve numerical stabilty
    let stablePoints (trafo: aval<Trafo3d>) (positions: aval<V3d[]>) : aval<V3f[]> =
        positions 
        |> AVal.map2 (fun (t: Trafo3d) x -> 
            x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) trafo

    let stablePoints' (positions : aval<V3d[]>) : (aval<V3f[]> * aval<Trafo3d>) = 
        let trafo =
            positions 
            |> AVal.map Array.tryHead 
            |> AVal.map (function | Some p -> Trafo3d.Translation p | None -> Trafo3d.Identity)

        stablePoints trafo positions, trafo

    let stableLines (close: bool) (trafo: aval<Trafo3d>) (points: alist<V3d>) : aval<Line3d[]> =
      points
        |> AList.toAVal 
        |> AVal.map2 (fun (t:Trafo3d) l ->
            let list = l |> IndexList.map(fun x -> t.Backward.TransformPos x) |> IndexList.toList
            let head = list |> List.tryLast  
            match head with
                | Some h -> 
                    if close then list @ [h] else list
                        |> List.pairwise
                        |> List.map (fun (a,b) -> new Line3d(a,b))
                        |> List.toArray
                | None -> [||]) trafo

    // Old version of stableLines...TODO Orti is this behavior intended?
    //let edgeLines (close : bool) (points : alist<V3d>) (trafo:aval<Trafo3d>) =
    //  points
    //    |> AList.map(fun d -> trafo.GetValue().Backward.TransformPos d)
    //    |> AList.toMod 
    //    |> AVal.map (fun l ->
    //        let list = IndexList.toList l
    //        let head = list |> List.tryHead              
    //        match head with
    //            | Some h -> if close then list @ [h] else list
    //                            |> List.pairwise
    //                            |> List.map (fun (a,b) -> new Line3d(a,b))
    //                            |> List.toArray
    //            | None -> [||])   

    // Points
    let private drawPoints (pointSize: aval<float>) (offsetNormalized: aval<float>) (pointsF: aval<V3f[]>) =
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
        |> Sg.uniform "PointSize" pointSize
        |> Sg.uniform "DepthOffset" offsetNormalized
        |> Sg.effect [
            Shader.PointSize.EffectPointTrafo
            toEffect DefaultSurfaces.pointSprite
            Shader.DepthOffset.Effect
            Shader.PointSize.EffectPointSpriteFragment
        ] 
        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])

    let drawSingleColorPoints (color: aval<V4f>) (pointSize: aval<float>) (offsetNormalized: aval<float>) (pointsF: aval<V3f[]>) =
        drawPoints pointSize offsetNormalized pointsF
        |> Sg.vertexBufferValue DefaultSemantic.Colors color

    let drawColoredPoints (colors: aval<V4f[]>) (pointSize: aval<float>) (offsetNormalized: aval<float>) (pointsF: aval<V3f[]>) = 
        drawPoints pointSize offsetNormalized pointsF
        |> Sg.vertexAttribute DefaultSemantic.Colors colors

    let drawPointList (color: aval<C4b>) (pointSize: aval<float>) (offsetWorld: aval<float>) (near: aval<float>) (far: aval<float>)  (positions: alist<V3d>) = 
        let nearFar = AVal.map2 (fun near far -> near,far) near far
        let offsetNormalized = AVal.map2 (fun (near, far) offsetWorld -> offsetWorld / (far - near)) nearFar offsetWorld
        let positions = positions |> AList.toAVal |> AVal.map IndexList.toArray
        let shiftedPoints, shiftTrafo  = stablePoints' positions

        drawSingleColorPoints (color |> AVal.map(fun x -> x.ToC4f().ToV4f())) pointSize offsetNormalized shiftedPoints
        |> Sg.trafo shiftTrafo
                     
    let lines (offsetNormalized: aval<float>) (color: aval<C4b>) (width: aval<float>) (trafo: aval<Trafo3d>) (points: alist<V3d>) = 
        let edges = stableLines false trafo points // refactored edgeLine
        edges
        |> Sg.lines color
        |> Sg.uniform "LineWidth" width
        |> Sg.uniform "DepthOffset" offsetNormalized
        |> Sg.effect [
            Shader.StableTrafo.Effect
            Shader.ThickLineNew.Effect
            Shader.DepthOffset.Effect
        ]
        |> Sg.trafo trafo
        |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])

    let lines' (offsetWorld: aval<float>) (color: aval<C4b>) (width: aval<float>) (near: aval<float>) (far: aval<float>) (points: alist<V3d>) =
        let nearFar = AVal.map2 (fun near far -> near,far) near far
        let offsetNormalized = AVal.map2 (fun (near, far) offsetWorld -> offsetWorld / (far - near)) nearFar offsetWorld
        
        let trafo =
            points 
            |> AList.toAVal
            |> AVal.map (fun l -> 
               match l |> IndexList.tryFirst with
               | Some p -> Trafo3d.Translation p
               | None -> Trafo3d.Identity)
        lines offsetNormalized color width trafo points
                               
    let scaledLines (color: aval<C4b>) (width: aval<float>) (trafo: aval<Trafo3d>) (points: alist<V3d>) = 
        let edges = stableLines false trafo points
        let size = edges |> AVal.map (fun line -> (float line.Length) * 100.0)                                             
        
        edges
        |> Sg.lines color
        |> Sg.uniform "WorldPos" (trafo |> AVal.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" size
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.ThickLineNew.Effect
        ]                               
        |> Sg.trafo trafo
        |> Sg.uniform "LineWidth" width  

    let scaledLines' (color: aval<C4b>) (width: aval<float>) (points: alist<V3d>) =
        let trafo =
            points 
            |> AList.toAVal
            |> AVal.map (fun l -> 
               match l |> IndexList.tryFirst with
               | Some p -> Trafo3d.Translation p
               | None -> Trafo3d.Identity)
        scaledLines color width trafo points

    // Geometries
    let discISg (color: aval<C4b>) (size: aval<float>) (height: aval<float>) (trafo: aval<Trafo3d>) =
        Sg.cylinder 30 color size height              
        |> Sg.uniform "WorldPos" (trafo |> AVal.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" size
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.StableLight.Effect
        ]
        |> Sg.trafo trafo

    let coneISg (color: aval<C4b>) (radius: aval<float>) (height: aval<float>) (trafo: aval<Trafo3d>) =  
        Sg.cone 30 color radius height
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.StableLight.Effect
        ]                   
        |> Sg.trafo trafo
