namespace OpcViewer.Base

module SgUtilities = 
    
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.SceneGraph

    // Color
    let colorAlpha (color:IMod<C4b>) (alpha:IMod<float>) : IMod<V4f> = 
        Mod.map2 (fun (c:C4b) a -> c.ToC4f() |> fun x -> C4f(x.R, x.G, x.B, float32 a).ToV4f()) color alpha

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
    let stablePoints (trafo: IMod<Trafo3d>) (positions: IMod<V3d[]>) : IMod<V3f[]> =
        positions 
        |> Mod.map2 (fun (t: Trafo3d) x -> 
            x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) trafo

    let stablePoints' (positions : IMod<V3d[]>) : (IMod<V3f[]> * IMod<Trafo3d>) = 
        let trafo =
            positions 
            |> Mod.map Array.tryHead 
            |> Mod.map (function | Some p -> Trafo3d.Translation p | None -> Trafo3d.Identity)

        stablePoints trafo positions, trafo

    let stableLines (close: bool) (trafo: IMod<Trafo3d>) (points: alist<V3d>) : IMod<Line3d[]> =
      points
        |> AList.toMod 
        |> Mod.map2 (fun (t:Trafo3d) l ->
            let list = l |> PList.map(fun x -> t.Backward.TransformPos x) |> PList.toList
            let head = list |> List.tryLast  
            match head with
                | Some h -> 
                    if close then list @ [h] else list
                        |> List.pairwise
                        |> List.map (fun (a,b) -> new Line3d(a,b))
                        |> List.toArray
                | None -> [||]) trafo

    // Old version of stableLines...TODO Orti is this behavior intended?
    //let edgeLines (close : bool) (points : alist<V3d>) (trafo:IMod<Trafo3d>) =
    //  points
    //    |> AList.map(fun d -> trafo.GetValue().Backward.TransformPos d)
    //    |> AList.toMod 
    //    |> Mod.map (fun l ->
    //        let list = PList.toList l
    //        let head = list |> List.tryHead
                
    //        match head with
    //            | Some h -> if close then list @ [h] else list
    //                            |> List.pairwise
    //                            |> List.map (fun (a,b) -> new Line3d(a,b))
    //                            |> List.toArray
    //            | None -> [||])   

    // Points
    let private drawPoints (pointSize: IMod<float>) (offset: IMod<float>) (pointsF: IMod<V3f[]>) =
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
        |> Sg.uniform "PointSize" pointSize
        |> Sg.uniform "DepthOffset" (offset |> Mod.map (fun depthWorld -> depthWorld / (100.0 - 0.1)))
        |> Sg.effect [
            Shader.PointSize.EffectPointTrafo
            toEffect DefaultSurfaces.pointSprite
            Shader.DepthOffset.Effect
            Shader.PointSize.EffectPointSpriteFragment
        ] 

    let drawSingleColorPoints (color: IMod<V4f>) (pointSize: IMod<float>) (offset: IMod<float>) (pointsF: IMod<V3f[]>) =
        drawPoints pointSize offset pointsF
        |> Sg.vertexBufferValue DefaultSemantic.Colors color

    let drawColoredPoints (colors: IMod<V4f[]>) (pointSize: IMod<float>) (offset: IMod<float>) (pointsF: IMod<V3f[]>) = 
        drawPoints pointSize offset pointsF
        |> Sg.vertexAttribute DefaultSemantic.Colors colors

    let drawPointList (color: IMod<C4b>) (pointSize: IMod<float>) (offset: IMod<float>) (positions: alist<V3d>) = 
        let positions = positions |> AList.toMod |> Mod.map PList.toArray
        let shiftedPoints, shiftTrafo  = stablePoints' positions

        drawSingleColorPoints (color |> Mod.map(fun x -> x.ToC4f().ToV4f())) pointSize offset shiftedPoints
        |> Sg.trafo shiftTrafo
                     
    let lines (offset: IMod<float>) (color: IMod<C4b>) (width: IMod<float>) (trafo: IMod<Trafo3d>) (points: alist<V3d>) = 
        let edges = stableLines false trafo points // refactored edgeLine
        edges
        |> Sg.lines color
        |> Sg.uniform "LineWidth" width
        |> Sg.uniform "DepthOffset" (offset |> Mod.map (fun depthWorld -> depthWorld / (100.0 - 0.1))) 
        |> Sg.effect [
            Shader.StableTrafo.Effect
            Shader.ThickLineNew.Effect
            Shader.DepthOffset.Effect
        ]
        |> Sg.trafo trafo

    let lines' (offset: IMod<float>) (color: IMod<C4b>) (width: IMod<float>) (points: alist<V3d>) =
        let trafo =
            points 
            |> AList.toMod
            |> Mod.map (fun l -> 
               match l |> PList.tryFirst with
               | Some p -> Trafo3d.Translation p
               | None -> Trafo3d.Identity)
        lines offset color width trafo points
                               
    let scaledLines (color: IMod<C4b>) (width: IMod<float>) (trafo: IMod<Trafo3d>) (points: alist<V3d>) = 
        let edges = stableLines false trafo points
        let size = edges |> Mod.map (fun line -> (float line.Length) * 100.0)                                             
        
        edges
        |> Sg.lines color
        |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" size
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.ThickLineNew.Effect
        ]                               
        |> Sg.trafo trafo
        |> Sg.uniform "LineWidth" width  

    let scaledLines' (color: IMod<C4b>) (width: IMod<float>) (points: alist<V3d>) =
        let trafo =
            points 
            |> AList.toMod
            |> Mod.map (fun l -> 
               match l |> PList.tryFirst with
               | Some p -> Trafo3d.Translation p
               | None -> Trafo3d.Identity)
        scaledLines color width trafo points

    // Geometries
    let discISg (color: IMod<C4b>) (size: IMod<float>) (height: IMod<float>) (trafo: IMod<Trafo3d>) =
        Sg.cylinder 30 color size height              
        |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" size
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.StableLight.Effect
        ]
        |> Sg.trafo trafo

    let coneISg (color: IMod<C4b>) (radius: IMod<float>) (height: IMod<float>) (trafo: IMod<Trafo3d>) =  
        Sg.cone 30 color radius height
        |> Sg.effect [
            Shader.StableTrafo.Effect
            toEffect DefaultSurfaces.vertexColor
            Shader.StableLight.Effect
        ]                   
        |> Sg.trafo trafo
