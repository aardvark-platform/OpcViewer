namespace Rabbyte.Drawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives

open DrawingModel
open OpcViewer.Base
open FShade.Primitives

module DrawingApp =

    let private finalPrimitiveType close model  = 
        let primitiveType = 
            match model.points |> PList.count, close with
            | 0, _ -> PrimitiveType.Empty
            | 1, _ -> PrimitiveType.Point
            | 2, _ -> PrimitiveType.Line
            | _, false -> PrimitiveType.PolyLine
            | _, true -> PrimitiveType.Polygon

        { model with primitiveType = primitiveType}

    let private syncPrimType model =
        finalPrimitiveType false model

    let rec update (model: DrawingModel) (act: DrawingAction) =
        match act with
        | ChangeColorPrimary c1 -> 
            { model with style = { model.style with primary = ColorPicker.update model.style.primary c1 }}
        | ChangeColorSecondary c2 -> 
            { model with style = { model.style with secondary = ColorPicker.update model.style.secondary c2 }}
        | ChangeColorAuto c -> 
            let primary = ColorPicker.update model.style.primary c
            let secCol = (SgUtilities.createSecondaryColor primary.c)
            let secondary = ColorPicker.update model.style.secondary (ColorPicker.Action.SetColor { c = secCol })
            { model with style = { model.style with primary = primary; secondary = secondary}}
        | ChangeThickness th ->
            { model with style = { model.style with thickness = th }}
        | ChangeSamplingRate sr ->
            { model with style = { model.style with samplingRate = sr }}
        | ChangeLineStyle l ->
            { model with style = { model.style with lineStyle = l }}
        | ChangeAreaStyle a ->
            { model with style = { model.style with areaStyle = a }}
        | Undo _ -> 
            match model.past with
            | None -> model
            | Some p -> { p with future = Some model }
        | Redo _ -> 
            match model.future with
            | None -> model
            | Some f -> f
        // Undo-Able Commands
        | RecalculateSegments hitF -> failwith "" // TODO?
        | AddPoint (p, hitF) -> 
            match model.points |> PList.isEmpty with
            | true -> 
                syncPrimType { model with points = model.points |> PList.prepend p; past = Some model}
            | false ->
                let startP = p
                let endP = model.points |> PList.first 
                let innerSegPoints = 
                    match hitF with
                    | Some f -> 
                        let vec  = (endP - startP)
                        let dir  = vec.Normalized
                        let l    = vec.Length
        
                        [ model.style.samplingRate .. model.style.samplingRate .. l-model.style.samplingRate] 
                        |> List.map(fun x -> startP + x * dir)
                        |> List.map f
                        |> List.choose id
                        |> PList.ofList
                    | None -> PList.empty

                let segment = {
                    startPoint = startP
                    endPoint = endP
                    innerPoints = innerSegPoints
                }
                syncPrimType { model with points = model.points |> PList.prepend p; segments = model.segments |> PList.prepend segment; past = Some model}
        //| AddTestBrushes pointsOnAxisFunc ->
        //  if model.intersectionPoints.Count > 0 then
        //    let rand = RandomSystem()
        //    let colors =
        //      [|
        //        for _ in 1 .. 20 do
        //          yield rand.UniformC3f().ToC4b()
        //      |]
        //    let testBrushes = 
        //      Seq.initInfinite (fun _ ->
        //        let mutable dir = rand.UniformV3dDirection()
        //        if dir.Z < 0.0 then dir.Z <- -dir.Z
        //        let p0 = (model.intersectionPoints.[rand.UniformInt(model.intersectionPoints.Count)])
        //        match pointsOnAxisFunc (PList.single p0) with
        //        | Some center ->
        //          let center = center.midPoint
        //          let dir = p0 - center |> Vec.normalize
        //          let t = Trafo3d.FromNormalFrame(p0, dir)
        //          let o = rand.UniformV2dDirection() * rand.UniformDouble()
        //          let p1 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()
        //          let p2 = o + rand.UniformV2dDirection() * 0.5 * rand.UniformDouble()
        //          let p0 = t.Forward.TransformPos(V3d(o, 0.0))
        //          let p1 = t.Forward.TransformPos(V3d(p1, 0.0))
        //          let p2 = t.Forward.TransformPos(V3d(p2, 0.0))
        //          let pts = PList.ofList [ p0; p1; p2; p0]
        //          match pointsOnAxisFunc (pts |> PList.skip 1) with
        //          | Some aps ->
        //            Some { 
        //              // 1m shift for scene with axis outside of tunnel....REMOVE
        //              pointsOnAxis = Some aps
        //              points = pts
        //              segments = PList.empty
        //              color = colors.[rand.UniformInt colors.Length]
        //            }
        //          | None ->
        //            None
        //        | _ ->
        //          None)
        //      |> Seq.choose id
        //      |> Seq.take 200
        //      |> PList.ofSeq
        //    let newGrouped =
        //      testBrushes |> Seq.fold (fun groupedBrushes newBrush -> 
        //        groupedBrushes|> HMap.alter newBrush.color (fun x -> 
        //          match x with 
        //          | Some y -> Some (y |> PList.append newBrush)
        //          | None -> Some (PList.single newBrush))
        //       ) model.groupedBrushes
        //    { model with brush = model.brush |> PList.concat2 testBrushes; intersectionPoints = PList.empty; segments = PList.empty; groupedBrushes = newGrouped }
        //  else 
        //    model
        | RemoveLastPoint ->
            match model.points |> PList.count with
            | 0 -> model
            | 1 -> syncPrimType { model with points = model.points |> PList.skip 1; past = Some model}
            | _ -> syncPrimType { model with points = model.points |> PList.skip 1; segments = model.segments |> PList.skip 1; past = Some model }
        | DrawingAction.Clear -> 
            syncPrimType { model with points = PList.empty; segments = PList.empty; past = Some model}
        | Finish -> 
            let m = finalPrimitiveType false model 
            { m with past = Some model}
        | FinishClose hitF -> 
            match model.primitiveType with
            | PolyLine -> 
                let newM = update model (AddPoint ((model.points |> PList.last), hitF))
                let updateType = finalPrimitiveType true newM
                // TODO -> FIX WINDING ORDER FOR GROUPED ANNOTATIONS!
                //    let p, pa = 
                //      // Fix winding order (if axis is available!)
                //      match pa with
                //      | None -> (p,pa)
                //      | Some paa -> 
                //        // for higher Precision shift by AxisPoint
                //        let axisPoint = paa.pointsOnAxis |> PList.skip 1 |> PList.first
                //        let p0 = p |> PList.first                  |> fun x -> x - axisPoint
                //        let p1 = p |> PList.skip 1 |> PList.first  |> fun x -> x - axisPoint
                //        let p2 = p |> PList.skip 2 |> PList.first  |> fun x -> x - axisPoint

                //        let dir1 = p1.Normalized  // already shifted by axisPoint
                //        let x1 = (p0-p1).Normalized
                //        let x2 = (p2-p1).Normalized
                //        let dir2 = (x1.Cross(x2)).Normalized

                //        if dir1.Dot(dir2) |> sign < 0 then
                //          let pRev = p |> PList.toList |> List.rev |> PList.ofList
                //          let aRev = { paa with pointsOnAxis = paa.pointsOnAxis |> PList.toList |> List.rev |> PList.ofList }
                //          printfn "\n\n\nFixed winding order \n\n\n"
                //          (pRev, Some aRev)
                //        else 
                //          (p,pa)

                { updateType with past = Some model }
            | _ -> { model with past = Some model }

    let allSegmentPoints (segments: alist<Segment>) : alist<V3d> = 
        let lastPoint = 
            segments 
            |> AList.toMod
            |> Mod.map (fun x -> 
                x 
                |> PList.tryLast 
                |> Option.map (fun x -> x.endPoint |> PList.single)
                |> Option.defaultValue PList.empty)
            |> AList.ofMod

        let allButLast = 
            segments 
            |> AList.map (fun x -> x.innerPoints |> PList.prepend x.startPoint |> AList.ofPList) 
            |> AList.concat

        AList.append allButLast lastPoint


    let drawContourWithPointSize (points: alist<V3d>) (segments: alist<Segment>) (style: MBrushStyle) (near: IMod<float>) (far: IMod<float>) (pointSize: IMod<float>) (depthOffset : IMod<float>)=  

        let pointsSg = 
            points 
            |> SgUtilities.drawPointList style.primary.c pointSize depthOffset near far

        let pointsInnerSg = 
            segments
            |> AList.map (fun x -> x.innerPoints |> AList.ofPList) 
            |> AList.concat 
            |> SgUtilities.drawPointList (style.primary.c |> Mod.map (fun c -> SgUtilities.createSecondaryColor c)) (pointSize |> Mod.map (fun x -> x * 0.8)) depthOffset near far

        let edgesSg = 
            let lineWidth = style.thickness |> Mod.map (fun x -> x * 1.1)
            let sPoints = allSegmentPoints segments
            sPoints
            |> SgUtilities.lines' (depthOffset|> Mod.map (fun x -> x / 1.9)) style.secondary.c lineWidth near far

        let edgesDirectSg = 
            points 
            |> SgUtilities.lines' (depthOffset|> Mod.map (fun x -> x / 2.0)) style.primary.c style.thickness near far
        
        // drawing order does not fix overlappings (offset in worldspace could fix this...)
        //let edgesSg = [edges; edgesDirect] |> Sg.group |> Sg.noEvents |> Sg.pass RenderPass.main
        //let pointsSg = [points; pointsInner] |> Sg.group |> Sg.noEvents |> Sg.pass (RenderPass.after "points" RenderPassOrder.Arbitrary RenderPass.main)
        //[pointsSg; edgesSg] |> Sg.ofList
        
        //[edgesSg; edgesDirectSg; pointsSg; pointsInnerSg] |> Sg.group
        pointsSg

    let drawContour (points: alist<V3d>) (segments: alist<Segment>) (style: MBrushStyle) (near: IMod<float>) (far: IMod<float>) =  
        drawContourWithPointSize points segments style near far (Mod.constant 4.0) (Mod.constant 0.1)

    let view (near: IMod<float>) (far: IMod<float>) (model: MDrawingModel)  = 
        drawContourWithPointSize model.points model.segments model.style near far (Mod.constant 10.0) (Mod.constant 0.1) |> Sg.noEvents

    let viewPointSize (near: IMod<float>) (far: IMod<float>) (pointSize: IMod<float>) (depthOffset: IMod<float>) (model: MDrawingModel) = 
        drawContourWithPointSize model.points model.segments model.style near far pointSize depthOffset |> Sg.noEvents

    let viewGui (model: MDrawingModel) = 
        
        let style' = "color: white; font-family:Consolas;"

        table [clazz "item"] [
            tr[][
                td[style style'][text "Type:"]
                td[style style'][
                    Incremental.text (model.primitiveType |> Mod.map (fun x -> 
                        match x with
                        | PrimitiveType.Empty -> "Empty"
                        | PrimitiveType.Point -> "Point"
                        | PrimitiveType.Line -> "Line"
                        | PrimitiveType.PolyLine -> "PolyLine"
                        | PrimitiveType.Polygon -> "Polygon"    
                        ))]
            ]
            tr[][
                td[style style'][text "Thickness:"]
                td[style style'][numeric { min = 1.0; max = 8.0; smallStep = 0.5; largeStep= 1.0 } [clazz "ui inverted input"] (model.style.thickness) ChangeThickness]
                //td[style style'][slider { min = 1.0; max = 8.0; step = 1.0 } [clazz "ui inverted red slider"] (model.style.thickness) ChangeThickness]
            ]
            tr[][
                td[style style'][text "SamplingRate:"]
                td[style style'][numeric { min = 0.02; max = 10.0; smallStep = 0.02; largeStep= 0.2 } [clazz "ui inverted input"] (model.style.samplingRate) ChangeSamplingRate]
                //td[style style'][slider { min = 0.02; max = 10.0; step = 0.2 } [clazz "ui inverted red slider"] (model.style.samplingRate) ChangeSamplingRate]
            ]
            tr[][
                td[style style'][text "AreaStyle:"]
                td[style style'][dropdown { placeholder = "AreaStyle"; allowEmpty = false } [ clazz "ui inverted selection dropdown" ] (model.areaStyleNames |> AMap.map (fun k v -> text v)) (model.style.areaStyle) ChangeAreaStyle]
            ]
            tr[][
                td[style style'][text "LineStyle:"]
                td[style style'][dropdown { placeholder = "LineStyle"; allowEmpty = false } [ clazz "ui inverted selection dropdown" ] (model.lineStyleNames |> AMap.map (fun k v -> text v)) (model.style.lineStyle) ChangeLineStyle]
            ]
            tr[][
                td[style style'][text "PrimaryColor:"]
                td[style style'][ColorPicker.view (model.style.primary) |> UI.map ChangeColorPrimary]
            ]
            tr[][
                td[style style'][text "SecondaryColor:"]
                td[style style'][ColorPicker.view (model.style.secondary) |> UI.map ChangeColorSecondary]
            ]
        ]