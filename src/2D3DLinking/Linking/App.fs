namespace Linking

open Aardvark.Base

open System
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Application

open PRo3D.Minerva
open FShade
open OpcViewer.Base
open Aardvark.UI
open Aardvark.UI.Primitives

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

    let loadFrustums (features: IndexList<Feature>) : HashMap<string, LinkingFeature> * Trafo3d * HashMap<Instrument, InstrumentParameter> =

        // sensor sizes
        let mastcamRLSensor = V2i(1600, 1200)

        let instrumentParameter = hmap.OfList [
            (Instrument.MastcamL, { horizontalFoV = 15.0; sensorSize = mastcamRLSensor }) //34 mm
            (Instrument.MastcamR, { horizontalFoV = 5.1; sensorSize = mastcamRLSensor }) // 100 mm
        ]
        
        // only interested in MastcamL and MastcamR products
        let reducedFeatures = features.Filter (fun _ f -> instrumentParameter.ContainsKey f.instrument)

        // creating frustums by specifying fov
        let createFrustumProj (p: InstrumentParameter) =
            let aspectRatio = (float p.sensorSize.X) / (float p.sensorSize.Y)
            let fov = p.horizontalFoV * aspectRatio
            let frustum = Frustum.perspective fov 0.01 15.0 aspectRatio
            let fullFrustum = Frustum.perspective fov 0.01 1000.0 aspectRatio
            let proj = Frustum.projTrafo(frustum)
            (proj, proj.Inverse, fullFrustum)

        let frustumData =
            instrumentParameter
            |> HashMap.map (fun _ v ->
                createFrustumProj(v)
            )

        let angleToRad = V3d(Math.PI / 180.0) * V3d(1.0,1.0,2.0)
        
        let originTrafo = 
            match reducedFeatures.TryGet 0 with
            | Some v -> Trafo3d.Translation v.geometry.positions.Head
            | None -> Trafo3d.Identity

        let originTrafoInv = originTrafo.Backward

        // map minerva features to linking features
        let linkingFeatures : HashMap<string, LinkingFeature> = 
            reducedFeatures.Map (fun _ f ->

                let position = originTrafoInv.TransformPos(f.geometry.positions.Head)
                let angles = f.geometry.coordinates.Head

                let color = f.instrument |> MinervaModel.instrumentColor

                let (w, h) = f.dimensions
                let dimensions = V2i(w, h)

                let frustumTrafo, frustumTrafoInv, fullFrustum = 
                    frustumData
                    |> HashMap.tryFind f.instrument
                    |> Option.defaultValue (Trafo3d.Scale 0.0, Trafo3d.Scale 0.0, Frustum.ofTrafo Trafo3d.Identity) // ignored
             
                let rotation = Rot3d.FromAngleAxis(angles * angleToRad)
                let translation = Trafo3d.Translation position

                let innerRot = Trafo3d.Rotation(V3d.OOI, -angles.Z * Math.PI / 180.0)

                let rotTranslateTrafo = innerRot * Trafo3d(rotation) * translation
                let trafo = frustumTrafoInv * rotTranslateTrafo
                let trafoInv = rotTranslateTrafo.Inverse * frustumTrafo

                let hull = trafoInv |> toHull3d 

                (f.id, {
                    id = f.id
                    hull = hull
                    position = position
                    rotation = rotation
                    trafo = trafo
                    trafoInv = trafoInv
                    camTrafo = originTrafo.Inverse * rotTranslateTrafo.Inverse
                    camFrustum = fullFrustum
                    color = color
                    instrument = f.instrument
                    imageDimensions = dimensions
                    imageOffset = V2i(305 + 48, 385) // ATTENTION/TODO hardcoded data value, replace with database!
                })
            )
            |> IndexList.toList
            |> HashMap.ofList

        (linkingFeatures, originTrafo, instrumentParameter)

    //---UPDATE
    let rec update (view: CameraView) (m: LinkingModel) (msg: LinkingAction) : LinkingModel =

        let minervaFrustumHit (hit: SceneHit) =
            let closestPoints = MinervaApp.queryClosestPoint m.minervaModel hit
            match closestPoints with
            | emptySeq when Seq.isEmpty emptySeq -> 
                None
            | seq -> 
                let index = seq |> Seq.map (fun (depth, pos, index) -> index) |> Seq.head
                let closestID = m.minervaModel.selection.flatID.[index]
                (m.frustums.TryFind closestID)
            
        match msg with
        | CheckPoint p ->
            let originP = m.trafo.Backward.TransformPos p

            let intersected = 
                m.frustums 
                |> HashMap.filter (fun _ v -> v.hull.Contains originP) 
                |> HashMap.keys

            let filterProducts =
                intersected
                |> HashSet.choose (fun p -> HashMap.tryFind p m.frustums)
                |> HashSet.map (fun p -> p.instrument)
                |> HashSet.mapHMap (fun _ -> true)

            let partialUpdatedM = { m with pickingPos = Some(originP); filterProducts = filterProducts }
            update view partialUpdatedM (MinervaAction(MinervaAction.UpdateSelection (intersected |> HashSet.toList)))

        | ToggleView i ->
            match m.filterProducts.TryFind i with
            | Some b -> 
                { m with filterProducts = m.filterProducts.Add (i, not b) }
            | None -> m

        | OpenFrustum f -> { m with overlayFeature = Some(f) }  // also handled by upper app
        | CloseFrustum -> { m with overlayFeature = None }
        | ChangeFrustumOpacity v -> { m with frustumOpacity = v }
        | MinervaAction a ->

            match a with

            | MinervaAction.LoadProducts _ -> 
                let minervaModel = MinervaApp.update view m.minervaModel a
                let (frustums, trafo, instrumentParameter) = loadFrustums minervaModel.data.features
                { m with minervaModel = minervaModel; frustums = frustums; trafo = trafo; instrumentParameter = instrumentParameter }

            | MinervaAction.HoverProducts hit ->
                { m with 
                    minervaModel = MinervaApp.update view m.minervaModel a
                    hoveredFrustrum = minervaFrustumHit hit 
                }

            |  MinervaAction.PickProducts hit -> //MinervaAction.AddProductToSelection name -> // MinervaAction.SingleSelectProduct |  no idea what difference
                let selectedFrustums =
                    match minervaFrustumHit hit with
                    | Some f -> 
                        if m.selectedFrustums.Contains f.id 
                        then m.selectedFrustums.Remove f.id
                        else m.selectedFrustums.Add f.id
                    | None -> m.selectedFrustums
                { m with minervaModel = MinervaApp.update view m.minervaModel a; selectedFrustums = selectedFrustums}
              
            | MinervaAction.UpdateSelection list ->
                let selectedFrustums = list |> List.filter(fun s -> (HashMap.containsKey s m.frustums)) |> HashSet.ofList

                Log.line "updateselection: s#: %A" list.Length
                { m with minervaModel = MinervaApp.update view m.minervaModel a; selectedFrustums = selectedFrustums}

            | MinervaAction.ClearSelection ->
                { m with minervaModel = MinervaApp.update view m.minervaModel a; selectedFrustums = hset.Empty}

            | _ -> { m with minervaModel = MinervaApp.update view m.minervaModel a}

        | _ -> failwith "Not implemented yet"

    //---Helpers
    let dependencies =
        Html.semui @ [
            { kind = Stylesheet; name = "linkingstyle.css"; url = "./resources/linkingstyle.css" }
        ]
    
    let cssColor (c: C4b) =
        sprintf "rgba(%d, %d, %d, %f)" c.R c.G c.B c.Opacity

    let instrumentColor (i: Instrument) =
        i |> MinervaModel.instrumentColor |> cssColor

    let imageSrc (f: LinkingFeature) =
        sprintf "MinervaData/%s.png" (f.id.ToLower())

    let svgLine (p1: float * float) (p2: float * float) (color: string) (storkeWidth: float) =
        let (x1, y1) = p1
        let (x2, y2) = p2
        Svg.line[
            attribute "x1" (string x1)
            attribute "y1" (string y1)
            attribute "x2" (string x2)
            attribute "y2" (string y2)
            attribute "stroke" color
            attribute "stroke-width" (string storkeWidth)
        ]
        
    let svgLine' (p1: V2d) (p2: V2d) (color: C4b) (storkeWidth: float) =
        svgLine (p1.X, p1.Y) (p2.X, p2.Y) (color |> cssColor) storkeWidth

    //---VIEWS
    let view (m: MLinkingModel) =

        let sgFrustum' (f: LinkingFeature) =
            Sg.wireBox' f.color (Box3d(V3d.NNN,V3d.III))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }

        let sgFrustum (f: aval<LinkingFeature>) =
            f |> AVal.map (fun x -> x |> sgFrustum' |> Sg.transform x.trafo)

        let hoverFrustum =
            m.hoveredFrustrum
            |> AVal.map (fun f -> f |> (Option.defaultValue { LinkingFeature.initial with trafo = Trafo3d.Scale 0.0 }))
            |> sgFrustum
            |> Sg.dynamic

        let frustra =
            m.frustums
            |> AMap.toASet
            |> ASet.map (fun (k, v) ->
                v
                |> sgFrustum'
                |> Sg.trafo (
                    m.selectedFrustums  
                    |> ASet.contains k
                    |> AVal.map (fun s -> if s then v.trafo else Trafo3d.Scale 0.0) 
                )
            )
            |> Sg.set

        let pickingIndicator =
            Sg.sphere 3 (AVal.constant C4b.VRVisGreen) (AVal.constant 0.05)
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.trafo (
                m.pickingPos 
                |> AVal.map (fun p -> 
                    p 
                    |> Option.map Trafo3d.Translation 
                    |> Option.defaultValue (Trafo3d.Scale 0.0)
                )
            )

        let defaultScene = 
            [|
                frustra
                hoverFrustum
            |]
            |> Sg.ofArray 

        let commonScene =
            [|
                pickingIndicator
            |]
            |> Sg.ofArray 

        let scene = 
            [|
                (
                    m.overlayFeature 
                    |> AVal.map(fun o ->
                        match o with
                        | None -> defaultScene
                        | Some x -> Sg.empty) // featureScene
                    |> Sg.dynamic
                )
                commonScene
            |]
            |> Sg.ofArray 
            |> Sg.trafo m.trafo

        [|
            scene
            MinervaApp.viewFeaturesSg m.minervaModel |> Sg.map MinervaAction
        |]
        |> Sg.ofArray 

    let viewSideBar (m: MLinkingModel) =

        let modD = 
            m.overlayFeature
            |> AVal.map (fun od -> 
                match od with
                | None -> (None, None)
                | Some(d) -> 
                    let before = 
                        d.before.TryGet (d.before.Count - 1) // get last element of before list (before f)
                        |> Option.map (fun f -> 
                            { 
                                before = d.before.Remove f
                                f = f
                                after = d.after.Prepend d.f
                            }
                        )

                    let after =
                        d.after.TryGet 0 // get first element of after list (after f)
                        |> Option.map (fun f -> 
                            {
                                before = d.before.Append d.f
                                f = f
                                after = d.after.Remove f
                            }
                        )

                    (before, after)
            )

        require dependencies (
            div [][
                div [clazz "inverted fluid ui vertical buttons"] [
                    button [clazz "inverted ui button"; onClick (fun _ -> MinervaAction(MinervaAction.UpdateSelection(m.frustums |> AMap.keys |> ASet.toList)))][text "Select All"]         
                    button [clazz "inverted ui button"; onClick (fun _ -> MinervaAction(MinervaAction.ClearSelection))][text "Clear Selection"]                 
                ]
                Incremental.div 
                    (AttributeMap.ofAMap (amap { 
                        let! d = m.overlayFeature
                        if d.IsNone then
                            yield style "display: none;"
                    }))
                    (AList.ofList [
                        slider 
                            {min = 0.0; max = 1.0; step = 0.01} 
                            [clazz "ui blue slider"]
                            m.frustumOpacity
                            ChangeFrustumOpacity

                        div [clazz "inverted fluid ui buttons"] [
                            Incremental.button (AttributeMap.ofAMap (amap {
                                let classString = "inverted labeled ui icon button"
                                
                                let! (before, _) = modD
                                match before with
                                | Some(d) -> 
                                    yield onClick (fun _ -> (LinkingAction.OpenFrustum d))
                                    yield clazz classString
                                | None -> yield clazz (classString + " disabled")

                            })) (AList.ofList [
                                i [clazz "caret left icon"][]
                                text "Previous"
                            ])
                            Incremental.button (AttributeMap.ofAMap (amap {
                                let classString = "inverted right labeled ui icon button"
                                                           
                                let! (_, after) = modD
                                match after with
                                | Some(d) -> 
                                    yield onClick (fun _ -> (LinkingAction.OpenFrustum d))
                                    yield clazz classString
                                | None -> yield clazz (classString + " disabled")

                            })) (AList.ofList [
                                i [clazz "caret right icon"][]
                                text "Next"
                            ])
                        ]

                        button [clazz "fluid inverted ui button"; onClick (fun _ -> LinkingAction.CloseFrustum)][
                            i [clazz "close icon"][]
                            text "Close"
                        ]
                    ])
            ]
        )

    let sceneOverlay (v: aval<CameraView>) (m: MLinkingModel) : DomNode<LinkingAction> =

        let overlayDom (f: LinkingFeature, dim: V2i) : DomNode<LinkingAction> =

            let offset = f.imageOffset //let border = (dim - V2d(f.imageDimensions)) * 0.5

            let frustumRect = [
                attribute "x" (string offset.X) //border.x
                attribute "y" (string offset.Y) // border.y
                attribute "width" (string f.imageDimensions.X)
                attribute "height" (string f.imageDimensions.Y)
            ]

            div [clazz "ui scene-overlay"] [
                Svg.svg [
                    clazz "frustum-svg"
                    attribute "id" "frustum-overlay-svg"
                    attribute "viewBox" "0 0 1600 1200"
                    style (sprintf "border-color: %s" (instrumentColor f.instrument))
                ][
                    Svg.path [
                        attribute "fill" "rgba(0,0,0,0.5)"
                        attribute "d" (sprintf "M0 0 h%d v%d h-%dz M%d %d v%d h%d v-%dz"
                            dim.X dim.Y dim.X offset.X offset.Y f.imageDimensions.Y f.imageDimensions.X f.imageDimensions.Y)
                    ]
                    DomNode.Node ("g", "http://www.w3.org/2000/svg",
                        AttributeMap.ofAMap (amap {
                            let! a = m.frustumOpacity
                            yield attribute "opacity" (string a)
                        }),
                        AList.ofList [
                            Svg.image (frustumRect @ [
                                attribute "href" (imageSrc f)
                            ])
                        ]
                    )
                ]
            ]

        let dom =
            m.overlayFeature
            |> AVal.bind (fun op -> 
                op 
                |> Option.map (fun d -> 
                    m.instrumentParameter 
                    |> AMap.tryFind d.f.instrument
                    |> AVal.map (fun ip -> (d.f, ip))
                )
                |> Option.defaultValue (AVal.constant (LinkingFeature.initial, None))
            )
            |> AVal.map(fun (f: LinkingFeature, s: Option<InstrumentParameter>) ->
                match s with
                | None -> div[][] // DomNode.empty requires unit?
                | Some(o) -> overlayDom (f, o.sensorSize)
            )
            |> AList.ofModSingle
            |> Incremental.div AttributeMap.empty
            
        require dependencies dom

    let viewHorizontalBar (m: MLinkingModel) =
        
        let products =
            m.selectedFrustums
            |> ASet.chooseM (fun k -> AMap.tryFind k m.frustums)

        let filteredProducts =
            products
            |> ASet.filterM (fun p -> 
                m.filterProducts
                |> AMap.tryFind p.instrument
                |> AVal.map (fun m -> m |> Option.defaultValue false))

        let productsAndPoints =
            filteredProducts
            |> ASet.map(fun prod ->
                m.pickingPos 
                |> AVal.map(fun f -> 
                    f 
                    |> Option.defaultValue V3d.Zero
                    |> fun p -> V4d(p, 1.0)
                    |> prod.trafoInv.Forward.Transform
                    |> fun p -> V3d((p.XY / p.W), p.Z)
                )
                |> AVal.map(fun pp -> (prod, pp))
            )
            |> ASet.mapM id

        let countStringPerInstrument =
            products
            |> ASet.groupBy (fun f -> f.instrument)
            |> AMap.map (fun _ v -> v.Count)
            |> AMap.toASet
            |> ASet.toAList
            |> AList.sortBy (fun (i, _) -> i)
            |> AList.map (fun (i, c) -> 
                let s = sprintf "%A: %d" i c
                (i, AVal.constant s)
            )

        let fullCount = products |> ASet.count 
        let filteredCount = filteredProducts |> ASet.count
        let countString =
            AVal.map2 (fun full filtered -> 
                if full = filtered then
                    full |> string
                else 
                    (sprintf "%d (%d)" full filtered)
            ) fullCount filteredCount

        require dependencies (
            body [style "width: 100%; height:100%; background: transparent; min-width: 0; min-height: 0"] [
                div[clazz "noselect"; style "color:white; padding: 5px; width: 100%; height: 100%; position: absolute;"][
                    div[style "padding: 5px; position: fixed"][
                        span[clazz "ui label inverted"; style "margin-right: 10px; width: 10em; position: relative;"][
                            text "Products"
                            div[clazz "detail"; style "position: absolute; right: 1em;"][Incremental.text countString]
                        ]
                        Incremental.span AttributeMap.Empty (
                            countStringPerInstrument
                            |> AList.map (fun (i, s) ->
                                let o = AMap.tryFind i m.filterProducts |> AVal.map (fun b -> b |> Option.defaultValue false)
                                span[clazz "ui inverted label";
                                    style (sprintf "background-color: %s;" (instrumentColor i))][
                                    Html.SemUi.iconCheckBox o (ToggleView i) //checkbox [clazz "ui inverted checkbox"] o (ToggleView i) s
                                    Incremental.text s
                                ]
                            )
                        )
                        ]

                    Incremental.div (AttributeMap.ofList[style "overflow-x: scroll; overflow-y: hidden; white-space: nowrap; height: 100%; padding-top: 2.8em;"]) (
                        let sortedProducts = 
                            productsAndPoints
                            |> ASet.toAList
                            |> AList.map (fun (f, p) ->
                                // check if inside image!
                                m.instrumentParameter 
                                |> AMap.tryFind f.instrument 
                                |> AVal.map (Option.map (fun par ->

                                    let sensor = V2d(par.sensorSize)
                                    let image = V2d(f.imageDimensions)

                                    let max = image / sensor // ratio is inside
                                        
                                    f, p, (image, sensor, max)
                                ))
                            )
                            |> AList.chooseM id
                            |> AList.sortBy (fun (f, p, (image, sensor, max)) ->
                                let ratioP = p.XY * V2d(1.0, sensor.Y / sensor.X)
                                let dist = V2d.Dot(ratioP, ratioP) // euclidean peseudo distance (w/o root)

                                if abs(p.X) > max.X || abs(p.Y) > max.Y
                                then infinity
                                else dist
                            )
                        
                        // if you have a cleaner way of doing this I would be so happy to know it
                        let neighborList =
                            sortedProducts 
                            |> AList.toMod
                            |> AVal.map (fun l -> 
                                l
                                |> IndexList.mapi (fun i e -> 
                                    let onlyFrustra = l |> IndexList.map(fun a -> 
                                        let (f, _, _) = a
                                        f
                                    )
                                    let before, w, after = IndexList.split i onlyFrustra // TODO find out what is w
                                    (before, e, after)                             
                                )
                            )
                            |> AList.ofMod
                            
                        neighborList
                        |> AList.map (fun (before, (f, p, (image, sensor, max)), after) -> 
                        
                            let webSrc = imageSrc f
                            
                            let (w, h) = (f.imageDimensions.X, f.imageDimensions.Y)
                            let c = (p.XY * V2d(1.0, -1.0)) // flip y

                            let cc = c / max // correct for max
                            
                            let ratio = (float h)/(float w)
                            let invRatio = 1.0/ratio

                            let rc = cc * V2d(invRatio, 1.0)

                            Incremental.div (AttributeMap.ofAMap (amap {
                                yield style (sprintf "border-color: %s" (instrumentColor f.instrument))
                                yield onClick (fun _ -> OpenFrustum { before = before; f = f; after = after })

                                let! selected = m.overlayFeature

                                match selected with
                                | Some d when d.f = f -> yield clazz "product-view selected"
                                | _ -> yield clazz "product-view"

                            })) (AList.ofList [
                                img[
                                    clazz f.id; 
                                    attribute "alt" f.id; 
                                    attribute "src" webSrc;
                                ]
                                Svg.svg[attribute "viewBox" (sprintf "%f -1 %f 2" (-invRatio) (invRatio * 2.0))
                                ][
                                    Svg.circle[
                                        attribute "cx" (sprintf "%f" rc.X)
                                        attribute "cy" (sprintf "%f" rc.Y)
                                        attribute "r" "1.0"
                                        attribute "fill" "transparent"
                                        attribute "stroke" (C4b.VRVisGreen |> cssColor)
                                        attribute "stroke-width" "0.03"
                                    ]
                                    svgLine (0.0, -1.0) (0.0, 1.0) "black" 0.01 // vertical line
                                    svgLine (-invRatio, 0.0) (invRatio, 0.0) "black" 0.01 // horizontal line
                                    svgLine (0.0, 0.0) (rc.X, rc.Y) "black" 0.01 // direction line
                                ]
                                text f.id
                                div [clazz "product-indexed"; style "position: absolute; bottom: 1.2em"][
     (* ༼つಠ益ಠ༽つ ─=≡ΣO) *)        text (string (before.Count + 1)) // geologists probably start their indices at 1
                                ]
                            ])
                        )
                    )
                ]
            ]
        )
        