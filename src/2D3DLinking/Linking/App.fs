namespace Linking

open Aardvark.Base

open System
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open Aardvark.Base.Incremental
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

    let loadFrustums (features: plist<Feature>) : hmap<string, LinkingFeature> * Trafo3d =
        
        // only interested in MastcamL and MastcamR products
        let reducedFeatures = features.Filter (fun _ f -> 
            match f.instrument with
            | Instrument.MastcamL | Instrument.MastcamR -> true
            | Instrument.Mastcam -> false // TODO: dunno?
            | _ -> false
        )

        // creating frustums by specifying fov
        let createFrustumProj (fov : float) =
            let aspectRatio = 1.3333333333333333333 // 1600:1200
            let frustum = Frustum.perspective fov 0.01 15.0 aspectRatio
            let fullFrustum = Frustum.perspective fov 0.01 1000.0 aspectRatio
            let proj = Frustum.projTrafo(frustum)
            (proj, proj.Inverse, fullFrustum)

        let cam34Frustum, cam34Inv, fullFrustum34 = createFrustumProj 16.370
        let cam100Frustum, cam100Inv, fullFrustum100 = createFrustumProj 5.67

        let angleToRad = V3d(Math.PI / 180.0) * V3d(1.0,1.0,2.0)
        
        let originTrafo = 
            match reducedFeatures.TryGet 0 with
            | Some v -> Trafo3d.Translation v.geometry.positions.Head
            | None -> Trafo3d.Identity

        let originTrafoInv = originTrafo.Backward
        
        //|> Mod.map2 (fun (t: Trafo3d) x -> 
        //    x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) trafo

        // map minerva features to linking features
        let linkingFeatures : hmap<string, LinkingFeature> = 
            reducedFeatures.Map (fun _ f ->

                let position = originTrafoInv.TransformPos(f.geometry.positions.Head)
                let angles = f.geometry.coordinates.Head

                let color = f.instrument |> MinervaModel.instrumentColor

                let (w, h) = f.dimensions
                let dimensions = V2i(w, h)

                let frustumTrafo, frustumTrafoInv, fullFrustum =
                    match f.instrument with
                    | Instrument.MastcamL -> cam34Frustum, cam34Inv, fullFrustum34
                    | Instrument.MastcamR -> cam100Frustum, cam100Inv, fullFrustum100
                    | Instrument.Mastcam -> (Trafo3d.Scale 0.0), (Trafo3d.Scale 0.0), (Frustum.ofTrafo Trafo3d.Identity)  // TODO: dunno?
                    | _ -> (Trafo3d.Scale 0.0), (Trafo3d.Scale 0.0), (Frustum.ofTrafo Trafo3d.Identity) // TODO: discard!

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
                })
            )
            |> PList.toList
            |> HMap.ofList

        (linkingFeatures, originTrafo)


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
                |> HMap.filter (fun _ v -> v.hull.Contains originP) 
                |> HMap.keys

            let filterProducts =
                intersected
                |> HSet.choose (fun p -> HMap.tryFind p m.frustums)
                |> HSet.map (fun p -> p.instrument)
                |> HSet.mapHMap (fun _ -> true)

            let partialUpdatedM = { m with pickingPos = Some(originP); filterProducts = filterProducts }
            update view partialUpdatedM (MinervaAction(MinervaAction.UpdateSelection (intersected |> HSet.toList)))

        | ToggleView i ->
            let filterProducts = 
                match m.filterProducts.TryFind i with
                | Some b -> m.filterProducts.Add (i, not b)
                | None -> m.filterProducts
            { m with filterProducts = filterProducts }

        | OpenFrustum f ->
            // also handled by upper app
            { m with overlayFeature = Some(f) }
            
        | CloseFrustum ->
            { m with overlayFeature = None }

        | MinervaAction a ->

            match a with

            | MinervaAction.LoadProducts _ -> 
                let minervaModel = MinervaApp.update view m.minervaModel a
                let (frustums, trafo) = loadFrustums minervaModel.data.features
                { m with minervaModel = minervaModel; frustums = frustums; trafo = trafo }

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
                let selectedFrustums = list |> List.filter(fun s -> (HMap.containsKey s m.frustums)) |> HSet.ofList

                Log.line "updateselection: s#: %A" list.Length
                { m with minervaModel = MinervaApp.update view m.minervaModel a; selectedFrustums = selectedFrustums}

            | MinervaAction.ClearSelection ->
                { m with minervaModel = MinervaApp.update view m.minervaModel a; selectedFrustums = hset.Empty}

            | _ -> { m with minervaModel = MinervaApp.update view m.minervaModel a}

        | _ -> failwith "Not implemented yet"

    
    let cssColor (c: C4b) =
        sprintf "rgba(%d, %d, %d, %f)" c.R c.G c.B c.Opacity

    let instrumentColor (i: Instrument) =
        i |> MinervaModel.instrumentColor |> cssColor

    let view (m: MLinkingModel) =

        let sgFrustum' (f: LinkingFeature) =
            Sg.wireBox' f.color (Box3d(V3d.NNN,V3d.III))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }

        let sgFrustum (f: IMod<LinkingFeature>) =
            Sg.wireBox (f |> Mod.map(fun f -> f.color)) (Mod.constant(Box3d(V3d.NNN,V3d.III)))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.trafo (f |> Mod.map(fun f -> f.trafo))

        let infinitelySmall = Trafo3d.Scale 0.0

        let hoverFrustum =
            m.hoveredFrustrum
            |> Mod.map (fun f -> f |> (Option.defaultValue { LinkingFeature.initial with trafo = infinitelySmall }))
            |> sgFrustum

        let frustra =
            m.frustums
            |> AMap.toASet
            |> ASet.map (fun (k, v) ->
                v
                |> sgFrustum'
                |> Sg.trafo (
                    m.selectedFrustums  
                    |> ASet.contains k
                    |> Mod.map (fun s -> if s then v.trafo else infinitelySmall) 
                )
            )
            |> Sg.set

        let pickingIndicator =
            Sg.sphere 3 (Mod.constant(C4b.VRVisGreen)) (Mod.constant(0.05))
            |> Sg.noEvents
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.trafo (
                m.pickingPos 
                |> Mod.map (fun p -> 
                    p 
                    |> Option.map Trafo3d.Translation 
                    |> Option.defaultValue (Trafo3d.Scale 0.0)
                )
            )


        let defaultScene = 
            Sg.ofArray [|
                frustra
                hoverFrustum
            |]

        let featureScene =
            Sg.empty

        let commonScene =
            Sg.ofArray [|
                pickingIndicator
            |]

        let scene = 
            Sg.ofArray [|
                (
                    m.overlayFeature 
                    |> Mod.map(fun o ->
                        if o = None
                        then defaultScene
                        else featureScene
                    )
                    |> Sg.dynamic
                )
                commonScene
            |]
            |> Sg.trafo m.trafo

        Sg.ofArray [|
            scene
            MinervaApp.viewFeaturesSg m.minervaModel |> Sg.map MinervaAction
        |]

    let viewSideBar (m: MLinkingModel) =
        div [clazz "ui buttons"] [
            //button [clazz "ui button"; onClick (fun _ -> LoadProducts)][text "Load"]
            button [clazz "ui button inverted"; onClick (fun _ -> MinervaAction(MinervaAction.UpdateSelection(m.frustums |> AMap.keys |> ASet.toList)))][text "Select All"]         
            button [clazz "ui button inverted"; onClick (fun _ -> MinervaAction(MinervaAction.ClearSelection))][text "Clear Selection"]         
            //button [clazz "ui button"; onClick (fun _ -> ApplyFilters)][text "Filter"]         
        ]

    let sceneOverlay (v: IMod<CameraView>) (m: MLinkingModel) =

        // 75% = aspect ratio 4:3
        let styleTag = 
            DomNode.Text("style", None, AttributeMap.Empty, (Mod.constant("
                 .scene-overlay {
                     position: absolute;
                     margin: 0;
                     top: 0;
                     z-index: 10;
                     width: 100%;
                     height: 100%;
                     text-align: center;
                 }

                 .frustum-svg {
                    height: 100%;
                    border: 2px solid white;
                 }
                ")))

        let overlayDom (f: LinkingFeature) : DomNode<LinkingAction> =
            
            let fullRect = [
                attribute "x" "0"
                attribute "y" "0"
                attribute "width" "1600"
                attribute "height" "1200"
            ]

            let dim = V2d(1600.0, 1200.0)
            let border = (dim - V2d(f.imageDimensions)) * 0.5

            let frustumRect = [
                attribute "x" (sprintf "%f" border.X)
                attribute "y" (sprintf "%f" border.Y)
                attribute "width" (string f.imageDimensions.X)
                attribute "height" (string f.imageDimensions.Y)
            ]

            div [clazz "ui scene-overlay"] [
                Svg.svg[
                    clazz "frustum-svg"
                    attribute "viewBox" "0 0 1600 1200"
                    style (sprintf "border-color: %s" (instrumentColor f.instrument))
                ][
                    (DomNode.Element ("mask", None, (AttributeMap.ofList [
                        attribute "id" "frustumMask"
                    ]), AList.ofList [
                        Svg.rect (fullRect @ [
                            attribute "fill" "white"
                        ])
                        Svg.rect (frustumRect @ [
                            attribute "fill" "black"
                        ])
                    ]))
                    Svg.rect (fullRect @ [
                        attribute "fill" "rgba(0,0,0,0.5)"
                        attribute "mask" "url(#frustumMask)"
                    ])
                    
                ]
            ]

        let dom =
            m.overlayFeature 
            |> Mod.map(fun f ->
                match f with
                | None -> div[][] // DomNode.empty requires unit?
                | Some(o) -> overlayDom o
            )
            |> AList.ofModSingle
            |> Incremental.div AttributeMap.empty
            
        div[][
            styleTag
            dom
        ]

    let viewHorizontalBar (m: MLinkingModel) =
        
        //<div class="ui checkbox">
        //    <input type="checkbox" name="example">
        //    <label>Make my profile visible</label>
        //</div>

        let products =
            m.selectedFrustums
            |> ASet.chooseM (fun k -> AMap.tryFind k m.frustums)

        let filteredProducts =
            products
            |> ASet.filterM (fun p -> 
                m.filterProducts
                |> AMap.tryFind p.instrument
                |> Mod.map (fun m -> m |> Option.defaultValue false))

        let productsAndPoints =
            filteredProducts
            |> ASet.map(fun prod ->
                m.pickingPos 
                |> Mod.map(fun f -> 
                    f 
                    |> Option.defaultValue V3d.Zero
                    |> fun p -> V4d(p, 1.0)
                    |> prod.trafoInv.Forward.Transform
                    |> fun p -> V3d((p.XY / p.W), p.Z)
                )
                |> Mod.map(fun pp -> (prod, pp))
            )
            |> ASet.mapM (fun p -> p)

        let countStringPerInstrument =
            products
            |> ASet.groupBy (fun f -> f.instrument)
            |> AMap.map (fun _ v -> v.Count)
            |> AMap.toASet
            |> ASet.toAList
            |> AList.sortBy (fun (i, _) -> i)
            |> AList.map (fun (i, c) -> 
                let s = sprintf "%A: %d" i c
                (i, Mod.constant s)
            )

        let dependencies =
            Html.semui @ [
                { kind = Stylesheet; name = "linking.css"; url = "resources/linking.css" }
            ]

        let styleTag = 
           DomNode.Text("style", None, AttributeMap.Empty, (Mod.constant("
                .product-view {
                    display: inline-block;
                    position: relative;
                    margin: 0 5px;
                    border-top: 2px solid white;
                    border-radius: 2px;
                    cursor: pointer;
                }

                .product-view img {
                    height: 100%;
                    display: inline-block
                }

                .product-view svg {
                    position: absolute;
                    top: 0;
                    left: 0;
                    right: 0;
                    display: inline-block;
                    height: 100%;
                }
                
                .product-view span {
                    position: absolute;
                    z-index: 5;
                    left: 0;
                    padding: 0.5em;
                    line-height: 1em;
                    font-size: 0.55em;
                    background-color: rgba(0,0,0,0.5);
                }

                .noselect {
                    cursor: default;
                    -webkit-touch-callout: none;
                    -webkit-user-select: none;
                    -khtml-user-select: none;
                    -moz-user-select: none;
                    -ms-user-select: none;
                    user-select: none;
                }
                ")))
           
        //let scriptTag =
        //   DomNode.Text("script", None, AttributeMap.Empty, (Mod.constant("

        //        function productLoad(elem) {
        //            var h = $(this).height();
        //            var w = $(this).width();
        //        }

        //   ")))

        let fullCount = products |> ASet.count 
        let filteredCount = filteredProducts |> ASet.count
        let countString =
            Mod.map2 (fun full filtered -> 
                if full = filtered
                then full |> string
                else (sprintf "%d (%d)" full filtered)
            ) fullCount filteredCount


        require dependencies (
            body [style "width: 100%; height:100%; background: transparent;"] [
                styleTag
                //scriptTag
                div[clazz "noselect"; style "color:white; padding: 5px; width: 100%; height: 100%; position: absolute;"][
                    div[style "padding: 5px; position: fixed"][
                        span[clazz "ui label inverted"; style "margin-right: 10px; width: 10em; position: relative;"][
                            text "Products"
                            div[clazz "detail"; style "position: absolute; right: 1em;"][Incremental.text countString]
                        ]
                        Incremental.span AttributeMap.Empty (
                            countStringPerInstrument
                            |> AList.map (fun (i, s) ->
                            
                                //Incremental.li (AttributeMap.ofList([style "display: inline-block;";])) (AList.ofModSingle (Mod.constant (Incremental.text m)))
                                
                                let o = AMap.tryFind i m.filterProducts |> Mod.map (fun b -> b |> Option.defaultValue false)
                                //checkbox [clazz "ui inverted checkbox"] o (ToggleView i) s
                                //span[clazz "spectrum-Label spectrum-Label--grey";
                                span[clazz "ui inverted label";
                                    style (sprintf "background-color: %s;" (instrumentColor i))][
                                    Html.SemUi.iconCheckBox o (ToggleView i)
                                    Incremental.text s
                                    //checkbox [clazz "ui inverted checkbox"] o (ToggleView i) s
                                ]
                            )
                        )
                        //checkbox [clazz "ui inverted toggle checkbox"] m.pickingModel.debugShadowVolume PickingAction.ShowDebugVis "Show Debug Vis"
                    ]

                    Incremental.div (AttributeMap.ofList[style "overflow-x: scroll; overflow-y: hidden; white-space: nowrap; height: 100%; padding-top: 2.8em;"]) (
                        productsAndPoints
                        |> ASet.toAList
                        |> AList.map (fun (f, p) ->
                            // check if inside image!
                            let (sensorW, sensorH) = (1600, 1200)

                            let sensor = V2d(sensorW, sensorH)
                            let image = V2d(f.imageDimensions)

                            let max = image / sensor // ratio is inside

                            (f, p, (image, sensor, max))
                        )
                        |> AList.sortBy (fun (f, p, (image, sensor, max)) ->
                            let ratioP = p.XY * V2d(1.0, sensor.Y / sensor.X)
                            let dist = V2d.Dot(ratioP, ratioP) // euclidean peseudo distance (w/o root)

                            if abs(p.X) > max.X || abs(p.Y) > max.Y
                            then infinity
                            else dist
                        )
                        |> AList.map (fun (f, p, (image, sensor, max)) -> 
                            let fileName = sprintf "MinervaData\%s.png" (f.id.ToLower())
                            let imgSrc = System.IO.Path.GetFullPath(System.IO.Path.Combine(Environment.CurrentDirectory, fileName))
                            let webSrc = "file:///" + imgSrc.Replace("\\", "/")
                            
                            let (w, h) = (f.imageDimensions.X, f.imageDimensions.Y)
                            //let c = ((p.XY * V2d(1.0, -1.0)) + 1.0) * 0.5 // transform [-1, 1] to [0, 1]
                            let c = (p.XY * V2d(1.0, -1.0)) // flip y

                            let cc = c / max // correct for max
                            
                            let ratio = (float h)/(float w)
                            let invRatio = 1.0/ratio

                            let rc = cc * V2d(invRatio, 1.0)

                            //div[style "display: inline-block; position: relative; margin: 0 5px";
                            div[
                                clazz "product-view"
                                style (sprintf "border-color: %s" (instrumentColor f.instrument))
                                onClick (fun _ -> OpenFrustum f)
                            ][
                                img[
                                    clazz f.id; 
                                    attribute "alt" f.id; 
                                    attribute "src" webSrc;
                                    //attribute "onload" "productLoad(this);"
                                    //style "height: 100%; display: inline-block"
                                ]
                                Svg.svg[attribute "viewBox" (sprintf "%f -1 %f 2" (-invRatio) (invRatio * 2.0))
                                //style "position: absolute; top: 0; left: 0; display: inline-block; height: 100%"
                                ][
                                    Svg.circle[
                                        attribute "cx" (sprintf "%f" rc.X)
                                        attribute "cy" (sprintf "%f" rc.Y)
                                        attribute "r" "1.0"
                                        //attribute "fill" (C4b.VRVisGreen |> cssColor)
                                        attribute "fill" "transparent"
                                        attribute "stroke" (C4b.VRVisGreen |> cssColor)
                                        attribute "stroke-width" "0.03"
                                    ]
                                    // vertical line
                                    Svg.line[
                                        attribute "x1" "0"
                                        attribute "y1" "-1"
                                        attribute "x2" "0"
                                        attribute "y2" "1"
                                        attribute "stroke" "black"
                                        attribute "stroke-width" "0.01"
                                    ]
                                    // horizontal line
                                    Svg.line[
                                        attribute "x1" (sprintf "%f" -invRatio)
                                        attribute "y1" "0"
                                        attribute "x2" (sprintf "%f" invRatio)
                                        attribute "y2" "0"
                                        attribute "stroke" "black"
                                        attribute "stroke-width" "0.01"
                                    ]
                                    Svg.line[
                                        attribute "x1" "0"
                                        attribute "y1" "0"
                                        attribute "x2" (sprintf "%f" rc.X)
                                        attribute "y2" (sprintf "%f" rc.Y)
                                        attribute "stroke" "black"
                                        attribute "stroke-width" "0.01"
                                    ]
                                ]
                                //text (sprintf "%s" (cc.ToString("0.00")))
                                text f.id
                            ]
                        )
                    )
                ]
            ]
        )
        