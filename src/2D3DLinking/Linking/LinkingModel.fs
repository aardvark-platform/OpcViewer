namespace PRo3D.Linking

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open PRo3D.Minerva

type LinkingFeature =
    {
        id: string
        hull: Hull3d
        position: V3d
        rotation: Rot3d
        trafo: Trafo3d
        trafoInv: Trafo3d
        camTrafo: Trafo3d
        camFrustum: Frustum
        color: C4b
        instrument: Instrument
        imageDimensions: V2i
        imageOffset: V2i
    }

type LinkingFeatureDisplay =
    {
        before: plist<LinkingFeature>
        f:        LinkingFeature
        after:    plist<LinkingFeature>
    }

type LinkingAction =
    //| MinervaAction of MinervaAction
    | UpdatePickingPoint of Option<V3d> * hmap<Instrument, bool>
    //| CheckPoint of V3d
    | ToggleView of Instrument
    | OpenFrustum of LinkingFeatureDisplay
    | ChangeFrustumOpacity of float
    | CloseFrustum

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkingFeature =
    let initial = {
        id = ""
        hull = Hull3d 0
        position = V3d.Zero
        rotation = Rot3d.Identity
        trafo = Trafo3d.Identity
        trafoInv = Trafo3d.Identity
        camTrafo = Trafo3d.Identity
        camFrustum = Frustum.perspective 60.0 0.01 1000.0 1.0
        color = C4b.Black
        instrument = Instrument.NotImplemented
        imageDimensions = V2i.Zero
        imageOffset = V2i.Zero
    }

type InstrumentParameter =
    {
        horizontalFoV:  float
        sensorSize:     V2i
    }

module InstrumentParameter =
    let initial = {
        horizontalFoV = 0.0
        sensorSize = V2i.Zero
    }

[<DomainType>]
type LinkingModel =
    {
        frustums:               hmap<string,LinkingFeature>
        instrumentParameter:    hmap<Instrument, InstrumentParameter>
        //selectedFrustums:       hset<string>
        //hoveredFrustrum:        Option<LinkingFeature>
        trafo:                  Trafo3d
        pickingPos:             Option<V3d>
        filterProducts:         hmap<Instrument, bool>
        overlayFeature:         Option<LinkingFeatureDisplay>
        frustumOpacity:         float
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkingModel = 
    let initial = {
        frustums            = hmap.Empty
        instrumentParameter = hmap.Empty
        //selectedFrustums    = hset.Empty
        //hoveredFrustrum     = None
        trafo               = Trafo3d.Identity
        pickingPos          = None
        filterProducts      = hmap.Empty
        overlayFeature      = None
        frustumOpacity      = 0.5
    }