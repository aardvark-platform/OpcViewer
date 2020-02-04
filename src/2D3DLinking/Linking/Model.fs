namespace Linking

open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open PRo3D.Minerva

open Adaptify

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
        before: IndexList<LinkingFeature>
        f:        LinkingFeature
        after:    IndexList<LinkingFeature>
    }

type LinkingAction =
    | MinervaAction of MinervaAction
    | CheckPoint of V3d
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

[<ModelType>]
type LinkingModel =
    {
        frustums:               HashMap<string,LinkingFeature>
        instrumentParameter:    HashMap<Instrument, InstrumentParameter>
        selectedFrustums:       HashSet<string>
        hoveredFrustrum:        Option<LinkingFeature>
        trafo:                  Trafo3d
        minervaModel:           MinervaModel
        pickingPos:             Option<V3d>
        filterProducts:         HashMap<Instrument, bool>
        overlayFeature:         Option<LinkingFeatureDisplay>
        frustumOpacity:         float
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkingModel = 
    let initial = {
        frustums            = HashMap.Empty
        instrumentParameter = HashMap.Empty
        selectedFrustums    = HashSet.Empty
        hoveredFrustrum     = None
        trafo               = Trafo3d.Identity
        minervaModel        = Initial.model
        pickingPos          = None
        filterProducts      = HashMap.Empty
        overlayFeature      = None
        frustumOpacity      = 0.5
    }