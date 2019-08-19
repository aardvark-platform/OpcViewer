namespace Linking

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open PRo3D.Minerva

type LinkingAction =
    | MinervaAction of MinervaAction
    | CheckPoint of V3d
    | SelectFeature of string
    | UnselectFeature of string

type LinkingFeature =
    {
        id: string
        hull: Hull3d
        position: V3d
        rotation: Rot3d
        trafo: Trafo3d
        color: C4b
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkingFeature =
    let initial = {
        id = ""
        hull = Hull3d 0
        position = V3d.Zero
        rotation = Rot3d.Identity
        trafo = Trafo3d.Identity
        color = C4b.Black
    }

[<DomainType>]
type LinkingModel =
    {
        frustums:           hmap<string,LinkingFeature>
        selectedFrustums:   hset<string>
        hoveredFrustrum:    Option<LinkingFeature>
        trafo:              Trafo3d
        minervaModel:       MinervaModel
        pickingPos:         Option<V3d>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkingModel = 
    let initial = {
        frustums            = hmap.Empty
        selectedFrustums    = hset.Empty
        hoveredFrustrum     = None
        trafo               = Trafo3d.Identity
        minervaModel        = Initial.model
        pickingPos          = None
    }