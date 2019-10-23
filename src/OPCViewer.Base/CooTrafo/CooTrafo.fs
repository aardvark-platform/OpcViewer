namespace OpcViewer.Base

open System
open System.Runtime.InteropServices
open Aardvark.Base


type Planet = 
  | Earth = 0
  | Mars  = 1
  | None  = 2

type SphericalCoo = {
  longitude : double
  latitude  : double
  altitude  : double
  radian    : double
}

module CooTrafoWrapper =

        [<DllImport(@".\coo\CooTransformation.dll")>]
        extern uint32 GetDllVersion()

        [<DllImport(@".\coo\CooTransformation.dll")>]
        extern int Init(string configDir, string logDir);

        [<DllImport(@".\coo\CooTransformation.dll")>]
        extern void DeInit();

        [<DllImport(@".\coo\CooTransformation.dll")>]
        extern int Xyz2LatLonRad(double dX, double dY, double dZ, double& pdLat, double& pdLon, double& pdRad)

        [<DllImport(@".\coo\CooTransformation.dll")>]
        extern int Xyz2LatLonAlt(string pcPlanet, double dX, double dY, double dZ, double& pdLat, double& pdLon, double& pdAlt )

        [<DllImport(@".\bin\CooTransformation.dll")>]
        extern int LatLonAlt2Xyz(string pcPlanet, double dLat, double dLon, double dAlt, double& pdX, double& pdY, double& pdZ )

module CooTransformation = 
    open System.IO
    
    let init = 0.0

    let initCooTrafo () = 
        let logDir = @".\coo" 
        let configDir = @".\coo"                 
        Log.line "[CooTrafo] cootrafo directory %A" (System.IO.Path.GetFullPath(configDir))
        if (Directory.Exists logDir) && (Directory.Exists configDir) then
            let errorCode = CooTrafoWrapper.Init(configDir, logDir)
            if errorCode > 0 then
                Log.line "[CooTrafo] cootrafo directory %d" errorCode
        else
            Log.error "[CooTrafo] init paths %A or %A not found" logDir configDir

    let deInitCooTrafo () = 
        CooTrafoWrapper.DeInit()

    let getLatLonAlt (p:V3d) (planet:Planet): SphericalCoo = 
      match planet with
      | Planet.None ->
        { latitude = nan; longitude = nan; altitude = nan; radian = 0.0 }
      | _ ->
        let mutable lat = init
        let mutable lon = init
        let mutable alt = init
      
        let bla = CooTrafoWrapper.Xyz2LatLonAlt(planet.ToString(), p.X, p.Y, p.Z, &lat, &lon, &alt)
        
        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        {
            latitude  = lat
            longitude = lon
            altitude  = alt
            radian = 0.0;
        }

    let getLatLonRad (p:V3d) : SphericalCoo = 
        let mutable lat = init
        let mutable lon = init
        let mutable rad = init
        let bla = CooTrafoWrapper.Xyz2LatLonRad( p.X, p.Y, p.Z, &lat, &lon, &rad)
        
        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        {
            latitude  = lat
            longitude = lon
            altitude  = 0.0
            radian    = rad
        }

    let getXYZFromLatLonAlt (sc:SphericalCoo) (planet:Planet) : V3d = 
      match planet with
      | Planet.None -> V3d.NaN
      | _ ->
        let mutable pX = init
        let mutable pY = init
        let mutable pZ = init
        let error = CooTrafoWrapper.LatLonAlt2Xyz(planet.ToString(), sc.latitude, sc.longitude, sc.altitude, &pX, &pY, &pZ)

        if error <> 0 then
            Log.line "cootrafo errorcode %A" error

        V3d(pX, pY, pZ)

    let getHeight (p:V3d) (up:V3d) (planet:Planet) = 
      match planet with
      | Planet.None -> (p * up).Length // p.Z //
      | _ ->
        let sc = getLatLonAlt p planet
        sc.altitude

    let getAltitude (p:V3d) (up:V3d) (planet:Planet) = 
      match planet with
      | Planet.None -> (p * up).Z // p.Z //
      | _ ->
        let sc = getLatLonAlt p planet
        sc.altitude

    let getUpVector (p:V3d) (planet:Planet) = 
      match planet with
      | Planet.None -> V3d.ZAxis
      | _ ->
        let sc = getLatLonAlt p planet
        let height = sc.altitude + 100.0
        
        let v2 = getXYZFromLatLonAlt ({sc with altitude = height}) planet
        (v2 - p).Normalized

        
       