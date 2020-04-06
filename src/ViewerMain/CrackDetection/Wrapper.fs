namespace CrackDetection

open Aardvark.Base

module Wrapper =

    module private CrackDetectionDll =
        open System.Runtime.InteropServices

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "GetDllVersion", CallingConvention = CallingConvention.Cdecl)>]
        extern uint32 GetDllVersion()

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "Init", CallingConvention = CallingConvention.Cdecl)>]
        extern int32 Init(string configDirectory, string logDirectory)

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "DeInit", CallingConvention = CallingConvention.Cdecl)>]
        extern void DeInit()

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "EnableDebugLogging", CallingConvention = CallingConvention.Cdecl)>]
        extern void EnableDebugLogging(bool enable)

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "FindCrack", CallingConvention = CallingConvention.Cdecl)>]
        extern int32 FindCrack(float32[] crackCoefficients, uint32 width, uint32 height, V2f[] controlPoints, uint32 numControlPoints, uint32& numCrackPoints);

        [<DllImport(@".\crackDetection\CrackDetection.dll", EntryPoint = "GetCrack", CallingConvention = CallingConvention.Cdecl)>]
        extern int32 GetCrack(V2f* pCrackPoints)


    let private check (origin : string) (status : int32) =
        if status <> 0 then
            failwith (sprintf "CrackDetectionDll.%s() returned error code %d" origin status)

    /// Returns the version of the crack detection library.
    let getLibraryVersion =
        CrackDetectionDll.GetDllVersion

    /// Initializes the crack detection library.
    /// Throws an exception on failure.
    let initialize configDirectory logDirectory =
        CrackDetectionDll.Init(configDirectory, logDirectory)
            |> check "Init"

    /// Cleans up resources used by the crack detection library.
    let free =
        CrackDetectionDll.DeInit

    /// Enables or disables debug logging.
    let setDebugLogging enabled =
        CrackDetectionDll.EnableDebugLogging enabled

    /// Finds crack points for the given coefficients and control points.
    /// Throws an exception if an error occurs.
    let findCrack (width : uint32) (height : uint32) (coefficients : float32[]) (controlPoints : V2f[]) =
        let mutable numCrackPoints = 0u

        // Find crack points and return count
        CrackDetectionDll.FindCrack(
                coefficients, width, height,
                controlPoints, uint32 controlPoints.LongLength,
                &numCrackPoints
            ) |> check "FindCrack"

        // Allocate array and retrieve points
        let n = int numCrackPoints

        if n > 0 then
            let ptr = NativePtr.alloc<V2f> n

            try
                CrackDetectionDll.GetCrack(ptr) |> check "GetCrack"
                ptr |> NativePtr.toArray n
            finally
                NativePtr.free ptr
        else
            Array.empty