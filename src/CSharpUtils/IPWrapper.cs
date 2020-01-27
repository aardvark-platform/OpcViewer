
using Aardvark.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;

namespace IPWrappers
{
    public class CrackDetectionWrappers
    {
        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern uint GetDllVersion();

        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern int Init(string configDir, string logDir);

        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern void DeInit();
        
        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern uint GetNrOfAvailablePlatforms();

        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern void EnableDebugLogging(bool enable);

        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern int FindCrack(float[] pfCrackCoefficients, int nWidth, int nHeight, SPoint2D[] poControlPoints, int nNrOfControlPoints, ref int pnNrOfCrackPoints );

        [DllImport(@".\crackDetection\CrackDetection.dll")]
        public static extern int GetCrack([MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] IntPtr[] pCrackPoints); // (ref SPoint2D pCrackPoints );
       

        [StructLayout(LayoutKind.Sequential)]
        public struct SPoint2D
        {
            public double m_dX;
            public double m_dY;

            public static SPoint2D Default()
            {
                return new SPoint2D()
                {
                    m_dX = 0.0,
                    m_dY = 0.0
                };
            }
        };

        public static volatile Dictionary<IntPtr, IntPtr[]> s_childPointerLut = new Dictionary<IntPtr, IntPtr[]>();

        /// <summary>
        /// Marshals array of structs to a pointer. In this process global memory is allocated 
        /// and the resulting pointer is returned.
        /// </summary>
        public static IntPtr MarshalArray<T>(T[] input) where T : struct
        {
            // compute sizes
            var count = input.Length;
            var sizeOfOne = Marshal.SizeOf(typeof(T));
            var arraySize = sizeOfOne * count;

            // allocate mem and get buffer pointer
            var arrayPtr = Marshal.AllocHGlobal(arraySize);
            var intPtrs = new IntPtr[count];

            //first ptr is array ptr
            var runningPtr = arrayPtr;
            for (int i = 0; i < count; i++)
            {
                intPtrs[i] = runningPtr;
                Marshal.StructureToPtr(input[i], runningPtr, false);
                runningPtr = new IntPtr((long)runningPtr + sizeOfOne);
            }

            s_childPointerLut.Add(arrayPtr, intPtrs);

            return arrayPtr;
        }

    }
}
