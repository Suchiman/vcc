namespace Microsoft.Research.Vcc.VSPackage
{
    using System;
    using System.Runtime.InteropServices;

    internal class NativeMethods
    {
        [DllImport("user32.dll")]
        static internal extern IntPtr GetForegroundWindow(); 
    }
}
