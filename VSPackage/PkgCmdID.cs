// PkgCmdID.cs
// MUST match PkgCmdID.h
using System;

namespace MicrosoftResearch.VSPackage
{
    static class PkgCmdIDList
    {
        public const uint cmdidMyCommand =        0x100;
        public const uint cmdidVerifyActiveFile = 0x102;
        public const uint cmdidVerifyCurrentFunction = 0x0104;
    };
}