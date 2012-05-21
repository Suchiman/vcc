// PkgCmdID.cs
// MUST match PkgCmdID.h

namespace Microsoft.Research.Vcc.VSPackage
{
    internal static class PkgCmdIDList
    {
        public const uint CmdidVerifyActiveFile = 0x0102;
        public const uint CmdidVerifyActiveFileWithoutIncludes = 0x0103;
        public const uint CmdidCancel = 0x0105;
        public const uint CmdidContextVerifyActiveFile = 0x0106;
        public const uint CmdidContextVerifyActiveFileWithoutIncludes = 0x0107;
        public const uint CmdidContextCustomVerify = 0x0109;
        public const uint CmdidContextCancel = 0x0110;
        public const uint CmdidCustomVerify = 0x0111;
        public const uint CmdidReVerify = 0x0112;
        public const uint CmdidContextReVerify = 0x0113;
        public const uint CmdidContextVerifyThis = 0x0114;
        public const uint CmdidVerifyThis = 0x0115;
        public const uint CmdidOptions = 0x0116;
        public const uint CmdidShowErrorModel = 0x0117;

        public const uint CmdidVerifyMenu = 0x1021;
        public const uint CmdidContextMenuGroup = 0x1022;

        public const uint CmdidMathSymbolForall = 0x0220;
        public const uint CmdidMathSymbolExists = 0x0221;
        public const uint CmdidMathSymbolIn = 0x0222;
        public const uint CmdidMathSymbolUnion = 0x0223;
        public const uint CmdidMathSymbolIntersection = 0x0224;
        public const uint CmdidMathSymbolLambda = 0x0225;
    }
}
