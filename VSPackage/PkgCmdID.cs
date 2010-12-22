// PkgCmdID.cs
// MUST match PkgCmdID.h

namespace MicrosoftResearch.VSPackage
{
    static class PkgCmdIDList
    {
        public const uint cmdidMyCommand =                      0x0100;
        public const uint cmdidVerifyActiveFile =               0x0102;
        public const uint cmdidVerifyCurrentFunction =          0x0104;
        public const uint cmdidCancel =                         0x0105;

        public const uint cmdidContextVerifyActiveFile =        0x0106;
        public const uint cmdidContextVerifyCurrentFunction =   0x0108;
        public const uint cmdidContextCancel =                  0x0110;

        public const uint cmdidVerifyMenu =                     0x1021;

    };
}