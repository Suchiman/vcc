// PkgCmdID.cs
// MUST match PkgCmdID.h

namespace MicrosoftResearch.VSPackage
{
    static class PkgCmdIDList
    {
        public const uint cmdidVerifyActiveFile =                           0x0102;
        public const uint cmdidCheckAdmissibilityOfCurrentStruct =          0x0103;
        public const uint cmdidVerifyCurrentFunction =                      0x0104;
        public const uint cmdidVerifyThis =                                 0x0115;
        public const uint cmdidReVerify =                                   0x0112;
        public const uint cmdidCustomVerify =                               0x0111;
        public const uint cmdidCancel =                                     0x0105;
        public const uint cmdidOptions =                                    0x0116;

        public const uint cmdidContextVerifyActiveFile =                    0x0106;
        public const uint cmdidContextCheckAdmissibilityOfCurrentStruct =   0x0107;
        public const uint cmdidContextVerifyCurrentFunction =               0x0108;
        public const uint cmdidContextVerifyThis =                          0x0114;
        public const uint cmdidContextReVerify =                            0x0113;
        public const uint cmdidContextCustomVerify =                        0x0109;
        public const uint cmdidContextCancel =                              0x0110;
        public const uint cmdidContextOptions =                             0x0117;
        public const uint cmdidVerifyMenu =                                 0x1021;

        public const uint cmdidContextMenuGroup =                           0x1022;

    };
}