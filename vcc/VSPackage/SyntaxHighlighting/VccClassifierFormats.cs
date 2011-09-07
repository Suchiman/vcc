using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;


namespace Microsoft.Research.Vcc.VSPackage
{
    internal static class VccClassifierFormats
    {
        private const double Opacity = 0.6;

        [Export(typeof (EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = VccClassificationTypeDefinitions.SpecType)]
        [Name(VccClassificationTypeDefinitions.SpecType)]
        internal sealed class VccSpecFormat : ClassificationFormatDefinition
        {
            public VccSpecFormat()
            {
                this.ForegroundOpacity = Opacity;
            }
        }

        [Export(typeof (EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = VccClassificationTypeDefinitions.KeywordType)]
        [Name(VccClassificationTypeDefinitions.KeywordType)]
        internal sealed class VccKeywordFormat : ClassificationFormatDefinition
        {
            public VccKeywordFormat()
            {
                this.ForegroundOpacity = Opacity;
            }
        }
    }
}
