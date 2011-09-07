using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;


namespace Microsoft.Research.Vcc.VSPackage
{
    internal static class VccClassifierFormats
    {
        [Export(typeof (EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "vcc.spec")]
        [Name("vcc.spec")]
        internal sealed class VccSpecFormat : ClassificationFormatDefinition
        {
            public VccSpecFormat()
            {
                this.ForegroundOpacity = 0.5;
                //this.BackgroundColor = Colors.BurlyWood;
            }
        }
    }
}
