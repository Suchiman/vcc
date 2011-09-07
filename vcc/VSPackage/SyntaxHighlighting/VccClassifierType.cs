using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.Research.Vcc.VSPackage
{
    internal static class VccClassificationTypeDefinitions
    {

        // http://msdn.microsoft.com/en-us/library/microsoft.visualstudio.language.standardclassification.predefinedclassificationtypenames.aspx


        [Export]
        [Name("vcc")]
        [BaseDefinition(Microsoft.VisualStudio.Language.StandardClassification.PredefinedClassificationTypeNames.Comment)]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static ContentTypeDefinition vccContentTypeDefinition = null;
// ReSharper restore RedundantDefaultFieldInitializer

        [Export]
        [FileExtension(".c")]
        [ContentType("vcc")]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static FileExtensionToContentTypeDefinition vccSourceFileExtension = null;
// ReSharper restore RedundantDefaultFieldInitializer

        [Export]
        [FileExtension(".h")]
        [ContentType("vcc")]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static FileExtensionToContentTypeDefinition vccHeaderFileExtension = null;
// ReSharper restore RedundantDefaultFieldInitializer

        [Export]
        [Name("vcc")]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static ClassificationTypeDefinition vccClassificationDefinition = null;
// ReSharper restore RedundantDefaultFieldInitializer

        [Export]
        [Name("vcc.spec")]
        [BaseDefinition("vcc")]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static ClassificationTypeDefinition vccSpecDefinition = null;
// ReSharper restore RedundantDefaultFieldInitializer
    }
}
