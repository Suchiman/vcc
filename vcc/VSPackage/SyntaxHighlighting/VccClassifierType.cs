﻿using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.Research.Vcc.VSPackage
{
    internal static class VccClassificationTypeDefinitions
    {
        internal const string SpecType = "vcc.spec";
        internal const string KeywordType = "vcc.keyword";


        [Export]
        [Name("vcc")]
        [BaseDefinition("C/C++")]
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
        [Name(SpecType)]
        [BaseDefinition(Microsoft.VisualStudio.Language.StandardClassification.PredefinedClassificationTypeNames.FormalLanguage)]
        // ReSharper disable RedundantDefaultFieldInitializer
        internal static ClassificationTypeDefinition vccSpecDefinition = null;
// ReSharper restore RedundantDefaultFieldInitializer

        [Export]
        [Name(KeywordType)]
        [BaseDefinition(Microsoft.VisualStudio.Language.StandardClassification.PredefinedClassificationTypeNames.Keyword)]
// ReSharper disable RedundantDefaultFieldInitializer
        internal static ClassificationTypeDefinition vccKeywordDefinition = null;
// ReSharper restore RedundantDefaultFieldInitializer
    }
}