using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.Research.Vcc.VSPackage
{

    #region Provider definition
    /// <summary>
    /// This class causes a classifier to be added to the set of classifiers. Since 
    /// the content type is set to "text", this classifier applies to all text files
    /// </summary>
    [Export(typeof(IClassifierProvider))]
    [ContentType("text")]
    internal class VccClassifierProvider : IClassifierProvider
    {
        /// <summary>
        /// Import the classification registry to be used for getting a reference
        /// to the custom classification type later.
        /// </summary>
        [Import]
// ReSharper disable RedundantDefaultFieldInitializer
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF
// ReSharper restore RedundantDefaultFieldInitializer

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            return buffer.Properties.GetOrCreateSingletonProperty(() => new VccClassifier(ClassificationRegistry));
        }
    }
    #endregion //provider def

    #region Classifier
    /// <summary>
    /// Classifier that classifies all text as an instance of the OrinaryClassifierType
    /// </summary>
    internal class VccClassifier : IClassifier
    {
        readonly IClassificationType _classificationType;

        readonly static char[] SplitChars = new char[] { ' ', '\t' };

        internal VccClassifier(IClassificationTypeRegistryService registry)
        {
            _classificationType = registry.GetClassificationType("vcc.spec");
        }

        /// <summary>
        /// This method scans the given SnapshotSpan for potential matches for this classification.
        /// In this instance, it classifies everything and returns each span as a new ClassificationSpan.
        /// </summary>
        /// <param name="span">The span currently being classified</param>
        /// <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {

            var snapshot = span.Snapshot;

            if (snapshot.Length == 0) return new ClassificationSpan[] {};

            var snapshotText = snapshot.GetText();
            var positions = SyntaxHighlighting.Parser.Parse(snapshotText);

            var classifications = new List<ClassificationSpan>(positions.Length);

            foreach (var pos in positions)
            {
                classifications.Add(
                    new ClassificationSpan(new SnapshotSpan(snapshot, pos.Item1.Column,
                                                            pos.Item2.Column - pos.Item1.Column + 1), _classificationType));
            }

            return classifications;
        }

#pragma warning disable 67
        // This event gets raised if a non-text change would affect the classification in some way,
        // for example typing /* would cause the classification to change in C# without directly
        // affecting the span.
        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
#pragma warning restore 67
    }
    #endregion //Classifier
}
