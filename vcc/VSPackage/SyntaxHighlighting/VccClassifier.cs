using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.Research.Vcc.VSPackage
{
    [Export(typeof(IClassifierProvider))]
    [ContentType("C/C++")]
    internal class VccClassifierProvider : IClassifierProvider
    {
        [Import]
// ReSharper disable RedundantDefaultFieldInitializer
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF
// ReSharper restore RedundantDefaultFieldInitializer

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            return buffer.Properties.GetOrCreateSingletonProperty(() => new VccClassifier(ClassificationRegistry));
        }
    }

    internal class VccClassifier : IClassifier
    {
        private readonly IClassificationTypeRegistryService registry;

        internal VccClassifier(IClassificationTypeRegistryService registry)
        {
            this.registry = registry;
        }

        private static readonly IList<ClassificationSpan> emptyClassification = new ClassificationSpan[] { };

        private readonly Dictionary<ITextBuffer, Tuple<int, IList<ClassificationSpan>>> classificationCache = new Dictionary<ITextBuffer, Tuple<int, IList<ClassificationSpan>>>();

        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {
          if (span.Length == 0) return emptyClassification;

          // classifications are cached by textbuffer and invalidated as the version of the snapshot changes
          Tuple<int, IList<ClassificationSpan>> cacheEntry;
          if (this.classificationCache.TryGetValue(span.Snapshot.TextBuffer, out cacheEntry) && cacheEntry.Item1 == span.Snapshot.Version.VersionNumber) {
            return cacheEntry.Item2;
          } else {
            // previously unknown or version changed
            try {
              var newClassification = GetClassificationSpans(span.Snapshot);
              this.classificationCache[span.Snapshot.TextBuffer] = new Tuple<int,IList<ClassificationSpan>>(span.Snapshot.Version.VersionNumber, newClassification);
              return newClassification;
            } catch {
              // errors in syntax highlighting should not bring down VS
              return emptyClassification;
            }
          }
        }

        private IList<ClassificationSpan> GetClassificationSpans(ITextSnapshot snapshot)
        {
            var specType = this.registry.GetClassificationType(VccClassificationTypeDefinitions.SpecType);
            var keywordType = this.registry.GetClassificationType(VccClassificationTypeDefinitions.KeywordType);

            var positions = SyntaxHighlighting.Parser.Parse(snapshot.GetText());
            var classifications = new List<ClassificationSpan>(positions.Length);

            foreach (var pos in positions)
            {
                if (pos.IsSpec)
                {
                    var spec = (SyntaxHighlighting.Ast.Span.Spec) pos;
                    classifications.Add(
                        new ClassificationSpan(new SnapshotSpan(snapshot, spec.Item1, spec.Item2), specType));
                }
                else if (pos.IsKeyword)
                {
                    var kw = (SyntaxHighlighting.Ast.Span.Keyword) pos;
                    classifications.Add(
                        new ClassificationSpan(new SnapshotSpan(snapshot, kw.Item1, kw.Item2), keywordType));
                }
            }

            return classifications.AsReadOnly();
        }

#pragma warning disable 67
        // This event gets raised if a non-text change would affect the classification in some way,
        // for example typing /* would cause the classification to change in C# without directly
        // affecting the span.
        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
#pragma warning restore 67
    }
}