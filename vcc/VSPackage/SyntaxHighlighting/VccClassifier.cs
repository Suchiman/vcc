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
        private readonly IClassificationType specType;
        private readonly IClassificationType keywordType;

        internal VccClassifier(IClassificationTypeRegistryService registry)
        {
          this.keywordType = registry.GetClassificationType(VccClassificationTypeDefinitions.KeywordType);
          this.specType= registry.GetClassificationType(VccClassificationTypeDefinitions.SpecType);
        }

        private static readonly IList<ClassificationSpan> emptyClassification = new ClassificationSpan[] { };

        private readonly Dictionary<ITextBuffer, Tuple<int, FSharp.Collections.FSharpList<SyntaxHighlighting.Ast.Span>>> classificationCache = new Dictionary<ITextBuffer, Tuple<int, FSharp.Collections.FSharpList<SyntaxHighlighting.Ast.Span> >>();

        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {
          if (span.Length == 0) return emptyClassification;
          // classifications are cached by textbuffer and invalidated as the version of the snapshot changes
          Tuple<int, FSharp.Collections.FSharpList<SyntaxHighlighting.Ast.Span>> cacheEntry;
          FSharp.Collections.FSharpList<SyntaxHighlighting.Ast.Span> cachedSpans;

          if (this.classificationCache.TryGetValue(span.Snapshot.TextBuffer, out cacheEntry) && cacheEntry.Item1 == span.Snapshot.Version.VersionNumber) {
            cachedSpans = cacheEntry.Item2;
          } else {
            // previously unknown or version changed
            try {
              cachedSpans = SyntaxHighlighting.Parser.Parse(span.Snapshot.GetText());
              this.classificationCache[span.Snapshot.TextBuffer] = Tuple.Create(span.Snapshot.Version.VersionNumber, cachedSpans);
            } catch {
              // errors in syntax highlighting should not bring down VS
              return emptyClassification;
            }
          }

          // return list of detected spans filtered to those that overlap the given span
          List<ClassificationSpan> result = new List<ClassificationSpan>();
          foreach (var pos in cachedSpans) {
            if (pos.IsSpec) {
              var spec = (SyntaxHighlighting.Ast.Span.Spec)pos;
              var specSpan = new Span(spec.Item1, spec.Item2);
              if (span.OverlapsWith(specSpan)) result.Add(new ClassificationSpan(new SnapshotSpan(span.Snapshot, specSpan), specType));
            } else if (pos.IsKeyword) {
              var kw = (SyntaxHighlighting.Ast.Span.Keyword)pos;
              var kwSpan = new Span(kw.Item1, kw.Item2);
              if (span.OverlapsWith(kwSpan)) result.Add(new ClassificationSpan(new SnapshotSpan(span.Snapshot, kwSpan), keywordType));
            }
          }

          return result;
        }

#pragma warning disable 67
        // This event gets raised if a non-text change would affect the classification in some way,
        // for example typing /* would cause the classification to change in C# without directly
        // affecting the span.
        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
#pragma warning restore 67
    }
}