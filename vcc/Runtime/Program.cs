using System;
using System.Collections.Generic;
using System.IO;
using System.Resources;
using System.Text;
using Microsoft.Boogie;
using Microsoft.Boogie.AbstractInterpretation;
using Microsoft.Cci;
using Microsoft.Cci.Framework;
using Microsoft.Cci.VerifiedC;

using Framework = Microsoft.Cci.Framework;
using VerifiedC = Microsoft.Cci.VerifiedC;
using Microsoft.Cci.AssertionAdder;

public class VerifiedCCommandLineHost {
  /// <summary>
  /// The main entry point for the application.
  /// </summary>
  [STAThread]
  static int Main(string[] args) {
    if (args == null || args.Length == 0) return 0;
    if (0 < args.Length && args[0] == "/break") {
      string[] newArgs = new string[args.Length - 1];
      Array.Copy(args, 1, newArgs, 0, newArgs.Length);
      args = newArgs;
      System.Diagnostics.Debugger.Break();
    }
    string fileName = args[args.Length - 1];
    if (fileName.EndsWith(".c")) {
      VerifiedCCommandLineHost.Compile(fileName);
    } else if (fileName.EndsWith(".i")) {
      VerifiedCCommandLineHost.TranslateToBpl(fileName);
    } else if (Directory.Exists(fileName)) {
      int errorCount = 0;
      foreach (FileInfo fi in new DirectoryInfo(fileName).GetFiles("*", SearchOption.AllDirectories)) {
        if (!VerifiedCCommandLineHost.RunSuite(fi.Name, new StreamReader(fi.Open(FileMode.Open, FileAccess.Read)), !fi.DirectoryName.Contains("WithErrors")))
        errorCount++;
      }
      if (errorCount != 0)
        Console.WriteLine("\n\n*** {0} error(s) ***\n", errorCount);
    } else {
      VerifiedCCommandLineHost.RunSuite(fileName);
    }
    return 0;
  }

  private static Program StandardPrelude {
    get {
      if (standardPrelude == null)
        standardPrelude = GetStandardPrelude();
      return standardPrelude;
    }
  }
  //^ [Once]
  static Program/*?*/ standardPrelude;

  private static Program GetStandardPrelude() {
    using (System.IO.Stream stream = typeof(Felt2Boogie.ConvertFelt2Boogie).Assembly.GetManifestResourceStream("Felt2Boogie.VerifiedCPrelude.bpl")) {
      BoogiePL.Buffer.Fill(new System.IO.StreamReader(stream));
      BoogiePL.Scanner.Init("<VerifiedCPrelude.bpl>");
      Program prelude;
      int errorCount = BoogiePL.Parser.Parse(out prelude);
      if (prelude == null || errorCount > 0) { return new Program(); } //TODO: error
      return prelude;
    }
  }

  private static void Compile(string fileName) {
    HostEnvironment hostEnvironment = new HostEnvironment();
    hostEnvironment.CompilationErrors += HandleErrors;
    IName name = hostEnvironment.NameTable.GetNameFor(fileName);
    IDictionary<string, string> options = new Dictionary<string, string>(); //TODO: extract from arguments
    List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
    List<IModuleReference> moduleReferences = new List<IModuleReference>();
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity)));
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.VerifiedCRuntimeAssemblyIdentity)));
    StreamReader instream = File.OpenText(fileName);
    List<SourceDocument> programSources = new List<SourceDocument>(1);
    VerifiedC.VerifiedCAssembly assem = new VerifiedC.VerifiedCAssembly(name, Path.GetFullPath(fileName), hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
    VerifiedCCompilationHelper helper = new VerifiedCCompilationHelper((Framework.Compilation)assem.Compilation);
    programSources.Add(new VerifiedCSourceDocument(helper, name, Path.GetFullPath(fileName), instream));
    Felt2Cci.MetadataMapper mapper = new Felt2Cci.MetadataMapper();
    mapper.Visit(assem);
    Felt2Cci.ConvertFelt2Cci converter = new Felt2Cci.ConvertFelt2Cci(assem.Compilation, mapper.metadataMap, assem.Compilation.ContractProvider);
    try {
      converter.Visit(assem);
    } catch {
      return;
    }
    converter.WriteAssembly();
    AssertionAdderVisitor assertionAdder = new AssertionAdderVisitor();
    assertionAdder.Visit(assem);
    Felt2Boogie.ConvertFelt2Boogie boogie = new Felt2Boogie.ConvertFelt2Boogie(assertionAdder.AddedAssertions, assem.Compilation.ContractProvider);
    try {
      boogie.Visit(assem);
      Program p = (Program)boogie.result;
      //Program pp = new Program();
      //pp.TopLevelDeclarations.AddRange(StandardPrelude.TopLevelDeclarations);
      //pp.TopLevelDeclarations.AddRange(p.TopLevelDeclarations);
      //Verify(pp);
      WriteBplFile(p, fileName);
    } catch {
      return;
    }
  }

  public static bool TranslateToExe(string fileName) {
    hasError = false; // each time we clear this flag;
    HostEnvironment hostEnvironment = new HostEnvironment();
    hostEnvironment.CompilationErrors += HandleErrors;
    IName name = hostEnvironment.NameTable.GetNameFor(fileName);
    IDictionary<string, string> options = new Dictionary<string, string>(); //TODO: extract from arguments
    List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
    List<IModuleReference> moduleReferences = new List<IModuleReference>();
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity)));
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.VerifiedCRuntimeAssemblyIdentity)));
    StreamReader instream = File.OpenText(fileName);
    List<SourceDocument> programSources = new List<SourceDocument>(1);
    VerifiedC.VerifiedCAssembly assem = new VerifiedC.VerifiedCAssembly(name, Path.GetFullPath(fileName), hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
    VerifiedCCompilationHelper helper = new VerifiedCCompilationHelper((Framework.Compilation)assem.Compilation);
    programSources.Add(new VerifiedCSourceDocument(helper, name, Path.GetFullPath(fileName), instream));
    Felt2Cci.MetadataMapper mapper = new Felt2Cci.MetadataMapper();
    mapper.Visit(assem);
    Felt2Cci.ConvertFelt2Cci converter = new Felt2Cci.ConvertFelt2Cci(assem.Compilation, mapper.metadataMap, assem.Compilation.ContractProvider);
    try {
      converter.Visit(assem);
    } catch {
      return false;
    }
    if (!hasError) {
      converter.WriteAssembly();
      return true;
    }
    return false;
  }

  private static void TranslateToBpl(string fileName) {
    HostEnvironment hostEnvironment = new HostEnvironment();
    hostEnvironment.CompilationErrors += HandleErrors;
    IName name = hostEnvironment.NameTable.GetNameFor(fileName);
    IDictionary<string, string> options = new Dictionary<string, string>(); //TODO: extract from arguments
    List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
    List<IModuleReference> moduleReferences = new List<IModuleReference>();
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity)));
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.VerifiedCRuntimeAssemblyIdentity)));
    StreamReader instream = File.OpenText(fileName);
    List<SourceDocument> programSources = new List<SourceDocument>(1);
    VerifiedC.VerifiedCAssembly assem = new VerifiedC.VerifiedCAssembly(name, Path.GetFullPath(fileName), hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
    VerifiedCCompilationHelper helper = new VerifiedCCompilationHelper((Framework.Compilation)assem.Compilation);
    programSources.Add(new VerifiedCSourceDocument(helper, name, Path.GetFullPath(fileName), instream));
    AssertionAdderVisitor assertionAdder = new AssertionAdderVisitor();
    assertionAdder.Visit(assem);
    Felt2Boogie.ConvertFelt2Boogie boogie = new Felt2Boogie.ConvertFelt2Boogie(assertionAdder.AddedAssertions, assem.Compilation.ContractProvider);
    try {
      boogie.Visit(assem);
      Program p = (Program)boogie.result;
      WriteBplFile(p, fileName);
    } catch {
      return;
    }
  }

  private static void WriteBplFile(Program program, string fileName) {
    TokenTextWriter writer = new TokenTextWriter(System.IO.Path.ChangeExtension(fileName, ".bpl"));
    program.Emit(writer);
    writer.Close();
  }

  private static void Verify(Program program) {
    //CommandLineOptions.Clo.ProverAst = CommandLineOptions.ProverAST.Let;
    //CommandLineOptions.Clo.ProverAst = CommandLineOptions.ProverAST.Standalone;
    CommandLineOptions.Clo.ProverAst = CommandLineOptions.ProverAST.Typed;
    CommandLineOptions.Clo.vcVariety = CommandLineOptions.VCVariety.Dag;
    CommandLineOptions.Clo.Prover = CommandLineOptions.ProverSelection.Z3;
    CommandLineOptions.Clo.Z3Options = "/la";
    CommandLineOptions.Clo.Z3mam = 4;
    CommandLineOptions.Clo.Bitvectors = CommandLineOptions.BvHandling.ToInt;
    CommandLineOptions.Clo.BracketIdsInVC = 0;
    IErrorSink errorSink = new BoogieErrorSink();
    int numErrors = program.Resolve(errorSink);
    if (numErrors == 0)
      numErrors += program.Typecheck(errorSink);
    if (numErrors == 0) 
    {
      AbstractInterpretation.RunAbstractInterpretation(program);
      VC.VCGen vcgen = new VC.VCGen(program, null, false);
      foreach (Declaration decl in program.TopLevelDeclarations)
      {
        Implementation/*?*/ impl = decl as Implementation;
        if (impl == null) continue;
        List<Counterexample> counterExamples;
        VC.VCGen.Outcome outcome = vcgen.VerifyImplementation(impl, program, out counterExamples);
        ReportOutcome(outcome, impl.Name, counterExamples);
      }
    }
  }

  private static void ReportOutcome(VC.VCGen.Outcome outcome, string methodName, List<Counterexample> counterExamples) {
    switch (outcome) {
      case VC.VCGen.Outcome.Correct:
        Console.WriteLine("Verification of {0} succeeded.", methodName);
        break;
      case VC.VCGen.Outcome.Inconclusive:
        Console.WriteLine("Verification of {0} was inconslusive.", methodName);
        break;
      case VC.VCGen.Outcome.TimedOut:
        Console.WriteLine("Verification of {0} timed out.", methodName);
        break;
      case VC.VCGen.Outcome.Errors:
        Console.WriteLine("Verification of {0} failed.", methodName);
        foreach (Counterexample ce in counterExamples) {
          ReturnCounterexample/*?*/ rce = ce as ReturnCounterexample;
          if (rce != null) {
            Microsoft.Boogie.Token ensTok = rce.FailingEnsures.tok;
            Console.WriteLine("Post condition '{0}' did not verify", ensTok.val);
            Console.WriteLine("Location of post condition: {0}({1},{2})", ensTok.filename, ensTok.line, ensTok.col);
            Microsoft.Boogie.Token retTok = rce.FailingReturn.tok;
            Console.WriteLine("Location of return statement: {0}({1},{2})", retTok.filename, retTok.line, retTok.col);
          }
          AssertCounterexample/*?*/ ace = ce as AssertCounterexample;
          if (ace != null) {
            Microsoft.Boogie.Token assertTok = ace.FailingAssert.tok;
            Console.WriteLine("Assertion '{0}' did not verify", assertTok.val);
            Console.WriteLine("Location of assertion: {0}({1},{2})", assertTok.filename, assertTok.line, assertTok.col);
          }
          CallCounterexample/*?*/ cce = ce as CallCounterexample;
          if (cce != null) {
            Microsoft.Boogie.Token callTok = cce.FailingCall.tok;
            string/*?*/ comment = cce.FailingRequires.Comment;
            if (comment != null) comment = ": " + comment; else comment = "";
            Console.WriteLine("Call '{0}' did not verify{1}", callTok.val, comment);
            Microsoft.Boogie.Token reqTok = cce.FailingRequires.tok;
            Console.WriteLine("Location of pre condition: {0}({1},{2})", reqTok.filename, reqTok.line, reqTok.col);
          }
        }
        break;
    }
  }

  private static bool RunSuite(string suiteName) {
    return VerifiedCCommandLineHost.RunSuite(suiteName, File.OpenText(suiteName), true);
  }

  public static bool RunSuite(string suiteName, TextReader instream, bool verify) {
    System.Diagnostics.Debug.Listeners.Remove("Default");
    HostEnvironment hostEnvironment = new HostEnvironment();
    hostEnvironment.CompilationErrors += HandleErrors;
    StringBuilder source = null;
    StringBuilder expectedOutput = null;
    StringBuilder actualOutput = null;
    List<string> suiteParameters = new List<string>();
    List<string> compilerParameters = null;
    List<string> testCaseParameters = null;
    int errors = 0;
    try {
      int ch = instream.Read();
      int line = 1;
      while (ch >= 0) {
        compilerParameters = new List<string>(suiteParameters);
        bool skipTest = false;
        if (ch == '`') {
          ch = instream.Read();
          bool parametersAreForEntireSuite = false;
          if (ch == '`') {
            parametersAreForEntireSuite = true;
            ch = instream.Read();
          }
          while (ch == '/') {
            //compiler parameters
            StringBuilder cParam = new StringBuilder();
            do {
              cParam.Append((char)ch);
              ch = instream.Read();
            } while (ch != '/' && ch != 0 && ch != 10 && ch != 13);
            for (int i = cParam.Length - 1; i >= 0; i--) {
              if (!Char.IsWhiteSpace(cParam[i])) break;
              cParam.Length = i;
            }
            string cp = cParam.ToString();
            compilerParameters.Add(cp);
          }
          if (parametersAreForEntireSuite)
            suiteParameters.AddRange(compilerParameters);
          if (ch == 13) ch = instream.Read();
          if (ch == 10) {
            line++;
            ch = instream.Read();
            if (parametersAreForEntireSuite && ch == '`') continue;
          }
        }
        if (ch == ':') {
          ch = instream.Read();
          while (ch == '=') {
            //test case parameters
            StringBuilder tcParam = new StringBuilder();
            ch = instream.Read(); //discard =
            while (ch != '=' && ch != 0 && ch != 10 && ch != 13) {
              tcParam.Append((char)ch);
              ch = instream.Read();
            }
            for (int i = tcParam.Length - 1; i >= 0; i--) {
              if (!Char.IsWhiteSpace(tcParam[i])) break;
              tcParam.Length = i;
            }
            if (testCaseParameters == null) testCaseParameters = new List<string>();
            testCaseParameters.Add(tcParam.ToString());
          }
          if (ch == 13) ch = instream.Read();
          if (ch == 10) {
            ch = instream.Read();
            line++;
          }
        }
        source = new StringBuilder();
        while (ch >= 0 && ch != '`') {
          source.Append((char)ch);
          ch = instream.Read();
          if (ch == 10) line++;
        }
        if (ch < 0) {
          Console.WriteLine("The last test case in the suite has not been provided with expected output");
          errors++;
          break;
        }
        ch = instream.Read();
        if (ch == 13) ch = instream.Read();
        if (ch == 10) {
          line++;
          ch = instream.Read();
        }
        int errLine = line;
        expectedOutput = new StringBuilder();
        while (ch >= 0 && ch != '`') {
          expectedOutput.Append((char)ch);
          ch = instream.Read();
          if (ch == 10) line++;
        }
        if (expectedOutput.Length > 0 && expectedOutput[expectedOutput.Length - 1] == 10)
          expectedOutput.Length -= 1;
        if (expectedOutput.Length > 0 && expectedOutput[expectedOutput.Length - 1] == 13)
          expectedOutput.Length -= 1;
        ch = instream.Read();
        if (ch == 13) ch = instream.Read();
        if (ch == 10) {
          ch = instream.Read();
          line++;
        }
        if (skipTest) continue;
        actualOutput = new StringBuilder();
        TextWriter savedOut = Console.Out;
        Console.SetOut(new StringWriter(actualOutput));
        System.Diagnostics.TextWriterTraceListener myWriter = new System.Diagnostics.TextWriterTraceListener(System.Console.Out);
        System.Diagnostics.Debug.Listeners.Add(myWriter);
        try {
          int returnCode = RunTest(hostEnvironment, Path.GetFileNameWithoutExtension(suiteName), source.ToString(), actualOutput, compilerParameters, testCaseParameters, verify);
          if (returnCode != 0)
            actualOutput.Append("Non zero return code: " + returnCode);
        } catch (System.Reflection.TargetInvocationException e) {
          actualOutput.Append(e.InnerException.Message);
        } catch (Exception e) {
          actualOutput.Append(e.Message);
        }
        compilerParameters = null;
        testCaseParameters = null;
        Console.SetOut(savedOut);
        System.Diagnostics.Debug.Listeners.Remove(myWriter);
        if (actualOutput.Length > 0 && actualOutput[actualOutput.Length - 1] == 10)
          actualOutput.Length -= 1;
        if (actualOutput.Length > 0 && actualOutput[actualOutput.Length - 1] == 13)
          actualOutput.Length -= 1;
        if (!expectedOutput.ToString().Equals(actualOutput.ToString())) {
          if (errors++ == 0) Console.WriteLine(suiteName + " failed\n");
          Console.WriteLine("source({0}):", errLine);

          if (source != null)
            Console.WriteLine(source);
          Console.WriteLine("actual output:");
          Console.WriteLine(actualOutput);
          Console.WriteLine("expected output:");
          if (expectedOutput != null)
            Console.WriteLine(expectedOutput);
        }
      }
      instream.Close();
      if (errors == 0)
        Console.WriteLine(suiteName + " passed");
      else {
        Console.WriteLine();
        Console.WriteLine(suiteName + " had " + errors + (errors > 1 ? " failures" : " failure"));
      }
    } catch {
      Console.WriteLine(suiteName + " failed\n");
      Console.WriteLine("source:");
      if (source != null)
        Console.WriteLine(source);
      Console.WriteLine("actual output:");
      Console.WriteLine(actualOutput);
      Console.WriteLine("expected output:");
      if (expectedOutput != null)
        Console.WriteLine(expectedOutput);
    }

    return (errors == 0);
  }

  private static int RunTest(HostEnvironment hostEnvironment, string suiteName, string test, StringBuilder actualOutput, List<string> compilerParameters, List<string> testCaseParameters, bool verify) {
    IName name = hostEnvironment.NameTable.GetNameFor(suiteName);
    IDictionary<string, string> options = new Dictionary<string, string>(); //TODO: extract from params
    List<IAssemblyReference> assemblyReferences = new List<IAssemblyReference>();
    List<IModuleReference> moduleReferences = new List<IModuleReference>();
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.CoreAssemblySymbolicIdentity)));
    assemblyReferences.Add(new AssemblyReference(hostEnvironment.LoadAssembly(hostEnvironment.VerifiedCRuntimeAssemblyIdentity)));
    IUnit unit;
    VerifiedC.VerifiedCAssembly/*?*/ assem = null;
    VerifiedCCompilationHelper helper;
    if (hostEnvironment.previousDocument != null && compilerParameters.Contains("/incremental")) {
      unit = hostEnvironment.GetIncrementalUnit(test);
      helper = (VerifiedCCompilationHelper)hostEnvironment.previousDocument.CompilationPart.Helper;
    } else {
      List<SourceDocument> programSources = new List<SourceDocument>(1);
      assem = new VerifiedC.VerifiedCAssembly(name, "", hostEnvironment, options, assemblyReferences, moduleReferences, programSources);
      helper = new VerifiedCCompilationHelper((Framework.Compilation)assem.Compilation);
      programSources.Add(hostEnvironment.previousDocument = new VerifiedCSourceDocument(helper, name, "", test));
      unit = assem;
    }
    if (assem != null && assem.EntryPoint.ResolvedMethod != Dummy.Method) {
      Felt2Cci.MetadataMapper mapper = new Felt2Cci.MetadataMapper();
      mapper.Visit(assem);
      Felt2Cci.ConvertFelt2Cci converter = new Felt2Cci.ConvertFelt2Cci(assem.Compilation, mapper.metadataMap);
      try {
        converter.Visit(assem);
      } catch (Exception e) {
        if (e == null) return -2;
        //System.Console.Error.WriteLine("Got exception when converting felt to cci: " + e);
        return -1; 
      }
      return converter.RunAssembly(true); //TODO: pass in test case parameters
    }
    AssertionAdderVisitor assertionAdder = new AssertionAdderVisitor();
    assertionAdder.Visit(assem);
    Felt2Boogie.ConvertFelt2Boogie boogie = new Felt2Boogie.ConvertFelt2Boogie(assertionAdder.AddedAssertions, assem.Compilation.ContractProvider);
    try {
      boogie.Visit(assem);
      Program p = (Program)boogie.result;
      if (verify) {
        Program pp = new Program();
        pp.TopLevelDeclarations.AddRange(StandardPrelude.TopLevelDeclarations);
        pp.TopLevelDeclarations.AddRange(p.TopLevelDeclarations);
        Verify(pp);
      }
    } catch (Exception e) {
      if (e == null) return -2;
      //System.Console.WriteLine("Got exception when running boogie: " + e);
      return -1;
    }
    return 0;
  }

  static bool hasError = false;

  private static void HandleErrors(object sender, Microsoft.Cci.ErrorEventArgs args) {
    foreach (IErrorMessage error in args.Errors) {
      if (!error.IsWarning) {
        hasError = true;
      }
      ISourceLocation/*?*/ sourceLocation = error.Location as ISourceLocation;
      if (sourceLocation == null) continue;
      Framework.CompositeSourceDocument/*?*/ compositeDocument = sourceLocation.SourceDocument as Framework.CompositeSourceDocument;
      if (compositeDocument != null) {
        foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sourceLocation)) {
          sourceLocation = sl;
          break;
        }
      }
      IPrimarySourceLocation/*?*/ primarySourceLocation = sourceLocation as IPrimarySourceLocation;
      if (primarySourceLocation == null) continue;
      string docName = primarySourceLocation.SourceDocument.Name.Value;
      int startLine = primarySourceLocation.StartLine;
      int startColumn = primarySourceLocation.StartColumn;
      int endLine = primarySourceLocation.EndLine;
      int endColumn = primarySourceLocation.EndColumn;
      IncludedSourceLocation/*?*/ includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
      if (includedSourceLocation != null) {
        docName = includedSourceLocation.OriginalSourceDocumentName;
        startLine = includedSourceLocation.OriginalStartLine;
        endLine = includedSourceLocation.OriginalEndLine;
      }
      Console.Out.WriteLine("({0},{1})-({2},{3}): {4}: {5}", startLine, startColumn, endLine, endColumn,
        error.IsWarning ? "warning" : "error", error.Message);
      foreach (ILocation relatedLocation in error.RelatedLocations) {
        ISourceLocation/*?*/ sloc = relatedLocation as ISourceLocation;
        if (sloc != null) {
          compositeDocument = sloc.SourceDocument as Framework.CompositeSourceDocument;
          if (compositeDocument != null) {
            foreach (ISourceLocation sl in compositeDocument.GetFragmentLocationsFor(sloc)) {
              sloc = sl;
              break;
            }
          }
          primarySourceLocation = sloc as IPrimarySourceLocation;
          if (primarySourceLocation == null) continue;
          docName = primarySourceLocation.SourceDocument.Name.Value;
          startLine = primarySourceLocation.StartLine;
          startColumn = primarySourceLocation.StartColumn;
          endLine = primarySourceLocation.EndLine;
          endColumn = primarySourceLocation.EndColumn;
          includedSourceLocation = primarySourceLocation as IncludedSourceLocation;
          if (includedSourceLocation != null) {
            docName = includedSourceLocation.OriginalSourceDocumentName;
            startLine = includedSourceLocation.OriginalStartLine;
            endLine = includedSourceLocation.OriginalEndLine;
          }
          Console.Out.WriteLine("({0},{1})-({2},{3}): (Location of symbol related to previous {4}.)", startLine, startColumn, endLine, endColumn, error.IsWarning ? "warning" : "error");
        }
        //TODO: deal with non source locations
      }
    }
  }
}

class BoogieErrorSink : IErrorSink {
  public void Error(Microsoft.Boogie.Token tok, string msg) {
    Console.Out.WriteLine("({0},{1}): verification: {2}", tok.line, tok.col, msg);
  }
}

internal class HostEnvironment : Framework.SourceEditHostEnvironment {

  internal HostEnvironment()
    : base(new NameTable(), 8) {
    this.moduleReadWriteFactory = new Microsoft.Cci.ModuleReadWrite.ModuleReadWriteFactory(this);
    string/*?*/ loc = typeof(object).Assembly.Location;
    if (loc == null) loc = "";
    this.mscorlibIdentity =
      new AssemblyIdentity(this.NameTable.GetNameFor("mscorlib"), "", new Version(2, 0, 0, 0), new byte[] { 0xB7, 0x7A, 0x5C, 0x56, 0x19, 0x34, 0xE0, 0x89 }, loc);
    this.RegisterAsLatest(this.moduleReadWriteFactory.OpenAssembly(BinaryDocument.GetBinaryDocumentForFile(this.mscorlibIdentity.Location, this)));
    loc = typeof(Microsoft.VerifiedC.Runtime).Assembly.Location;
    if (loc == null) loc = "";
    this.verifiedCRuntimeAssemblyIdentity =
      new AssemblyIdentity(this.NameTable.GetNameFor("Microsoft.VerifiedC.Runtime"), "", new Version(1, 0, 0, 0), new byte[] { 0x73, 0x64, 0x40, 0xC9, 0xB4, 0x14, 0xEA, 0x16 }, loc);
    this.RegisterAsLatest(this.moduleReadWriteFactory.OpenAssembly(BinaryDocument.GetBinaryDocumentForFile(this.verifiedCRuntimeAssemblyIdentity.Location, this)));
  }

  internal IUnit GetIncrementalUnit(string newText) {
    string[] lines = newText.Split('$');
    if (lines.Length != 4) return Dummy.Unit;
    string prefix = lines[0];
    string textToReplace = lines[1];
    string replacement = lines[2];
    IVerifiedCSourceDocument updatedDocument = this.previousDocument.GetUpdatedDocument(prefix.Length, textToReplace.Length, replacement);
    return updatedDocument.VerifiedCCompilationPart.Compilation.Result;
  }

  internal VerifiedCSourceDocument/*?*/ previousDocument;

  public override AssemblyIdentity CoreAssemblySymbolicIdentity {
    get { return this.mscorlibIdentity; }
  }
  readonly AssemblyIdentity mscorlibIdentity;

  public AssemblyIdentity VerifiedCRuntimeAssemblyIdentity {
    get { return this.verifiedCRuntimeAssemblyIdentity; }
  }
  readonly AssemblyIdentity verifiedCRuntimeAssemblyIdentity;

  public override IUnit LoadUnitFrom(string location) {
    IUnit result = this.moduleReadWriteFactory.OpenModule(BinaryDocument.GetBinaryDocumentForFile(location, this));
    this.RegisterAsLatest(result);
    return result;
  }

  Microsoft.Cci.ModuleReadWrite.ModuleReadWriteFactory moduleReadWriteFactory;

  public override void ReportErrors(Microsoft.Cci.ErrorEventArgs errorEventArguments) {
    this.SynchronousReportErrors(errorEventArguments);
  }
}
