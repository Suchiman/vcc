﻿using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Boogie;
using System.IO;
using Microsoft.Boogie.AbstractInterpretation;

namespace Microsoft.Research.Vcc
{
  class Vcc2FunctionVerifier : FunctionVerifier
  {
    Microsoft.FSharp.Collections.FSharpList<CAST.Top> currentDecls;
    Vcc2Plugin parent;
    Helper.Env env;
    Boogie.Program currentBoogie;
    VC.VCGen vcgen;
    bool errorMode;
    VcOpt.Optimizer vcopt;
    VcOpt.DummyOpt dummyOpt;
    List<string> options = new List<string>();
    string[] standardBoogieOptions = new string[] { 
      // report up to 10 errors
      "/errorLimit:10", 
      // use monomorphic type encoding (i.e. no type encoding)
      "/typeEncoding:m",
      // this defaults to 100 and causes Boogie to kill the prover after it is done, but has exceeded 100M; we instead use the /memory: switch to Z3
      "/proverMemoryLimit:0",
      // print prover warnings to stdout
      "/proverWarnings:1",
      // skip AI (inference of variable ranges mostly)
      //"/noinfer",  
    };
        
    internal Vcc2FunctionVerifier(Vcc2Plugin parent, Microsoft.FSharp.Collections.FSharpList<CAST.Top> currentDecls, Helper.Env env)
      : base(env, currentDecls)
    {
      this.currentDecls = currentDecls;
      this.parent = parent;
      this.env = env;
      this.InitBoogie();
    }

    public override Microsoft.FSharp.Collections.FSharpList<string> FunctionsToVerify()
    {
      return this.FindAllFunctions(currentDecls);
    }

    public override void Close()
    {
      base.Close();
      CloseVcGen();
    }

    private static string/*?*/ GetExtraFunctionOptions(Implementation impl)
    {
      string res = impl.FindStringAttribute("vcc_extra_options");
      if (res == null)
        res = impl.Proc.FindStringAttribute("vcc_extra_options");
      return res;
    }

    private bool ReParseBoogieOptions(List<string> options, bool runningFromCommandLine)
    {
      List<string> optionsWithMemSetting = null;
      foreach (string option in options)
        if (option.StartsWith("/z3opt:/memory:", StringComparison.Ordinal)) {
          optionsWithMemSetting = options;
          break;
        }
      if (optionsWithMemSetting == null) {
        optionsWithMemSetting = new List<string>(options);
        int memval = 128;
        if (parent.options.Z3Version < 2)
          memval *= 1024 * 1024;
        optionsWithMemSetting.Add("/z3opt:/memory:" + memval);
      }
      CommandLineOptions.Clo = new CommandLineOptions();
      CommandLineOptions.Clo.RunningBoogieFromCommandLine = runningFromCommandLine;
      return CommandLineOptions.Clo.Parse(optionsWithMemSetting.ToArray()) == 1;
    }

    private void InitBoogie()
    {
      options.AddRange(standardBoogieOptions);
      options.Add("/proverOpt:V" + parent.options.Z3Version);
      if (parent.ModelFileName != null) {
        options.Add("/printModel:1");
        options.Add("/printModelToFile:" + parent.ModelFileName);
      }
      options.AddRange(parent.options.BoogieOptions);
      foreach (string z3option in parent.options.Z3Options)
        options.Add("/z3opt:" + z3option);
      if (!ReParseBoogieOptions(options, parent.options.RunningFromCommandLine)) return;
    }

    public override VerificationResult Verify(string funcName)
    {
      double start = Vcc2CommandLineHost.GetTime();

      if (parent.options.AggressivePruning) {
        var decls = TransUtil.pruneBy(env, funcName, currentDecls);
        var boogieDecls = Translator.translate(funcName, env, decls);
        if (!env.ShouldContinue) return VerificationResult.UserError;
        PrepareBoogie(boogieDecls);
      } else {
        if (currentBoogie == null) {
          var boogieDecls = Translator.translate(null, env, currentDecls);
          if (!env.ShouldContinue) return VerificationResult.UserError;
          PrepareBoogie(boogieDecls);
        }
      }

      Implementation impl = null;
      foreach (Declaration decl in currentBoogie.TopLevelDeclarations) {
        impl = decl as Implementation;
        if (impl != null && impl.Name == funcName) break;
        impl = null;
      }
      if (impl == null) {
        Console.WriteLine("cannot find function: {0}", funcName);
        return VerificationResult.UserError;
      }

      if (this.errorMode || ! env.ShouldContinue) return VerificationResult.UserError;

      if (impl.SkipVerification) return VerificationResult.Skipped;

      if (!parent.options.XmlFormatOutput && !parent.options.VCLikeErrorMessages)
        Console.Write("Verification of {0} ", funcName);

      string logPath = CommandLineOptions.Clo.SimplifyLogFilePath;
      if (logPath != null)
        logPath = logPath.Replace("@VCCFILE@", Vcc2CommandLineHost.currentTestcaseName);
      if (logPath != null && logPath.Contains("@VCCFUNC@")) {
        logPath = logPath.Replace("@VCCFUNC@", funcName.Replace("$", "_").Replace("^", "_"));
        CloseVcGen();
      }

      string extraFunctionOptions = null;
      if (parent.options.RunInBatchMode && (extraFunctionOptions = GetExtraFunctionOptions(impl)) != null) {
        CloseVcGen();
        VccOptions extraCommandLineOptions = OptionParser.ParseCommandLineArguments(Vcc2CommandLineHost.hostEnvironment, extraFunctionOptions.Split(' ', '\t'), false);
        List<string> effectiveOptions = new List<string>(extraCommandLineOptions.BoogieOptions);
        foreach (string z3option in extraCommandLineOptions.Z3Options)
          effectiveOptions.Add("/z3opt:" + z3option);
        effectiveOptions.AddRange(options);
        if (!ReParseBoogieOptions(effectiveOptions, parent.options.RunningFromCommandLine)) {
          Console.WriteLine("Error parsing extra options '{0}' for function '{1}'", extraFunctionOptions, impl.Name);
          return VerificationResult.UserError;
        }
        try {
          parent.swBoogie.Start();
          vcgen = new VC.VCGen(currentBoogie, logPath, CommandLineOptions.Clo.SimplifyLogFileAppend);
        } finally {
          parent.swBoogie.Stop();
        }
      } else if (vcgen == null) {
        // run with default options
        ReParseBoogieOptions(options, parent.options.RunningFromCommandLine);
        try {
          parent.swBoogie.Start();
          vcgen = new VC.VCGen(currentBoogie, logPath, CommandLineOptions.Clo.SimplifyLogFileAppend);
        } finally {
          parent.swBoogie.Stop();
        }
      }

      var reporter = new ErrorReporter(parent.options, impl.Proc.Name, start);

      bool hasStartHere = false;
      try {
        parent.swVcOpt.Start();
        if (vcopt != null) {
          impl = vcopt.RoundTrip(impl);
        } else if (impl.Proc.CheckBooleanAttribute("has_start_here", ref hasStartHere) && hasStartHere) {
          if (dummyOpt == null) {
            dummyOpt = new VcOpt.DummyOpt(currentBoogie, env, Microsoft.FSharp.Collections.ListModule.of_seq(parent.options.VcOpt));
            dummyOpt.RemoveExpansionAxioms();
          }
          impl = dummyOpt.RoundTrip(impl);
        }
      } finally {
        parent.swVcOpt.Stop();
      }

      
      VC.VCGen.Outcome outcome;

      try {
        parent.swVerifyImpl.Start();
        outcome = vcgen.VerifyImplementation(impl, currentBoogie, reporter);
      } finally {
        parent.swVerifyImpl.Stop();
      }

      if (extraFunctionOptions != null) {
        CloseVcGen();
      }

      reporter.PrintSummary(outcome);

      switch (outcome) {
        case VC.VCGen.Outcome.Correct: return VerificationResult.Succeeded;
        case VC.VCGen.Outcome.Errors: return VerificationResult.Failed;
        case VC.VCGen.Outcome.Inconclusive: return VerificationResult.Inconclusive;
        case VC.VCGen.Outcome.OutOfMemory: return VerificationResult.Crashed;
        case VC.VCGen.Outcome.TimedOut: return VerificationResult.Crashed;
        default: return VerificationResult.Crashed;
      }
    }

    private void CloseVcGen()
    {
      if (vcgen != null) {
        try {
          parent.swBoogie.Start();
          vcgen.Close();
        } finally {
          parent.swBoogie.Stop();
        }
        vcgen = null;
      }
    }

    public override void DumpInternalsToFile(string fn, bool generate) {
      if (generate) {
        var boogieDecls = Translator.translate(null, env, currentDecls);
        PrepareBoogie(boogieDecls);
      }
      using (TokenTextWriter writer = new TokenTextWriter(Path.ChangeExtension(fn, (bplFileCounter++).ToString() + ".bpl")))
        currentBoogie.Emit(writer);
    }

    long bplFileCounter = 0;

    private void PrepareBoogie(Microsoft.FSharp.Collections.FSharpList<BoogieAST.Decl> boogieDecls)
    {
      currentBoogie = parent.GetBoogieProgram(boogieDecls);
      CloseVcGen();
      CommandLineOptions.Clo.Parse(standardBoogieOptions);
      IErrorSink errorSink = new BoogieErrorSink(parent.options.NoPreprocessor);

      int numErrors;

      try {
        parent.swBoogieResolve.Start();
        numErrors = currentBoogie.Resolve(errorSink);
      } finally {
        parent.swBoogieResolve.Stop();
      }
      if (numErrors == 0) {
        try {
          parent.swBoogieTypecheck.Start();
          numErrors = currentBoogie.Typecheck(errorSink);
        } finally {
          parent.swBoogieTypecheck.Stop();
        }
      }
      if (numErrors == 0) {
        try {
          parent.swBoogieAI.Start();
          AbstractInterpretation.RunAbstractInterpretation(currentBoogie, null);
        } finally {
          parent.swBoogieAI.Stop();
        }
      }

      if (numErrors != 0) {
        if (!parent.options.RunTestSuite) {
          Console.WriteLine("attempting to dump BPL to buggy.bpl");
          using(TokenTextWriter writer = new TokenTextWriter("buggy.bpl"))
            currentBoogie.Emit(writer);
        }
        errorMode = true;
      } else {
        if (parent.options.VcOpt.Count > 0) {
          try {
            parent.swVcOpt.Start();
            vcopt = new VcOpt.Optimizer(currentBoogie, env, Microsoft.FSharp.Collections.ListModule.of_seq(parent.options.VcOpt));
            vcopt.RemoveExpansionAxioms();
          } finally {
            parent.swVcOpt.Stop();
          }
        }
        dummyOpt = null;
      }
    }
  }
    
  internal class Vcc2Plugin : Plugin
  {
    internal VccOptions options;
    internal string ModelFileName;

    Stopwatch swBoogieAST = new Stopwatch("Boogie AST");
    internal Stopwatch swBoogie = new Stopwatch("Boogie");
    internal Stopwatch swBoogieAI = new Stopwatch("Boogie AI");
    internal Stopwatch swBoogieResolve = new Stopwatch("Boogie Resolve");
    internal Stopwatch swBoogieTypecheck = new Stopwatch("Boogie Typecheck");
    internal Stopwatch swVcOpt = new Stopwatch("VC Optimizer");
    internal Stopwatch swVerifyImpl = new Stopwatch("Boogie Verify Impl.");
    Stopwatch swSaveBPL = new Stopwatch("Boogie Save BPL");

    private void RegisterStopwatches()
    {
      this.RegisterStopwatch(swBoogie);
      this.RegisterStopwatch(swBoogieAST);
      this.RegisterStopwatch(swBoogieAI);
      this.RegisterStopwatch(swBoogieResolve);
      this.RegisterStopwatch(swBoogieTypecheck);
      this.RegisterStopwatch(swVcOpt);
      this.RegisterStopwatch(swVerifyImpl);
      this.RegisterStopwatch(swSaveBPL);
    }

    public override string Help()
    {
      return "";
    }

    public override string Name()
    {
      return "Vcc2";
    }

    public override void UseCommandLineOptions(List<string> p1)
    {
    }

    public override void UseVccOptions(VccOptions opts)
    {
      options = opts;
    } 

    public override bool IsModular()
    {
      if (options.TranslateToBPL) return false;
      else return true;
    }

    public override void Verify(string fileName, Helper.Env env, Microsoft.FSharp.Collections.FSharpList<CAST.Top> decls)
    {
      // this really only dumps the code to the .bpl file
      Init(env, fileName);
      decls = env.ApplyTransformers(decls);
      if (env.ShouldContinue) {
        if (env.Options.AggressivePruning && env.Options.Functions.Count > 0) {
          decls = TransUtil.pruneBy(env, env.Options.Functions[0], decls);
        }
        var boogieDecls = Translator.translate(null, env, decls);
        var p = TranslateToBoogie(env, boogieDecls);
        if (env.ShouldContinue) {
          try {
            swSaveBPL.Start();
            TokenTextWriter writer = new TokenTextWriter(Path.ChangeExtension(fileName, ".bpl"));
            p.Emit(writer);
            writer.Close();
          } finally {
            swSaveBPL.Stop();
          }
        }
      }
    }

    private Program TranslateToBoogie(Helper.Env env, Microsoft.FSharp.Collections.FSharpList<BoogieAST.Decl> boogieDecls)
    {
      try {
        swBoogieAST.Start();
        return BoogieAST.trProgram(boogieDecls);
      } finally {
        swBoogieAST.Stop();
      }
    }

    private void Init(Helper.Env env, string filename)
    {
      RegisterStopwatches();
      foreach (var s in env.Stopwatches)
        this.RegisterStopwatch(s);

      Transformers.init(env);
      Transformers.processPipeOptions(env);
      options = env.Options;
      if (options.SaveModel) {
        ModelFileName = Path.ChangeExtension(filename, "vccmodel");
      } else {
        ModelFileName = null;
      }
    }

    public override FunctionVerifier GetFunctionVerifier(string fileName, Helper.Env env, Microsoft.FSharp.Collections.FSharpList<CAST.Top> decls)
    {
      Init(env, fileName);
      decls = env.ApplyTransformers(decls);
      return new Vcc2FunctionVerifier(this, decls, env);
    }

    internal Boogie.Program GetBoogieProgram(Microsoft.FSharp.Collections.FSharpList<BoogieAST.Decl> boogieDecls)
    {
      var p = BoogieAST.trProgram(boogieDecls);
      try {
        swBoogie.Start();
        var pp = new Boogie.Program();
        pp.TopLevelDeclarations.AddRange(Vcc2CommandLineHost.StandardPrelude.TopLevelDeclarations);
        pp.TopLevelDeclarations.AddRange(p.TopLevelDeclarations);
        return pp;
      } finally {
        swBoogie.Stop();
      }
      
    }
  }


  internal class ErrorReporter : Microsoft.Boogie.VerifierCallback
  {
    bool outcomeReported;
    string name;
    double startTime;
    string prevPhase;
    bool lineDirty;
    double prevTime;
    VccOptions commandLineOptions;

    public ErrorReporter(VccOptions opts, string name, double startTime)
    {
      this.name = name;
      this.startTime = startTime;
      this.prevTime = Vcc2CommandLineHost.GetTime();
      this.commandLineOptions = opts;
    }

    public void PrintSummary(VC.VCGen.Outcome outcome)
    {
      PrintSummary(outcome, null);
    }

    public void PrintSummary(VC.VCGen.Outcome outcome, string addInfo)
    {
      if (!this.outcomeReported) {
        this.outcomeReported = true;
        this.lineDirty = false;
        Vcc2CommandLineHost.ReportOutcomeMethodSummary(outcome, addInfo, this.name, this.startTime);
      }
      if (this.lineDirty) {
        Console.WriteLine();
        this.lineDirty = false;
      }
    }

    public override void OnTimeout(string reason)
    {
      this.PrintSummary(VC.VCGen.Outcome.TimedOut, reason);
    }

    public override void OnCounterexample(Counterexample ce, string message)
    {
      this.PrintSummary(VC.VCGen.Outcome.Errors);
      Vcc2CommandLineHost.ReportCounterexample(ce, message);
    }

    public override void OnOutOfMemory(string reason)
    {
      this.PrintSummary(VC.VCGen.Outcome.OutOfMemory, reason);
    }

    public override void OnProgress(string phase, int step, int totalSteps, double progressEst)
    {
      if (phase == "done") return;

      if (double.IsNaN(progressEst)) return;

      if (commandLineOptions != null && commandLineOptions.TimeStats && !commandLineOptions.XmlFormatOutput && !commandLineOptions.RunTestSuite) {
        double now = Vcc2CommandLineHost.GetTime();
        double length = now - this.prevTime;
        if (length < 0.5) return; // don't even bother reporting
        this.prevTime = now;
        this.lineDirty = true;
        //Console.Write("{0}/{1}/[{2:0.00}s]..", step, totalSteps, length);
        if (commandLineOptions.TimeStatsForVs && phase == "VCprove")
          Console.WriteLine("vcc : Verification of {0} - progress {1:0.00}%", name, progressEst * 100);
        else {
          Console.Write("(");
          if (this.prevPhase != phase)
            Console.Write("{0}:", phase);
          Console.Write(step);
          if (phase == "VCprove")
            Console.Write("/{0}/{1:0.00}%) ", totalSteps - step, progressEst * 100);
          else if (step != totalSteps || phase != "smoke")
            Console.Write("/{0}) ", totalSteps);
          else Console.Write(") ");
        }
        this.prevPhase = phase;
      }
    }

    private bool ReportUnreachable(IToken tok)
    {
      if (tok.filename == null || tok.filename == "") return false;
      PrintSummary(VC.VCGen.Outcome.Correct); // it is correct, but
      Console.WriteLine("{0}: found unreachable code, possible soundness violation, please check the axioms or add an explicit assert(false)", 
                        Vcc2CommandLineHost.TokenLocation(tok));
      return true;
    }

    public override void OnUnreachableCode(Implementation impl)
    {
      for (int i = impl.Blocks.Count - 1; i >= 0; i--) {
        Block b = impl.Blocks[i];
        foreach (var cmd in b.Cmds) {
          PredicateCmd pred = cmd as PredicateCmd;
          if (pred != null) {
            NAryExpr nary = pred.Expr as NAryExpr;
            if (nary != null) {
              FunctionCall f = nary.Fun as FunctionCall;
              if (f != null && f.Func.Name == "$expect_unreachable") return;
            }
          }
        }
        if (this.ReportUnreachable(b.TransferCmd.tok)) break;
        for (int j = b.Cmds.Length - 1; j >= 0; j--) {
          if (this.ReportUnreachable(b.Cmds[j].tok)) goto outer;
        }
      }
      PrintSummary(VC.VCGen.Outcome.Correct); // it is correct, but
      Console.WriteLine("Found unreachable code, but cannot figure out where it is.");
    outer: ;
    }
  }

}
