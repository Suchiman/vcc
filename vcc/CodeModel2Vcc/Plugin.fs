//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light


namespace Microsoft.Research.Vcc
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 type VerificationResult =
   | Succeeded
   | Failed
   | Inconclusive
   | Crashed
   | UserError
   | Skipped

 type MessageHandler = delegate of string -> unit
 
 type [<AbstractClass>] FunctionVerifier(env:Helper.Env, initialDecls:list<Top>) =
   
   abstract FunctionsToVerify : unit -> list<string>
   abstract Verify : string -> VerificationResult
   abstract DumpInternalsToFile : string * bool -> unit
   abstract Close : unit -> unit
   
   default this.Close () = ()
   
   member this.FindAllFunctions decls =
    if env.ShouldContinue then
      [ for d in decls do
          match d with
            | CAST.FunctionDecl f when f.Body.IsSome -> yield f.Name
            | _ -> yield! [] ]
    else []
 
 type [<AbstractClass>] Plugin() =
   let (triggerMessageHandler : (string -> unit)), messageHandlerEvent = Event.create()
   let messageHandler = new DelegateEvent<MessageHandler>()
   let stopWatches = glist []
   
   abstract Name : unit -> string
   abstract Help : unit -> string
   abstract IsModular : unit -> bool
   abstract UseCommandLineOptions : GList<string> -> unit
   abstract UseVccOptions : VccOptions -> unit
   // depending on IsModular one uses FunctionVerifier or method Verify
   abstract GetFunctionVerifier : string * Helper.Env * list<Top> -> FunctionVerifier
   abstract Verify : string * Helper.Env * list<Top> -> unit
   
   
   default this.IsModular () = true
   default this.GetFunctionVerifier (_, _, _) = raise (System.NotImplementedException())
   default this.Verify (_, _, _) = raise (System.NotImplementedException())
   default this.UseVccOptions _ = ()


   member this.MessageHandler = messageHandlerEvent
   member this.RegisterStopwatch (s:Stopwatch) = 
     let rec repl i =
       if i >= stopWatches.Count then
         stopWatches.Add s
       else
         if s.ShouldReplace stopWatches.[i] then
           stopWatches.[i] <- s
         else repl (i + 1)
     repl 0
   member this.Stopwatches = (stopWatches :> seq<Stopwatch>)
