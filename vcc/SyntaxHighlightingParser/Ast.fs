namespace SyntaxHighlighting

open Microsoft.FSharp.Text.Lexing

module Ast =

  type Span = 
    | Spec of int * int
    | Keyword of int * int

  let private toKeywordSet =  
    List.map (fun (str:string) -> str.ToCharArray()) >> Set.ofList

  let private keywords =
    toKeywordSet [
                  "abstract" ;
                  "admissibility" ;
                  "always" ;
                  "atomic" ;
                  "atomic_inline" ;
                  "atomic_op";
                  "atomic_read" ;
                  "as_array" ;
                  "assume" ;
                  "assume_correct" ;
                  "assert" ; 
                  "axiom" ; 
                  "backing_member" ;
                  "begin_update" ;
                  "blob" ;
                  "blobify" ;
                  "blob_of" ;
                  "bump_volatile_version" ;
                  "by_claim" ;
                  "claimable" ;
                  "colon" ;
                  "contract" ;
                  "contract_pointer" ;
                  "CommandLineSwitches" ;
                  "datatype" ; 
                  "decreases" ; 
                  "def" ;
                  "dynamic_owns" ;
                  "ensures" ; 
                  "frame_axiom" ;
                  "ghost" ;
                  "ghost_atomic" ;
                  "group" ;
                  "havoc_others" ;
                  "increases" ;
                  "inline" ;
                  "invariant" ;
                  "isolate_proof" ;
                  "join_blobs" ;
                  "known" ;
                  "maintains" ;
                  "no_reads_check" ;
                  "option" ;
                  "out" ;
                  "precise" ;
                  "primitive" ;
                  "pure" ;
                  "read_only" ;
                  "reads" ;
                  "reads_havoc" ;
                  "record" ;
                  "requires" ;
                  "recursive_with";
                  "returns" ;
                  "retype" ;
                  "root_array" ;
                  "root_index" ;
                  "skip_smoke" ;
                  "split_blob" ;
                  "TestResults" ;
                  "type" ;
                  "typedef" ;
                  "unblobify" ;
                  "unchecked" ;
                  "union_reinterpret" ;
                  "unwrap" ;
                  "unwrapping" ;
                  "updates" ;
                  "volatile_owns" ;
                  "wrap" ;
                  "writes" ;
                  "yarra" ;
    ]

  let private guardedKeywords =
    toKeywordSet [
                  "\\account_claim" ;
                  "\\active_claim" ;
                  "\\addr" ;
                  "\\addr_eq" ;
                  "\\alloc" ;
                  "\\alloc_array" ;
                  "\\always_by_claim" ;
                  "\\approves"; 
                  "\\array_members" ;
                  "\\array_range" ;
                  "\\arrays_disjoint" ;
                  "\\at" ;
                  "\\begin_update" ;
                  "\\claim" ;
                  "\\claim_count" ;
                  "\\claimable" ;
                  "\\claims"; 
                  "\\claims_claim" ;
                  "\\claims_object" ;
                  "\\closed" ;
                  "\\composite_extent" ;
                  "\\deep_eq" ;
                  "\\deep_unwrap" ;
                  "\\depends" ;
                  "\\destroy_claim" ;
                  "\\diff" ;
                  "\\disjoint" ;
                  "\\domain" ;
                  "\\domain_root" ;
                  "\\domain_updated_at" ;
                  "\\embedding" ;
                  "\\exists" ;
                  "\\extent" ;
                  "\\extent_fresh" ;
                  "\\extent_mutable" ;
                  "\\false" ;
                  "\\forall" ;
                  "\\fresh" ;
                  "\\from_bytes" ;
                  "\\full_context" ;
                  "\\full_extent" ;
                  "\\gemb" ;
                  "\\in_range_phys_ptr";
                  "\\in_range_spec_ptr";
                  "\\inter" ;
                  "\\in" ;
                  "\\in0" ;
                  "\\in_array";
                  "\\index_within";
                  "\\integer" ;
                  "\\inv" ;
                  "\\inv2" ;
                  "\\inv2s" ;
                  "\\is" ;
                  "\\is_array" ;
                  "\\join_arrays" ;
                  "\\lambda";
                  "\\make_claim" ;
                  "\\malloc_root" ;
                  "\\map" ;
                  "\\match_long" ;
                  "\\match_ulong" ;
                  "\\may_diverge" ;
                  "\\mine";
                  "\\mutable" ;
                  "\\mutable_array" ;
                  "\\natural" ;
                  "\\nested" ;
                  "\\non_primitive_ptr" ;
                  "\\not_shared" ;
                  "\\now" ;
                  "\\object" ;
                  "\\object_root" ;
                  "\\objset" ;
                  "\\old" ;
                  "\\on_unwrap" ;
                  "\\owner" ;
                  "\\owns" ;
                  "\\program_entry_point" ;
                  "\\recursive_with";
                  "\\result";
                  "\\set" ;
                  "\\set_closed_owns" ;
                  "\\shallow_eq" ;
                  "\\simple_embedding" ;
                  "\\size" ;
                  "\\size_t" ;
                  "\\span" ;
                  "\\split_array" ;
                  "\\start_here";
                  "\\state" ;
                  "\\subset" ;
                  "\\this" ;
                  "\\thread" ;
                  "\\thread_local" ;
                  "\\thread_local_array" ;
                  "\\to_bytes" ;
                  "\\true" ;
                  "\\type" ;
                  "\\typeof" ;
                  "\\unchanged";
                  "\\union" ;
                  "\\union_active" ;
                  "\\union_reinterpret" ;
                  "\\universe" ;
                  "\\unwrap" ;
                  "\\upgrade_claim" ;
                  "\\valid" ;
                  "\\vdomain" ;
                  "\\when_claimed"; 
                  "\\wrap" ;
                  "\\wrapped" ;
                  "\\wrapped0";
                  "\\wrapped_with_deep_domain" ;
                  "\\writable" ;
    ]

  let isGuardedKeyword chars = Set.contains chars guardedKeywords

  let isKeyword chars = Set.contains chars keywords
