#ifdef VERIFY
// --- Sequence ---
SPEC_TYPE(seq)

typedef void *tptr;

int seq_length(seq s);
axiom (forall (seq s; seq_length(s) >= 0));

seq seq_empty();
axiom (seq_length(seq_empty()) == 0);
axiom (forall (seq s; seq_length(s) == 0 ==> s == seq_empty()));

seq seq_singleton(tptr e);
axiom (forall (tptr t; { seq_length(seq_singleton(t)) } seq_length(seq_singleton(t)) == 1));

seq seq_append(seq s1, seq s2);
axiom (forall (seq s0; seq s1; { seq_length(seq_append(s0,s1)) } 
  seq_length(seq_append(s0,s1)) == seq_length(s0) + seq_length(s1)));

tptr seq_index(seq s, int pos);
axiom (forall (tptr t; { seq_index(seq_singleton(t), 0) } seq_index(seq_singleton(t), 0) == t));
axiom (forall (seq s0; seq s1; int n; { seq_index(seq_append(s0,s1), n) }
  (n < seq_length(s0) ==> seq_index(seq_append(s0,s1), n) == seq_index(s0, n)) &&
  (seq_length(s0) <= n ==> seq_index(seq_append(s0,s1), n) == seq_index(s1, n - seq_length(s0)))));

bool seq_equal(seq s1, seq s2);
axiom (forall (seq s0; seq s1; { seq_equal(s0,s1) }
    seq_length(s0) == seq_length(s1) &&
     forall (int j; { dont_instantiate_int(j) }
        0 <= j && j < seq_length(s0) ==> seq_index(s0,j) == seq_index(s1,j))
  ==> seq_equal(s0,s1)
    ));

// optional
axiom (forall (seq s0; seq s1; { seq_equal(s0,s1) }
         seq_equal(s0,s1) ==> s0 == s1));

seq seq_take(seq s, int howMany);
axiom (forall (seq s; int n; { seq_length(seq_take(s,n)) }
  0 <= n ==>
    (n <= seq_length(s) ==> seq_length(seq_take(s,n)) == n) &&
    (seq_length(s) < n ==> seq_length(seq_take(s,n)) == seq_length(s))));
axiom (forall (seq s; int n; int j; { seq_index(seq_take(s,n), j) }
  0 <= j && j < n && j < seq_length(s) ==>
    seq_index(seq_take(s,n), j) == seq_index(s, j)));

seq seq_drop(seq s, int howMany);
axiom (forall (seq s; int n; { seq_length(seq_drop(s,n)) }
  0 <= n ==>
    (n <= seq_length(s) ==> seq_length(seq_drop(s,n)) == seq_length(s) - n) &&
    (seq_length(s) < n ==> seq_length(seq_drop(s,n)) == 0)));
axiom (forall (seq s; int n; int j; { seq_index(seq_drop(s,n), j) }
  0 <= n && 0 <= j && j < seq_length(s)-n ==>
    seq_index(seq_drop(s,n), j) == seq_index(s, j+n)));


seq seq_remove(seq s, int i);
axiom (forall (seq s; int i; { seq_length(seq_remove(s,i)) }
  0 <= i && i < seq_length(s) ==> 
     seq_length(seq_remove(s,i)) == seq_length(s) - 1));
axiom (forall (seq s; int i; int j; { seq_index(seq_remove(s,i), j) }
  0 <= i && 0 <= j ==>
    (j < i ==> seq_index(seq_remove(s,i), j) == seq_index(s, j)) &&
    (i <= j ==> seq_index(seq_remove(s,i), j) == seq_index(s, j + 1))
    ));
axiom (forall (seq s; int i; { seq_remove(s,i) }  // PM: added since we needed a different trigger 
  seq_length(s) <= i ==> seq_remove(s,i) == s 
      ));

#endif
