#include <vcc.h>

#define MAXLEN 1000 // R

#define ASSERT(E) { bool tmp = E; assert(tmp); } // A

spec( typedef bool uset_t[unsigned]; )

spec( ispure mathint card(uset_t s); ) // A
spec( ispure uset_t empty()  // A
  returns(lambda(unsigned i; false)); ) // A
spec( ispure uset_t addone(uset_t s, unsigned i) // A
  returns(lambda(unsigned j; i == j ? true : s[j])); ) // A

axiom( forall(uset_t s; card(empty()) == 0) ); // A
axiom( forall(uset_t s; unsigned i; card(s) >= 0 && !s[i] ==> card(addone(s, i)) == card(s) + 1) ); // A

spec( ispure bool upper_card(uset_t s, mathint n) returns(forall(unsigned i; s[i] ==> i < n)); ) // A
axiom( forall(uset_t s; mathint n; upper_card(s, n) ==> card(s) <= n) ); // A

#define wrappedD(a) (in_domain(a, a) && wrapped(a))

struct Array {               // R
  spec(int ab[unsigned]; )
  int val[MAXLEN];           // R
  unsigned idx[MAXLEN];      // R
  unsigned back[MAXLEN];     // R
  unsigned sz;               // R

  invariant(sz <= MAXLEN)

  spec(uset_t mapped; )
  invariant(forall(unsigned i; mapped[i] <==> i < MAXLEN && idx[i] < sz && back[idx[i]] == i))
  invariant(forall(unsigned i; i < MAXLEN; ab[i] == (mapped[i] ? val[i] : 0)))
  invariant(card(mapped) == (mathint)sz)
};                             // R

void init(struct Array *a)     // R
  writes(span(a))
  ensures(wrappedD(a))
  ensures(forall(unsigned i; a->ab[i] == 0))
{                              // R
  a->sz = 0;                   // R
  speconly( a->ab = lambda(unsigned i; 0); 
            a->mapped = empty(); )
  wrap(a);
}                              // R

void set(struct Array *a, unsigned i, int v) // R
  writes(a)
  requires(i < MAXLEN)
  maintains(wrappedD(a))
  ensures(a->ab == lambda(unsigned j; i == j ? v : old(a->ab)[j]))
{                                                          // R
  expose(a) {
    speconly( a->ab[i] = v; )
    a->val[i] = v;                                         // R
    if (!(a->idx[i] < a->sz && a->back[a->idx[i]] == i)) { // R
      speconly( a->mapped = addone(a->mapped, i); )
      assert(upper_card(a->mapped, MAXLEN));
      a->idx[i] = a->sz;                                   // R
      a->back[a->sz++] = i;                                // R
    }                                                      // R
  }
}                                                          // R

int get(struct Array *a, unsigned i)                       // R
  requires(i < MAXLEN)
  maintains(wrapped(a))
  ensures(result == a->ab[i])
{                                                         // R
  if (a->idx[i] < a->sz && a->back[a->idx[i]] == i)       // R
    return a->val[i];                                     // R
  else return 0;                                          // R
}                                                         // R

void sparseArrayTestHarness()                             // R
{                                                         // R
  struct Array a, b;                                      // R

  init(&a);                                               // R
  init(&b);                                               // R
  ASSERT(get(&a, 5) == 0 && get(&b, 7) == 0);             // R
  set(&a, 5, 1); set(&b, 7, 2);                           // R
  ASSERT(get(&a, 5) == 1 && get(&b, 7) == 2);             // R
  ASSERT(get(&a, 0) == 0 && get(&b, 0) == 0);             // R

  unwrap(&a);
  unwrap(&b);
}                                                         // R

// the warnings about unreachable code are expected, an artifact of the translation of lazy operators

/*`
Verification of Array#adm succeeded.
Verification of init succeeded.
Verification of set succeeded.
Verification of get succeeded.
Verification of sparseArrayTestHarness succeeded.
testcase(86) : warning : found unreachable code, possible soundness violation, please check the axioms or add an explicit assert(false)
testcase(85) : warning : found unreachable code, possible soundness violation, please check the axioms or add an explicit assert(false)
testcase(83) : warning : found unreachable code, possible soundness violation, please check the axioms or add an explicit assert(false)
`*/
