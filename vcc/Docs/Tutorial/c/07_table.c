#include <vcc.h>
#include <stdlib.h>

struct SafeString {
  unsigned capacity, len;
  char *content;
  invariant(len < capacity)
  invariant(content[len] == '\0')
  invariant(keeps(as_array(content, capacity)))
};

/*{obj}*/
struct vcc(dynamic_owns) SafeContainer {
  struct SafeString **strings;
  unsigned len;

  invariant(keeps(as_array(strings, len)))
  invariant(forall(unsigned i; i < len ==> 
      keeps(strings[i])))
  invariant(forall(unsigned i, j; i < len && j < len ==>
      i != j ==> strings[i] != strings[j]))
};
/*{set}*/
void sc_set(struct SafeContainer *c, 
            struct SafeString *s, unsigned idx)
  requires(wrapped(c) && wrapped(s))
  requires(idx < c->len)
  ensures(wrapped(c))
  ensures(c->strings[idx] == s)
  ensures(wrapped(old(c->strings[idx])))
  ensures(c->len == old(c->len))
  writes(c, s)
{
  expose(c) {
    expose(as_array(c->strings, c->len)) {
      c->strings[idx] = s;
    }
    speconly(
      // could be also done with one assignment
      set_owns(c, set_difference(owns(c), SET(old(c->strings[idx]))));
      set_owns(c, set_union(owns(c), SET(s)));
    )
  }
}
/*{use}*/
void use_case(struct SafeContainer *c, struct SafeString *s)
  requires(wrapped(c) && wrapped(s))
  requires(c->len > 10)
  writes(c, s)
{
  struct SafeString *o;
  o = c->strings[5];

  assert(wrapped(c)); // OK
  assert(wrapped(s)); // OK
  assert(wrapped(o)); // error
  sc_set(c, s, 5);
  assert(wrapped(c)); // OK
  assert(wrapped(s)); // error
  assert(wrapped(o)); // OK
}
/*{out}*/
/*`
Verification of SafeString#adm succeeded.
Verification of SafeContainer#adm succeeded.
Verification of sc_set succeeded.
Verification of use_case failed.
testcase(54,12) : error VC9500: Assertion '_vcc_wrapped(o)' did not verify.
`*/
