#include <vcc.h>
#include <stdlib.h>

#define inv0(p) ((p) == NULL || inv(p))
#define nextclosed(f) forall(struct Node *n; {set_in(n->f, owns(this))} set_in(n, owns(this)) ==> n->f == NULL || set_in(n->f, owns(this)))

struct Node {
  struct Node *l, *r, *p;
  volatile int val, sum;
  spec( struct Mgr ^m; )
  invariant(closed(m))
  invariant(l != NULL ==> l != this && l->p == this && l->m == m && closed(l))
  invariant(r != NULL ==> r != this && r->p == this && r->m == m && closed(r))
  invariant(this == m->except || sum == unchecked(val + (l == NULL ? 0 : l->sum) + (r == NULL ? 0 : r->sum)))
  invariant(unchanged(sum) || inv(p) || p == NULL)
  on_unwrap(false)
};
typedef struct Node *PNode;

spec(
struct vcc(dynamic_owns) Mgr {
  volatile PNode except;
  invariant(unchanged(except) || inv0(old(except)))
  invariant(approves(owner(this), except))
  invariant(forall(struct Node *n; set_in(n, owns(this)) ==> n->m == this))
  invariant(nextclosed(p))
  invariant(nextclosed(l))
  invariant(nextclosed(r))
  on_unwrap(false)
};
)

void update(struct Node *n, int v spec(struct Mgr ^m))
  requires(set_in(n, owns(m)))
  maintains(wrapped(m) && m->except == NULL)
  writes(m)
{
  atomic(n, m) {
    speconly( m->except = n; )
    n->val = v;
    speconly( bump_volatile_version(m); )
  }

  while (n) 
    invariant(m->except == n)
    invariant(wrapped(m))
    invariant(n == NULL || set_in(n, owns(m)))
    writes(m)
  {
    int a,b;

    assert(in_domain(m, m));
    assert(in_domain(n, m));
    atomic(n,m) {
      assert(n->p == NULL || in_domain(n->p, m));
      assume(a == n->l->sum && b == n->r->sum); // assume that we can read it atomically
      n->sum = unchecked(n->val + (n->l == NULL ? 0 : a) + (n->r == NULL ? 0 : b));
      speconly( m->except = n->p; )
      speconly( bump_volatile_version(m); )
    }
    n = n->p;
  }
}
