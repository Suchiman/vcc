#include <vcc2test.h>

struct X { int y; };

struct Data {
  int dummy;
  spec( volatile bool handles[struct Handle*]; )
  on_unwrap(forall(struct Handle *h; ! handles[h]))
  invariant(approves(owner(this), handles))
  //invariant(forall(struct Handle *h; closed(h) && h->data == this ==> handles[h]))
  invariant(forall(struct Handle *h; old(handles[h]) && !handles[h] ==> !closed(h)))
};

struct Handle {
  int dummy;
  spec( struct Data *data; )
  invariant(closed(data) && data->handles[this])
};

void foo(struct X *x) writes(extent(x)) maintains(mutable(x) && is_object_root(x));

void wrapped_use()
{
  struct Data d;
  struct Handle h;
  struct X x;

  speconly(

  d.handles = lambda(struct Handle *h; true; false);
  assert(forall(struct Handle *h; h->data == &d ==> !inv(h)));
  wrap(&d);
  assert(in_domain(&d,&d));

  atomic(&d) {
    d.handles = lambda(struct Handle *hh; true; (bool)(hh == &h));
    bump_volatile_version(&d);
  }
  assert(in_domain(&d,&d));
  h.data = &d;
  wrap(&h);

  )

  foo(&x);

  speconly(

  atomic(&d) {
    unwrap(&h);
    begin_update();
    d.handles = lambda(struct Handle *hh; true; false);
    bump_volatile_version(&d);
  }
  assert(in_domain(&d,&d));

  unwrap(&d);

  )
}

struct Container {
  struct Data d;
  struct Handle h;
  struct Handle h2;

  invariant(keeps(&d))
  invariant(forall(struct Handle *hh; d.handles[hh] ==> hh == &h || hh == &h2))
};

void init()
{
  struct Container *c = (struct Container *)malloc(sizeof(struct Container));
  if (c != NULL) {
    speconly( c->d.handles = lambda(struct Handle *h; true; false); )
    assert(forall(struct Handle *h; h->data == &c->d ==> !inv(h)));
    wrap(&c->d);
    wrap(c);
  }
}

void closed_use(struct Container *c)
  writes(extent(&c->h))
  requires(wrapped(c))
  ensures(wrapped(&c->h) && c->h.data == &c->d)
{
speconly(
  atomic(&c->d) {
    assert(inv(c));
    c->d.handles = lambda(struct Handle *hh; true; (bool)(hh == &c->h || c->d.handles[hh]));
  }
  c->h.data = &c->d;
  wrap(&c->h);
)
}

/*`
Verification of Data#adm succeeded.
Verification of Handle#adm succeeded.
Verification of Container#adm succeeded.
Verification of wrapped_use succeeded.
Verification of init succeeded.
Verification of closed_use succeeded.
`*/
