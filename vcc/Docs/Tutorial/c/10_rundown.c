#include <vcc.h>
#include <limits.h>

vcc(atomic_inline) unsigned InterlockedCompareExchange(volatile unsigned *Destination, unsigned Exchange, unsigned Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}


/*{refcnt}*/
struct RefCnt {
  volatile unsigned  cnt;
  spec(obj_t resource; )
  invariant(keeps(resource))
  invariant(is_claimable(resource))
  invariant(ref_cnt(resource) == cnt >> 1)
  invariant(old(cnt & 1) ==> old(cnt) >= cnt)
};
/*{init}*/
void init(struct RefCnt *r spec(obj_t rsc))
  writes(span(r), rsc)
  requires(wrapped0(rsc) && is_claimable(rsc))
  ensures(wrapped(r) && r->resource == rsc)
{
  r->cnt = 0;
  speconly( r->resource = rsc; )
  wrap(r);
}
/*{incr}*/
int try_incr(struct RefCnt *r claimp(c) 
             spec(out claim_t ret))
  always(c, closed(r))
  ensures(result == 0 ==> 
     claims_obj(ret, r->resource) && wrapped0(ret) && 
     is_fresh(ret))
{
  unsigned v, n;

  for (;;) {
    atomic (c, r) { v = r->cnt; }
    if (v & 1) return -1;

    assume(v <= UINT_MAX - 2);
    atomic (c, r) {
      n = InterlockedCompareExchange(&r->cnt, v + 2, v);
      speconly(
        if (v == n) ret = claim(r->resource, true); )
    }

    if (v == n) return 0;
  }
}
/*{decr}*/
void decr(struct RefCnt *r claimp(c) claimp(handle))
  always(c, closed(r))
  requires(claims_obj(handle, r->resource) && wrapped0(handle))
  requires(c != handle)
  writes(handle)
{
  unsigned v, n;

  for (;;)
    invariant(wrapped(c) && wrapped0(handle))
  {
    atomic (c, r) {
      v = r->cnt;
      assert(valid_claim(handle));
      assert(v >= 2);
    }

    atomic (c, r) {
      n = InterlockedCompareExchange(&r->cnt, v - 2, v);
      speconly(
        if (v == n) {
          unclaim(handle, r->resource);
        }
      )
    }

    if (v == n) break;
  }
}
/*{use}*/
struct vcc(claimable) A {
  volatile int x;
};

struct B {
  struct RefCnt rc;
  struct A a;
  invariant(keeps(&rc))
  invariant(rc.resource == &a)
};

void useb(struct B *b claimp(c))
  always(c, closed(b))
{
  spec(claim_t ac;)
  if (try_incr(&b->rc spec(c) spec(out ac)) == 0) {
    atomic (&b->a, ac) {
      b->a.x = 10;
    }
    decr(&b->rc spec(c) spec(ac));
  }
}
/*{enduse}*/

void initb(struct B *b)
  writes(extent(b))
  ensures(wrapped(b))
{
  b->a.x = 7;
  wrap(&b->a);
  init(&b->rc spec(&b->a));
  wrap(b);
}

/*{out}*/
/*`
`*/
