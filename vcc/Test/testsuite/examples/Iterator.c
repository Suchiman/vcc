#include <vcc2test.h>

struct Collection {
  int *arr;
  int ver;
  
  def_group(Shadow, vcc(claimable))
  spec( in_group(Shadow) volatile state_t shadow; )
  inv_group(Shadow, approves(owner(this), shadow))
  inv_group(Shadow, in_state(old(shadow), ver) <= in_state(shadow, ver) )
  inv_group(Shadow, in_state(old(shadow), ver) == in_state(shadow, ver) ==>
                       in_state(old(shadow), arr) == in_state(shadow, arr))
  
  invariant(keeps(this::Shadow))
  invariant(in_state(shadow, arr) == arr)
  invariant(in_state(shadow, ver) == ver)
};


void init()
{
  struct Collection *coll = (struct Collection *)malloc(sizeof(struct Collection));
  coll->arr = NULL;
  coll->ver = 0;

  speconly(
    coll->shadow = current_state();
    wrap(coll::Shadow);
    wrap(coll);
  )
}


void add(struct Collection *c)
  maintains(wrapped(c))
  requires(c->ver < 100000)
  writes(c)
{
  int x;

  expose(c) {
    assert(inv(c));
    c->arr = &x;
    c->ver++;
    
    speconly(
      atomic(c::Shadow) {
        c->shadow = current_state();
	bump_volatile_version(c::Shadow);
      }
    )
  }
}

struct Iterator {
  struct Collection *coll;
  int ver;
  spec( claim_t cl; )
  spec( int *arr; )

#define shadow_copy(o, f) in_state((o)->shadow, in_state(current_state(), o)->f)
  invariant(keeps(cl) && claims_obj(cl, coll::Shadow))
  invariant(shadow_copy(coll, ver) >= ver)
  invariant(shadow_copy(coll, ver) == ver ==> shadow_copy(coll, arr) == arr)
};

void get_iter(struct Collection *c)
  requires(wrapped(c))
{
  struct Iterator *iter = (struct Iterator *)malloc(sizeof(struct Iterator));

  iter->coll = c;
  iter->ver = c->ver;
  speconly( iter->arr = c->arr; )
  atomic(c) {
    speconly( iter->cl = claim(c::Shadow, true); )
  }

  wrap(iter);
}

void iterate(struct Iterator *it)
  maintains(wrapped(it))
  requires(wrapped(it->coll))
  requires(it->ver == it->coll->ver)
  writes(it)
{
  expose(it) {
    assert(skip_wf(it->arr == it->coll->arr));
  }
}

/*`
Verification of Collection#adm succeeded.
Verification of Collection##Shadow#adm succeeded.
Verification of Iterator#adm succeeded.
Verification of init succeeded.
Verification of add succeeded.
Verification of get_iter succeeded.
Verification of iterate succeeded.
`*/
