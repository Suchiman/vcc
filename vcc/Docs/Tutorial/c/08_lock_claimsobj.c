#include <vcc.h>

/*{lock}*/
struct vcc(claimable) vcc(volatile_owns) Lock {
  volatile int locked;
  spec( obj_t protected_obj; )
  invariant( locked == 0 ==> keeps(protected_obj) )
};
/*{init}*/
void InitializeLock(struct Lock *l spec(obj_t obj))
  writes(span(l), obj)
  requires(wrapped(obj))
  ensures(wrapped(l) && l->protected_obj == obj)
{
  l->locked = 0;
  speconly(
    l->protected_obj = obj;
    set_owns(l, SET(obj));
    wrap(l);
  )
}
/*{xchg}*/
vcc(atomic_inline) int InterlockedCompareExchange(volatile int *Destination, int Exchange, int Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}
/*{acquire}*/
void Acquire(struct Lock *l claimp(c))
  always(c, closed(l))
  ensures( wrapped(l->protected_obj) && is_fresh(l->protected_obj) )
{
  int stop = 0;

  do {
    atomic (c, l) {
      stop = InterlockedCompareExchange(&l->locked, 1, 0) == 0;
      speconly(if (stop) giveup_closed_owner(l->protected_obj, l);)
    }
  } while (!stop);
}
/*{release}*/
void Release(struct Lock *l claimp(c))
  requires(wrapped(c) && claims_obj(c, l))
  requires(l->protected_obj != c)
  requires(wrapped(l->protected_obj))
  ensures(wrapped(c))
  writes(l->protected_obj)
{
  atomic (c, l) {
    l->locked = 0;
    set_closed_owner(l->protected_obj, l);
  }
}
/*{out}*/
/*`
`*/
