#include <vcc2test.h>


typedef struct vcc(claimable) vcc(volatile_owns) _LOCK {
  volatile int locked;

  spec( obj_t protected_obj; )

  invariant( locked == 0 ==> set_in(protected_obj, owns(this)) )
} LOCK;

void InitializeLock(LOCK *l spec(obj_t obj))
  writes(extent(l), obj)
  requires(wrapped(obj))
  ensures(wrapped(l) && l->protected_obj == obj)
{
  l->locked = 0;
  speconly(
    l->protected_obj = obj;
    set_owns(l, set_singleton(obj));
    wrap(l);
  )
}

vcc(atomic_inline) int InterlockedCompareExchange(volatile int *Destination, int Exchange, int Comparand) {
  if (*Destination == Comparand) {
    *Destination = Exchange;
    return Comparand;
  } else {
    return *Destination;
  }
}

void Acquire(LOCK *l claimp(c))
  always(c, closed(l))
  ensures( wrapped(l->protected_obj) && is_fresh(l->protected_obj) )
{
  int stop = 0;

  do 
    invariant(stop == 0)
  {
    atomic (c, l) {
      stop = InterlockedCompareExchange(&l->locked, 1, 0) == 0;
    	speconly(if (stop) giveup_closed_owner(l->protected_obj, l);)
    }
  } while (!stop);
}

void Release(LOCK *l claimp(c))
  always(c, closed(l))
  requires(l->protected_obj != c)
  writes( l->protected_obj )
  requires( wrapped(l->protected_obj) )
{
  atomic (c, l) {
    set_closed_owner(l->protected_obj, l);
    l->locked = 0;
  }
}


typedef struct _DATA {
  int a;
  int b;
  invariant(a + b > 0)
} DATA;

spec(
struct vcc(claimable) _DATA_CONTAINER {
  int dummy;
  invariant(keeps(&DataLock))
  invariant(DataLock.protected_obj == &Data)
} DataContainer;
)

LOCK DataLock;
DATA Data;

void testit(claimp(c))
  always(c, closed(&DataContainer))
{
  Acquire(&DataLock spec(c));
    expose (&Data) {
      Data.a = 5;
      Data.b = 7;
    }
  Release(&DataLock spec(c));
}

void boot()
  writes(set_universe())
  requires(program_entry_point())
{
  spec(claim_t c;)

  Data.a = 42;
  Data.b = 17;
  wrap(&Data);
  InitializeLock(&DataLock spec(&Data));
  set_owner(&DataLock, &DataContainer);
  wrap(&DataContainer);

  speconly(c = claim(&DataContainer, closed(&DataContainer)); )
  testit(spec(c));
}



/*`
Verification of _LOCK#adm succeeded.
Verification of _DATA#adm succeeded.
Verification of _DATA_CONTAINER#adm succeeded.
Verification of InitializeLock succeeded.
Verification of Acquire succeeded.
Verification of Release succeeded.
Verification of testit succeeded.
Verification of boot succeeded.
`*/
