
#include <vcc.h>
#include <stdlib.h>

#define SIMPLE_SPIN_LOCKS

typedef struct vcc(claimable) vcc(volatile_owns) _SPIN_LOCK
{
  volatile int Lock;

  spec(obj_t protected_obj;)
  invariant(!Lock ==> keeps(protected_obj))
} SPIN_LOCK;


void InitializeSpinLock(SPIN_LOCK * SpinLock spec(obj_t obj))
  writes(span(SpinLock), obj)
  requires(wrapped(obj))
  ensures(wrapped(SpinLock))
  ensures(SpinLock->protected_obj == obj)
  ;

#ifdef SIMPLE_SPIN_LOCKS

#define _claimable_

void Acquire(SPIN_LOCK *SpinLock)   
  requires(wrapped(SpinLock))
  ensures(wrapped(SpinLock->protected_obj))
  ensures(is_fresh(SpinLock->protected_obj))
 ;

void Release(SPIN_LOCK *SpinLock)
  writes(SpinLock->protected_obj)
  requires(wrapped(SpinLock))
  requires(wrapped(SpinLock->protected_obj))
 ;

#else

#define _claimable_ vcc(claimable)

void Acquire(SPIN_LOCK *SpinLock claimp(access_claim))   
  always(access_claim, closed(SpinLock))
  ensures(wrapped(SpinLock->protected_obj))
  ensures(is_fresh(SpinLock->protected_obj))
  ;

void Release(SPIN_LOCK *SpinLock claimp(access_claim))
  writes(SpinLock->protected_obj)
  always(access_claim, closed(SpinLock))
  requires(access_claim != SpinLock->protected_obj)
  requires(wrapped(SpinLock->protected_obj))
  ;

#endif