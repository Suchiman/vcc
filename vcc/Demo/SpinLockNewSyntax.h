
#include <vcc.h>
#include <stdlib.h>

typedef _(claimable) _(volatile_owns) struct _SPIN_LOCK
{
  volatile int Lock;
  _(ghost \object protected_obj)
  _(invariant !Lock ==> \mine(protected_obj))
} SPIN_LOCK;


void InitializeSpinLock(SPIN_LOCK * SpinLock _(ghost \object obj))
  _(writes \span(SpinLock), obj)
  _(requires \wrapped(obj))
  _(ensures \wrapped(SpinLock))
  _(ensures SpinLock->protected_obj == obj)
  ;

void Acquire(SPIN_LOCK *SpinLock _(ghost \claim access_claim))   
  _(always access_claim, SpinLock->\consistent)
  _(ensures \wrapped(SpinLock->protected_obj))
  _(ensures \fresh(SpinLock->protected_obj))
  ;

void Release(SPIN_LOCK *SpinLock _(ghost \claim access_claim))
  _(writes SpinLock->protected_obj)
  _(always access_claim, SpinLock->\consistent)
  _(requires access_claim != SpinLock->protected_obj)
  _(requires \wrapped(SpinLock->protected_obj))
  ;
