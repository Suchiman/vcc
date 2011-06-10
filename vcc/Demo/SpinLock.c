#include "SpinLock.h"
#include "Intrinsics.h"

void InitializeSpinLock(SPIN_LOCK *SpinLock spec(obj_t obj))
{
  SpinLock->Lock = 0;
  spec(SpinLock->protected_obj = obj;)
  set_owns(SpinLock, SET(obj));
  wrap(SpinLock);
}

#ifdef SIMPLE_SPIN_LOCKS

void Acquire(SPIN_LOCK *SpinLock)
{
  int stop;
  do  {
    atomic(SpinLock) {
      stop = (__interlockedcompareexchange(&SpinLock->Lock, 1, 0) == 0);
      spec(if (stop) giveup_closed_owner(SpinLock->protected_obj, SpinLock);)
    }
  } while (!stop);
}

void Release(SPIN_LOCK *SpinLock)
{
  atomic(SpinLock) {
    SpinLock->Lock = 0;
    set_closed_owner(SpinLock->protected_obj, SpinLock);
  }
}

#else

void Acquire(SPIN_LOCK *SpinLock claimp(access_claim))
{
  int stop;
  do {
    atomic(access_claim, SpinLock) {
      stop = (__interlockedcompareexchange(&SpinLock->Lock, 1, 0) == 0);
      spec(if (!stop) giveup_closed_owner(SpinLock->protected_obj, SpinLock);)
    }
  } while (!stop);
}

void Release(SPIN_LOCK *SpinLock claimp(access_claim))
{
  atomic(access_claim, SpinLock) {
    SpinLock->Lock = 0;
    set_closed_owner(SpinLock->protected_obj, SpinLock);
  }
}

#endif

// Note: expected output only applies if SIMPLE_SPIN_LOCKS is defined
/*`
Verification of _SPIN_LOCK#adm succeeded.
Verification of InitializeSpinLock succeeded.
Verification of Acquire succeeded.
Verification of Release succeeded.
`*/
