#include "SpinLockNewSyntax.h"
#include "IntrinsicsNewSyntax.h"

void InitializeSpinLock(SPIN_LOCK *SpinLock _(ghost \object obj))
{
  SpinLock->Lock = 0;
  _(ghost SpinLock->protected_obj = obj;
    ghost SpinLock->\owns = {obj};
    wrap SpinLock)
}

void Acquire(SPIN_LOCK *SpinLock _(ghost \claim access_claim))   
{
  int stop;
  do {
    _(atomic access_claim, SpinLock) {
      stop = (__interlockedcompareexchange(&SpinLock->Lock, 1, 0) == 0);
      _(ghost if (stop) SpinLock->\owns -= SpinLock->protected_obj)
    }
  } while (!stop);
}

void Release(SPIN_LOCK *SpinLock _(ghost \claim access_claim))
{
  _(atomic access_claim, SpinLock) {
    SpinLock->Lock = 0;
    _(ghost  SpinLock->\owns += SpinLock->protected_obj);
  }
}
