
#include <vcc.h>
#include "Spinlock.h"
#include "BitMap.h"

#ifdef SIMPLE_SPIN_LOCKS
#define _claimable_
#else define _claimable_ vcc(claimable)
#endif


struct _claimable_ LockContainer {
  BITMAP bitmap;
  vcc(as_array) unsigned int buffer[10];
  SPIN_LOCK Lock;

  invariant(keeps(&Lock))
  invariant(Lock.protected_obj == &bitmap)
};

void InitContainer(struct LockContainer *lc)
  writes(extent(lc))
  ensures(wrapped(lc))
{
  wrap(as_array(lc->buffer, 10));
  InitializeBitMap(&lc->bitmap, lc->buffer, 320);
  InitializeSpinLock(&lc->Lock spec(&lc->bitmap));
  wrap(lc);
}

#ifdef SIMPLE_SPIN_LOCKS

void UseContainer(struct LockContainer *lc, unsigned int bitNumber)
  writes(lc)
  maintains(wrapped(lc))
{
  expose(lc) {
    Acquire(&lc->Lock);
    assert(in_domain(&lc->Lock, &lc->Lock));
    if (bitNumber < lc->bitmap.Size) SetBit(&lc->bitmap, bitNumber);
    Release(&lc->Lock);
  }
}

#else

struct ConcurrentUser {
  struct LockContainer *lc;

  spec(claim_t cont_claim;)
  invariant(keeps(cont_claim))
  invariant(claims_obj(cont_claim, lc))
};

void UseContainer(struct LockContainer *lc, unsigned int bitNumber claimp(cont_claim))
  always(cont_claim, closed(lc))
{
  Acquire(&lc->Lock spec(cont_claim));
  assert(in_domain(&lc->bitmap, &lc->bitmap));
  if (bitNumber < lc->bitmap.Size) SetBit(&lc->bitmap, bitNumber);
  Release(&lc->Lock spec(cont_claim));
}

void InitializeConcurrentUser(struct ConcurrentUser *cu, struct LockContainer *lc)
  writes(lc, extent(cu))
  maintains(wrapped(lc))
  ensures(wrapped(cu))
  ensures(cu->lc == lc)
{
  spec(cu->cont_claim = claim(lc, closed(lc));)
  cu->lc = lc;
  wrap(cu);
}

void UseConcurrentOwner(struct ConcurrentUser *cu) 
  writes(cu)
  maintains(wrapped(cu)) 
{
  expose(cu) {
  UseContainer(cu->lc, 10 spec(cu->cont_claim));
  }
}

#endif

// Note: expected output only applies if SIMPLE_SPIN_LOCKS is defined
/*`
Verification of _SPIN_LOCK#adm succeeded.
Verification of _BITMAP#adm succeeded.
Verification of LockContainer#adm succeeded.
Verification of InitContainer succeeded.
Verification of UseContainer succeeded.
Verification of TestBit#reads succeeded.
`*/
