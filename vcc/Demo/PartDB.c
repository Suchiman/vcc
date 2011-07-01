#include <vcc.h>
#include "Spec.h"
#include <stdlib.h>

#define MAXPART 64
#define ISSET(n, v) (((v) & (1ULL << (n))) != 0)

typedef struct vcc(claimable) _Partition {
  bool active;

  struct _PartitionDB *db;
  unsigned idx;
  invariant(idx < MAXPART && db->partitions[idx] == this)

  spec(volatile bool signaled;)
  invariant(signaled <==> ISSET(idx, db->allSignaled))
  invariant(signaled ==> active)

  spec(claim_t db_claim;)
  invariant(keeps(db_claim) && claims_obj(db_claim, db))
} Partition;

typedef Partition *PPartition;

_InterlockedCompareExchange(PPartition)

typedef struct vcc(claimable) _PartitionDB {
  volatile uint64_t allSignaled;
  volatile PPartition partitions[MAXPART];
  invariant(forall(unsigned i; i < MAXPART;
                   unchanged(partitions[i]) ||
                   old(partitions[i]) == NULL || !closed(old(partitions[i]))))
  invariant(forall(unsigned i; i < MAXPART;
                   unchanged(ISSET(i, allSignaled)) ||
                   inv2(partitions[i])))
} PartitionDB;

void part_send_signal(Partition *part spec(claim_t c))
  requires(wrapped(c) && claims(c, closed(part)))
{
  PartitionDB *db = part->db;
  uint64_t idx = part->idx;

  if (!part->active) return;

  bv_lemma(forall(int i, j; uint64_t v; 0 <= i && i < 64 && 0 <= j && j < 64 ==>
    i != j ==> (ISSET(j, v) <==> ISSET(j, v | (1ULL << i)))));

  atomic(part, db, c) {
    spec(part->signaled = true;)
    InterlockedBitSet(&db->allSignaled, idx);
  }
}

// removal of partition requires only ownership of the partition
// nothing is said about the database
void remove_from_db(Partition *part)
  requires(wrapped0(part))
  ensures(mutable(part))
  writes(part)
{
  PartitionDB *db = part->db;
  uint64_t idx = part->idx;

  atomic (db) {
    unwrap(part);
    // TODO we should allow begin_update() to take additional claims
   assert(valid_claim(part->db_claim));
    begin_update();
    db->partitions[idx] = NULL;
  }
}

uint64_t add_to_db(PartitionDB *db, Partition *part claimp(c))
  requires(part->active)
  always(c, closed(db))
  maintains(wrapped(db))
  writes(extent(part), db)
  ensures(result == MAXPART ==> mutable(part))
  ensures(result < MAXPART ==> wrapped(part) && part->idx == result && part->db == db)
{
  unsigned i;
  Partition *old_value;

  // this loop is only an optimization from VCC point of view
  // we just locate i which is likely to be NULL a few lines down from here
  for (i = 0; i < MAXPART; ++i)
    writes(SET())
  {
    // TODO we could use atomic_op() here, not sure if that gives us anything
    atomic (db, c) {
      if (db->partitions[i] == NULL)
        break;
    }
  }

  if (i < MAXPART) {
    part->db = db;
    part->idx = i;
    spec(part->db_claim = claim(db, true);)
    atomic (db, c) {
      old_value = InterlockedCompareExchange(&db->partitions[i], part, NULL);
      spec(part->signaled = ISSET(i, db->allSignaled);)
    }
    // if the entry was still NULL, we could stick our partition in there
    // and can now wrap it
    if (old_value == NULL) {
      part->active = 1;
      wrap(part);
      return i;
    }
  }

  return MAXPART;
}

/*`
Verification of _Partition#adm succeeded.
Verification of _PartitionDB#adm succeeded.
Verification of part_send_signal succeeded.
Verification of remove_from_db succeeded.
Verification of add_to_db succeeded.
Verification of part_send_signal#bv_lemma#0 succeeded.
`*/
