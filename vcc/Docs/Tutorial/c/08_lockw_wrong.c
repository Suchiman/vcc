#include <vcc.h>

/*{lock}*/
struct vcc(volatile_owns) Lock {
  volatile int locked;
  spec(obj_t protected_obj;)
  invariant(locked == 0 ==> keeps(protected_obj))
};
/*{release}*/
void Release(struct Lock *l)
  requires(wrapped(l))
  requires(wrapped(l->protected_obj))
{
  atomic (l) {
    l->locked = 0;
    speconly(set_closed_owner(l->protected_obj, l);)
  }
}
/*{out}*/
/*`
Verification of Lock#adm succeeded.
Verification of Release failed.
testcase(16,45) : error VC8510: Assertion 'l->protected_obj is writable in call to _vcc_set_closed_owner(l->protected_obj, l)' did not verify.
`*/
