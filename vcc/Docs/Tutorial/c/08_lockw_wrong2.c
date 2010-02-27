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
  writes(l->protected_obj)
{
  atomic (l) {
    l->locked = 0;
  }
}
/*{out}*/
/*`
Verification of Lock#adm succeeded.
Verification of Release failed.
testcase(15,21) : error VC8524: Assertion 'chunk locked == 0 ==> _vcc_keeps(__this , protected_obj) of invariant of l holds after atomic' did not verify.
`*/
