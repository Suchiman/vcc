#include <vcc.h>

/*{lock}*/
_(volatile_owns) struct Lock {
  volatile int locked;
  _(ghost \object protected_obj;)
  _(invariant locked == 0 ==> \mine(protected_obj))
};
/*{release}*/
void Release(struct Lock *l)
  _(requires \wrapped(l))
  _(requires \wrapped(l->protected_obj))
{
  _(atomic l) {
    l->locked = 0;
    _(ghost \union_with(\owns(l),{l->protected_obj});)
  }
}
/*{out}*/
/*`
Verification of Lock#adm succeeded.
Verification of Release failed.
testcase(16,45) : error VC8510: Assertion 'l->protected_obj is writable in call to _vcc_set_closed_owner(l->protected_obj, l)' did not verify.
`*/
