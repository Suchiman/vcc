#include <vcc.h>

/*{obj}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  int consistent;
  _(ghost bool consistencyFlag;)
  _(invariant len < SSTR_MAXLEN)
  _(invariant content[len] == '\0')
};

/*{assert}*/
void sstr_append_char(struct SafeString *s, char c)
{
  // requires
  _(assume \wrapped(s))
  _(assume s->len < SSTR_MAXLEN - 1)
  // _(unwrap s);
  _(assert \wrapped(s))
  _(assume s->len < SSTR_MAXLEN && s->content[s->len] == '\0')
  _(ghost s->consistencyFlag = \false;)

  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  
  // _(wrap s);
  _(assert \unwrapped(s))
  _(assert s->len < SSTR_MAXLEN && s->content[s->len] == '\0')
  _(ghost s->consistencyFlag = \true;)
  // ensures
  _(assert \wrapped(s))
}
/*{out}*/
/*`
Verification of SafeString#adm succeeded.
Verification of sstr_append_char failed.
testcase(23,25) : error VC8507: Assertion 's->consistencyFlag is writable' did not verify.`*/
