#include <vcc.h>

/*{obj}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  int consistent;
  _(ghost bool consistencyFlag;)
  _(invariant len <= SSTR_MAXLEN)
  _(invariant content[len] == '\0')
};

_(logic bool \writable(\object o) = \true; )
_(logic bool \writable(\objset o) = \true; )

/*{assert}*/
void sstr_append_char(struct SafeString *s, char c)
  _(requires \wrapped(s))
  _(requires s->len < SSTR_MAXLEN)
  _(ensures \wrapped(s))
{
  // _(unwrap s), steps 1-5
  _(assert \writable(s))
  _(assert \wrapped(s))
  _(assume s->len <= SSTR_MAXLEN &&
           s->content[s->len] == '\0')
  _(ghost s->consistencyFlag = \false;)
  _(assume \writable(\span(s)))

  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  
  // _(wrap s), steps 1-3
  _(assert \mutable(s))
  _(assert s->len <= SSTR_MAXLEN &&
           s->content[s->len] == '\0')
  _(ghost s->consistencyFlag = \true;)
}
/*{out}*/
/*`
Verification of SafeString#adm succeeded.
Verification of sstr_append_char failed.
testcase(28,25) : error VC8507: Assertion 's->consistencyFlag is writable' did not verify.`*/
