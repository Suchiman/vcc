#include <vcc.h>
#define false 0
#define true 1

/*{obj}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  int consistent;
  invariant(len < SSTR_MAXLEN)
  invariant(content[len] == '\0')
};

/*{assert}*/
void sstr_append_char(struct SafeString *s, char c)
{
  // requires
  assume(wrapped(s));
  assume(s->len < SSTR_MAXLEN - 1);
  // _(unwrap s);
  assert(wrapped(s));
  assume(s->len < SSTR_MAXLEN && s->content[s->len] == '\0');
  speconly(s->consistencyFlag = false;)

  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  
  // _(wrap s);
  assert(mutable(s));
  assert(s->len < SSTR_MAXLEN && s->content[s->len] == '\0');
  speconly(s->consistencyFlag = true;)
  // ensures
  assert(wrapped(s));
}
