#include <vcc.h>
#define false 0
#define true 1

/*{obj}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  int consistent;
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
  _(ghost s->consistencyFlag = false;)

  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  
  // _(wrap s);
  _(assert \unwrapped(s))
  _(assert s->len < SSTR_MAXLEN && s->content[s->len] == '\0')
  _(ghost s->consistencyFlag = true;)
  // ensures
  _(assert \wrapped(s))
}
