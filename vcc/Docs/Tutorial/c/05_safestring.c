#include <vcc.h>

/*{obj}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  invariant(len < SSTR_MAXLEN)
  invariant(content[len] == '\0')
};
/*{init}*/
void sstr_init(struct SafeString *s)
  writes(span(s))
  ensures(wrapped(s))
{
  s->len = 0;
  s->content[0] = '\0';
  wrap(s);
}
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
  requires(wrapped(s))
  requires(s->len < SSTR_MAXLEN - 1)
  ensures(wrapped(s))
  writes(s)
{
  unwrap(s);
  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  wrap(s);
}

int sstr_index_of(struct SafeString *s, char c)
  requires(wrapped(s))
  ensures(result >= 0 ==> s->content[result] == c)
{
  unsigned i;
  for (i = 0; i < s->len; ++i)
    if (s->content[i] == c) return (int)i;
  return -1;
}
/*{out}*/
/*`
Verification of SafeString#adm succeeded.
Verification of sstr_init succeeded.
Verification of sstr_append succeeded.
Verification of sstr_index_of succeeded.
`*/
