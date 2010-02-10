#include <vcc.h>

/*{beg}*/
#define SSTR_MAXLEN 100
struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  invariant(len < SSTR_MAXLEN)
  invariant(content[len] == '\0')
};
/*{index}*/
int sstr_index_of(struct SafeString *s, char c)
  requires(wrapped(s))
  ensures(result >= 0 ==> s->content[result] == c)
{
  unsigned i;

  for (i = 0; i < s->len; ++i)
    if (s->content[i] == c) return (int)i;
  return -1;
}

/*`
Verification of SafeString#adm succeeded.
Verification of sstr_index_of succeeded.
`*/
