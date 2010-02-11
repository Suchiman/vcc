/*{include}*/
#include <vcc.h>
#include <stdlib.h>
/*{obj}*/
struct SafeString {
  unsigned capacity, len;
  char *content;
  invariant(len < capacity)
  invariant(content[len] == '\0')
  invariant(keeps(as_array(content, capacity)))
};
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
  requires(wrapped(s))
  requires(s->len < s->capacity - 1)
  ensures(wrapped(s))
  writes(s)
{
  expose(s) {
    expose(as_array(s->content, s->capacity)) {
      s->content[s->len++] = c;
      s->content[s->len] = '\0';
    }
  }
}
/*{alloc}*/
struct SafeString *sstr_alloc(unsigned capacity)
  requires(capacity > 0)
  ensures(result != NULL ==> wrapped(result))
{
  struct SafeString *s;

  s = malloc(sizeof(*s));
  if (s == NULL) return NULL;

  s->content = malloc(capacity);
  if (s->content == NULL) {
    free(s);
    return NULL;
  }

  s->capacity = capacity;
  s->len = 0;
  s->content[0] = '\0';

  wrap(as_array(s->content, capacity));
  wrap(s);

  return s;
}

/*
int sstr_index_of(struct SafeString *s, char c)
  requires(wrapped(s))
  ensures(result >= 0 ==> s->content[result] == c)
{
  unsigned i;
  for (i = 0; i < s->len; ++i)
    if (s->content[i] == c) return (int)i;
  return -1;
}
*/
/*`
`*/
