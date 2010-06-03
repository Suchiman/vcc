/*{include}*/
#include <vcc.h>
#include <stdlib.h>
/*{obj}*/
struct SafeString {
  unsigned capacity, len;
  char *content;
  _(invariant len < capacity)
  _(invariant content[len] == '\0')
  _(invariant \mine((char[capacity])content))
  _(ghost bool consistencyFlag; )
};
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
{
  _(ghost \object cont;)

  _(assume \wrapped(s))
  _(assume s->len < s->capacity - 1)

  _(ghost {
    cont = (char[s->capacity])(s->content); // for readability
    // unwrap s
    _(assert \wrapped(s))
    cont->theOwner = \me; // transfer 1
    s->consistencyFlag = false;
    // unwrap cont
    _(assert \wrapped(cont))
    c->consistencyFlag = false;
  })
  
  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  _(ghost {
    // wrap c
    _(assert \mutable(cont))
    _(assert \inv(cont))
    cont->consistencyFlag = true;
    // wrap s
    _(assert \mutable(s))
    cont->theOwner = s; // transfer 2
    _(assert \inv(s))
    s->consistencyFlag = true;
    // ensures
    _(assert \wrapped(s))
  })
}
/*{out}*/
// Not that the output makes so much sense, but at least there are no parse errors.
/*`
testcase(25,5) : error VC0000: The left of '->theOwner' must point to a struct or union.
testcase(29,5) : error VC0000: The left of '->consistencyFlag' must point to a struct or union.
testcase(38,5) : error VC0000: The left of '->consistencyFlag' must point to a struct or union.
testcase(41,5) : error VC0000: The left of '->theOwner' must point to a struct or union.
`*/
