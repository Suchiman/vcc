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
  _(requires \wrapped(s))
  _(requires s->len < s->capacity - 1)
  _(ensures \wrapped(s))
  _(writes s)
{
  _(ghost \object cont = (char[s->capacity]) s->content; )
  // _(unwrap s) steps 1-5
  _(assert \writable(s) && \wrapped(s))
  _(assume \writable(span(s)) && \inv(s))
  _(ghost s->consistencyFlag = \false; )
  // and the transfer:
  _(ghost cont->\owner = \me; )
  _(assume \writable(cont))
  // _(unwrap cont) steps 1-5
  _(assert \writable(cont) && \wrapped(cont))
  _(ghost cont->consistencyFlag = \false; )
  _(assume \writable(span(cont)) && \inv(cont))
  // no transfer here
  
  s->content[s->len++] = c;
  s->content[s->len] = '\0';

  // _(wrap cont) steps 1-3
  _(assert \unwrapped(cont) && \inv(cont))
  _(ghost cont->consistencyFlag = \true; )
  // _(wrap s) steps 1-3, with transfer in the middle
  _(assert \unwrapped(s))
  _(ghost cont->\owner = s; )
  _(assert \inv(s))
  _(ghost s->consistencyFlag = \true; )
}
/*{out}*/
// Not that the output makes so much sense, but at least there are no parse errors.
/*`
testcase(25,5) : error VC0000: The left of '->theOwner' must point to a struct or union.
testcase(29,5) : error VC0000: The left of '->consistencyFlag' must point to a struct or union.
testcase(38,5) : error VC0000: The left of '->consistencyFlag' must point to a struct or union.
testcase(41,5) : error VC0000: The left of '->theOwner' must point to a struct or union.
`*/
