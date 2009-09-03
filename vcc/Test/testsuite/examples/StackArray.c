#include <stdlib.h>
#include "vcc.h"

#define CAPACITY (100)

struct Stack {
    size_t topOfStack;
    int array[CAPACITY];
    invariant(is_malloc_root(this) && topOfStack <= CAPACITY)
};

frameaxiom
bool IsEmpty(struct Stack *S)
    reads(S)
    requires(wrapped(S))
    returns(S->topOfStack == 0)
{
    return S->topOfStack == 0;
}

ispure
bool IsFull(struct Stack *S)
    reads(S)
    requires(wrapped(S))
    returns(S->topOfStack == CAPACITY)
{
    return S->topOfStack == CAPACITY;
}

ispure
size_t Length(struct Stack *S)
    reads(S)
    requires(wrapped(S))
    returns(S->topOfStack)
{
    return S->topOfStack;
}

struct Stack *CreateStack()
    ensures(result == NULL ||
	    wrapped(result) && is_fresh(result) && IsEmpty(result))
{
    struct Stack *S;

    S = malloc(sizeof(*S));
    if (S != NULL) {
        S->topOfStack = 0;
        wrap(S);
    }
    return S;
}

void DisposeStack(struct Stack *S)
    requires(wrapped(S))
    writes(S)
{
    unwrap(S);
    free(S);
}

void MakeEmpty(struct Stack *S)
    maintains(wrapped(S))
    ensures(IsEmpty(S))
    writes(S)
{
    unwrap(S);
    S->topOfStack = 0;
    wrap(S);
}

void Push(struct Stack *S, int X)
    maintains(wrapped(S))
    requires(!IsFull(S))
    ensures(!IsEmpty(S))
    ensures(S->topOfStack==old(S->topOfStack) + 1)
    writes(S)
{
    unwrap(S);
    S->array[S->topOfStack++] = X;
    wrap(S);
}

int Top(struct Stack *S)
    requires(!IsEmpty(S))
    requires(wrapped(S))
    returns(S->array[S->topOfStack-1])
{
    return S->array[S->topOfStack-1];
}

void Pop(struct Stack *S)
    maintains(wrapped(S))
    requires(!IsEmpty(S))
    ensures(!IsFull(S))
    ensures(S->topOfStack==old(S->topOfStack) - 1)
    writes(S)
{
    unwrap(S);
    S->topOfStack--;
    wrap(S);
}

void test_main()
{
    struct Stack *S;
    size_t i;
    int j;

    S = CreateStack();

    if (S!=NULL) {
        assert(IsEmpty(S));
        i = 0;
        while (i < CAPACITY)
            invariant(wrapped(S))
            invariant(i == Length(S))
        {   
            Push(S, unchecked((int)i));
            i++;
        }

        while (i > 0)
            invariant(wrapped(S))
            invariant(i == Length(S))
        {   
            j = Top(S);
            Pop(S);
            i--;
        }

        DisposeStack(S);
    }
}

/*`
Verification of Stack#adm succeeded.
Verification of IsEmpty succeeded.
Verification of IsFull succeeded.
Verification of Length succeeded.
Verification of CreateStack succeeded.
Verification of DisposeStack succeeded.
Verification of MakeEmpty succeeded.
Verification of Push succeeded.
Verification of Top succeeded.
Verification of Pop succeeded.
Verification of test_main succeeded.
Verification of Length#reads succeeded.
Verification of IsFull#reads succeeded.
Verification of IsEmpty#reads succeeded.
`*/
