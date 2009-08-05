#include <stdlib.h>
#include "vcc.h"

#ifdef VERIFY
#define bool _Bool
#else
#define bool int
#endif

spec( ispure size_t Capacity()
    ensures(result > 0 && result <= 100); )

struct Stack{
    size_t topOfStack;
    int array[100];

    invariant (is_malloc_root(this) && is_array_emb(this->array,Capacity(),this) 
                 && this->topOfStack>=0 && this->topOfStack<= Capacity())
};

frameaxiom
bool IsEmpty(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == (S->topOfStack == 0))
{
    return S->topOfStack == 0;
}

ispure
bool IsFull(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == (S->topOfStack == Capacity()))
{
    return S->topOfStack == Capacity();
}

ispure
size_t Length(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == S->topOfStack)
{
    return S->topOfStack;
}

struct Stack * CreateStack()
    ensures(result == NULL || wrapped(result))
    ensures(result == NULL || is_fresh(result))
    ensures(result == NULL || IsEmpty(result))
{
        struct Stack * S;

        assume(Capacity() == 100);

    S = malloc(sizeof(struct Stack));
    if (S != NULL) {
      S->topOfStack = 0;
      wrap(S);
    }
    return S;
}


void DisposeStack(struct Stack * S)
    requires(wrapped(S))
    writes(S)
{
    unwrap(S);
        free(S);
}
void MakeEmpty(struct Stack * S)
    requires(wrapped(S))
    ensures(wrapped(S))
    ensures(IsEmpty(S))
    writes(S)

{
    unwrap(S);
        S->topOfStack = 0;
    wrap(S);
}



void Push(struct Stack * S, int X)
    requires(wrapped(S))  
    ensures(wrapped(S))
    requires(!IsFull(S))
    ensures(!IsEmpty(S))
    writes(S)
{
    unwrap(S);
        S->array [ S->topOfStack ] = X;
        S->topOfStack = S->topOfStack+1;
    wrap(S);        
}


int Top(struct Stack * S)
    requires(!IsEmpty(S))
    requires(wrapped(S))
    reads(S)
{
    assert(inv(S));
    return S->array[ S->topOfStack-1 ];
}

void Pop(struct Stack * S)
    requires(wrapped(S))  
    ensures(wrapped(S))
    requires(!IsEmpty(S))
    writes(S)
    ensures(!IsFull(S))
{   unwrap(S);
        S->topOfStack = S->topOfStack-1;
    wrap(S);
}

int test_main()
{
    struct Stack * S;
    size_t i; 
    int j;

    S = CreateStack();
    assume(S != NULL);
    assert(IsEmpty(S));
    i = 0;
    while(i < Capacity()) 
        invariant(wrapped(S))
//      invariant(i == Length(S))
    {   assume(!IsFull(S));
            Push(S, unchecked((int)i));
        i = i + 1;
    }

    while(i>0) 
        invariant(wrapped(S))
        //invariant(Length(S)>0)
    {   assume(!IsEmpty(S));
        j = Top(S);
        //assert(j<Capacity());
        Pop(S);
        i = i-1;
    }
    //assert(IsEmpty(S));

    DisposeStack(S);
    return 0;
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
