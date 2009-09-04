#include <stdlib.h>
#include <limits.h>

#include <vcc.h>

spec(struct AbsStack {
    size_t high_mark;
    size_t capacity;
    size_t entries[size_t];

    invariant(high_mark <= capacity)
    invariant(is_malloc_root(this))
};)

struct Stack{
    size_t topOfStack;
    size_t capacity;
    size_t *elements;
    spec(struct AbsStack ^abs;)
    spec(obj_t elementsAsArray;)

    invariant(elementsAsArray == as_array(elements,capacity))
    invariant(keeps(abs, elementsAsArray))
    invariant(is_malloc_root(this))
    invariant(is_malloc_root(elementsAsArray))
    invariant(topOfStack <= capacity)
    invariant(AbstractionRelation(this))
};

spec(
ispure
bool AbstractionRelation(struct Stack *stack)
    reads(set_universe())
    returns((stack->capacity == stack->abs->capacity) &&
        stack->topOfStack == stack->abs->high_mark &&
        forall(size_t idx; idx < stack->topOfStack;
            stack->elements[idx] == stack->abs->entries[idx]));)


frameaxiom
bool IsEmpty(struct Stack *S)
    reads(S)
    requires(wrapped(S))
    returns(S->abs->high_mark == 0)
{
    return S->topOfStack == 0;
}

ispure
bool IsFull(struct Stack *S)
    reads(S)
    requires(wrapped(S))
    returns(S->abs->high_mark == S->abs->capacity)
{
    return S->topOfStack == S->capacity;
}

size_t Length(struct Stack *S)
    requires(wrapped(S))
    returns(S->abs->high_mark)
{
    return S->topOfStack;
}

struct Stack *CreateStack(unsigned max_capacity)
    requires(max_capacity > 0)
    ensures(result == NULL ||
        wrapped(result) &&
        is_fresh(result) &&
        IsEmpty(result) &&
        result->abs->capacity == max_capacity)
{
    struct Stack *S;

    S = malloc(sizeof(struct Stack));
    if (S == NULL) return NULL;

    S->topOfStack = 0;
    S->capacity = max_capacity;
    S->elements = malloc(sizeof(size_t) * S->capacity);
    if (S->elements == NULL) {
        free(S);
        return NULL;
    }

    speconly(
        S->elementsAsArray = as_array(S->elements, S->capacity);
        S->abs = spec_malloc<struct AbsStack>();
        S->abs->high_mark = S->topOfStack;
        S->abs->capacity = S->capacity;)
    wrap(S->elementsAsArray);
    wrap(S->abs);
    wrap(S);
    return S;
}

void DisposeStack(struct Stack *S)
    requires(wrapped(S))
    writes(S)
{
    assert(in_domain(S->elementsAsArray, S));
    unwrap(S);
    unwrap(S->elementsAsArray);
    free((void*) as_array(S->elements,S->capacity));
    unwrap(S->abs);
    free(S);
}

void MakeEmpty(struct Stack *S)
    maintains(wrapped(S))
    ensures(IsEmpty(S))
    writes(S)
{
    unwrap(S);
    S->topOfStack = 0;
    unwrap(S->abs);
    speconly(S->abs->high_mark = S->topOfStack;)
    wrap(S->abs);
    wrap(S);
}

size_t Top(struct Stack *S)
    requires(wrapped(S))
    requires(!IsEmpty(S))
    returns(S->abs->entries[S->abs->high_mark - 1])
{
    assert(in_domain(S->elementsAsArray, S));
    return S->elements[S->topOfStack - 1];
}

void Pop(struct Stack *S)
    maintains(wrapped(S))
    writes(S)
    requires(!IsEmpty(S))
    ensures(!IsFull(S))
    ensures(S->abs->high_mark == old(S->abs->high_mark) - 1)
    ensures(forall(size_t idx; idx < S->abs->high_mark;
        unchanged(S->abs->entries[idx])))
{
    unwrap(S);
    S->topOfStack--;
    unwrap(S->abs);
    speconly(S->abs->high_mark--;)
    wrap(S->abs);
    wrap(S);
}

void Push(struct Stack *S, size_t X)
    maintains(wrapped(S))
    writes(S)
    requires(!IsFull(S))
    ensures(!IsEmpty(S))
    ensures(S->abs->high_mark == old(S->abs->high_mark) + 1)
    ensures(forall(size_t idx; idx < old(S->abs->high_mark);
        unchanged(S->abs->entries[idx])))
    ensures(S->abs->entries[S->abs->high_mark - 1] == X)
{
    assert(in_domain(S->elementsAsArray, S));
    unwrap(S);
    unwrap(S->elementsAsArray);
    S->elements[S->topOfStack] = X;
    wrap(S->elementsAsArray);
    S->topOfStack++;
    unwrap(S->abs);
    speconly(
        S->abs->entries[S->abs->high_mark] = X;
        S->abs->high_mark++;
    )
    wrap(S->abs);
    wrap(S);
}

ispure
bool IsIn(struct Stack *S, size_t value)
    requires(wrapped(S))
    reads(set_universe())
    returns(exists(size_t v; v < S->abs->high_mark && S->abs->entries[v] == value))
{
    unsigned index;

    assert(in_domain(S->elementsAsArray, S));
    assert(in_domain(S->abs, S));

    for (index = 0; index < S->topOfStack; index++)
        invariant(forall(size_t v; v < index; S->abs->entries[v] != value))
    {
        if (S->elements[index] == value)
            return 1;
    }
    return 0;
}

/*`
Verification of AbsStack#adm succeeded.
Verification of Stack#adm succeeded.
Verification of IsEmpty succeeded.
Verification of IsFull succeeded.
Verification of Length succeeded.
Verification of CreateStack succeeded.
Verification of DisposeStack succeeded.
Verification of MakeEmpty succeeded.
Verification of Top succeeded.
Verification of Pop succeeded.
Verification of Push succeeded.
Verification of IsIn succeeded.
Verification of IsFull#reads succeeded.
Verification of IsEmpty#reads succeeded.
`*/
