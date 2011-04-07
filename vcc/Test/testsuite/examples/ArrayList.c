//`/newsyntax
// ----------------------------------------------------------------------------
// ArrayList.c
//
// The capacity of an ArrayList is the number of elements the ArrayList can hold.
// As elements are added to an ArrayList, the capacity is automatically increased as required through reallocation.
// Elements in this collection can be accessed using an integer index. Indexes in this collection are zero-based.
//
// ----------------------------------------------------------------------------

#include <stdlib.h>
#include "vcc.h"

struct ArrayList{
    size_t capacity;
    size_t length;
    int *array;

    _(invariant \malloc_root(\this)
    && length <= capacity
    && \mine((void[capacity])array)
    && ((void[capacity])array)->\valid
    && \malloc_root((void[capacity])array))
};

_(pure)
size_t Length(struct ArrayList *A)
    _(reads A)
    _(requires \wrapped(A))
    _(returns A->length)
{
    return A->length;
}

struct ArrayList *CreateArrayList(size_t InitialCapacity)
  _(requires 0 < InitialCapacity)
  _(ensures \wrapped(\result))
  _(ensures Length(\result)==0)
  _(ensures \fresh(\result))
{
    struct ArrayList *A;
    _(ghost \object arr)

    A = malloc(sizeof(*A));
    _(assume A != NULL)

    A->capacity = InitialCapacity;
    A->length = 0;
    A->array = malloc(sizeof(*A->array) * InitialCapacity);
    _(assume A->array != NULL)

    _(ghost arr = (void[InitialCapacity])(A->array))
    _(wrap arr)
    _(ghost A->\owns =  {arr});
    _(wrap A)
    return A;
}

void MakeEmpty(struct ArrayList *A)
    _(maintains \wrapped(A))
    _(ensures Length(A)==0)
    _(writes A)
{
    _(unwrap A)
    A->length = 0;
    _(wrap A)
}

_(pure)
int Select(struct ArrayList *A, size_t i)
    _(requires i < Length(A))
    _(requires \wrapped(A))
    _(ensures \result == A->array[i])
    _(reads \universe())
{
    return A->array[i];
}

void Update(struct ArrayList *A, size_t i, int v)
    _(requires i < Length(A))
    _(requires \wrapped(A))
    _(writes A)
{
    _(unwrap A)
    _(unwrap (void[A->capacity])(A->array))
    A->array[i] = v;
    _(wrap (void[A->capacity])(A->array))
    _(wrap A)
}

void DisposeArrayList(struct ArrayList *A)
    _(requires \wrapped(A))
    _(writes A)
{
    _(unwrap A)
    _(unwrap (void[A->capacity])(A->array))
    free((void[A->capacity])(A->array));
    A->array = (int *) NULL;
    free(A);
}

void Add(struct ArrayList *A, int v)
    _(requires Length(A) < 100000)
    _(maintains \wrapped(A))
    _(ensures Length(A)==\old(Length(A))+1)
    // Note: there should be additional postconditions / annotations on the
    // elements of A after returning
    _(writes A)
{
    _(unwrap A)
    _(unwrap (void[A->capacity])(A->array))
    if (A->capacity == A->length) {
        size_t i;
        int *tmp;
        size_t newCapacity;

        newCapacity = A->capacity * 2 + 1;

        tmp = malloc(sizeof(*A->array) * newCapacity);
        _(assume tmp != NULL)

        i = 0;
        while (i < A->length)
            _(writes \array_range(tmp, A->length))
        {
            tmp[i] = A->array[i];
            i = i + 1;
        }
        free((void[A->capacity])(A->array));
        A->capacity = newCapacity;
        A->array = tmp;
        _(ghost A->\owns =  {(void[A->capacity])(A->array)});
    }
    A->array[A->length] = v;
    A->length++;
    _(wrap (void[A->capacity])(A->array))
    _(wrap A)
}

int main_test()
{
    size_t N = 42;
    struct ArrayList *A = CreateArrayList(N);
    size_t i = 0;

    while (i < N)
        _(invariant \wrapped(A))
        _(invariant Length(A)==i)
    {
        Add(A, (int)i);
        i++;
    }
    DisposeArrayList(A);
    return 0;
}

/*`
Verification of ArrayList#adm succeeded.
Verification of Length succeeded.
Verification of CreateArrayList succeeded.
Verification of MakeEmpty succeeded.
Verification of Select succeeded.
Verification of Update succeeded.
Verification of DisposeArrayList succeeded.
Verification of Add succeeded.
Verification of main_test succeeded.
Verification of Length#reads succeeded.
`*/
