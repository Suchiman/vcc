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

    invariant(
       is_malloc_root(this)
    && length <= capacity
    && keeps(as_array(array, capacity))
    && typed(as_array(array, capacity))
    && is_malloc_root(as_array(array, capacity)))
};

ispure
size_t Length(struct ArrayList *A)
    reads(A)
    requires(wrapped(A))
    returns(A->length)
{
    return A->length;
}

struct ArrayList *CreateArrayList(size_t InitialCapacity)
  requires(0 < InitialCapacity)
  ensures(wrapped(result))
  ensures(Length(result)==0)
  ensures(is_fresh(result))
{
    struct ArrayList *A;
    spec(obj_t arr;)

    A = malloc(sizeof(*A));
    assume(A != NULL);

    A->capacity = InitialCapacity;
    A->length = 0;
    A->array = malloc(sizeof(*A->array) * InitialCapacity);
    assume(A->array != NULL);

    speconly(arr = as_array(A->array, InitialCapacity);)
    wrap(arr);
    set_owns(A, SET(arr));
    wrap(A);
    return A;
}

void MakeEmpty(struct ArrayList *A)
    maintains(wrapped(A))
    ensures(Length(A)==0)
    writes(A)
{
    unwrap(A);
    A->length = 0;
    wrap(A);
}

ispure
int Select(struct ArrayList *A, size_t i)
    requires(i < Length(A))
    requires(wrapped(A))
    reads(set_universe())
{
    return A->array[i];
}

void Update(struct ArrayList *A, size_t i, int v)
    requires(i < Length(A))
    requires(wrapped(A))
    writes(A)
{
    unwrap(A);
    unwrap(as_array(A->array, A->capacity));
    A->array[i] = v;
    wrap(as_array(A->array, A->capacity));
    wrap(A);
}

void DisposeArrayList(struct ArrayList *A)
    requires(wrapped(A))
    writes(A)
{
    unwrap(A);
    unwrap(as_array(A->array, A->capacity));
    free(as_array(A->array, A->capacity));
    A->array = (int *) NULL;
    free(A);
}

void Add(struct ArrayList *A, int v)
    requires(Length(A) < 100000)
    maintains(wrapped(A))
    ensures(Length(A)==old(Length(A))+1)
    // Note: there should be additional postconditions / annotations on the
    // elements of A after returning
    writes(A)
{
    unwrap(A);
    unwrap(as_array(A->array, A->capacity));
    if (A->capacity == A->length) {
        size_t i;
        int *tmp;
        size_t newCapacity;

        newCapacity = A->capacity * 2 + 1;

        tmp = malloc(sizeof(*A->array) * newCapacity);
        assume(tmp != NULL);

        i = 0;
        while (i < A->length)
            writes(array_range(tmp, A->length))
        {
            tmp[i] = A->array[i];
            i = i + 1;
        }
        free(as_array(A->array, A->capacity));
        A->capacity = newCapacity;
        A->array = tmp;
        set_owns(A, SET(as_array(A->array, A->capacity)));
    }
    A->array[A->length] = v;
    A->length++;
    wrap(as_array(A->array, A->capacity));
    wrap(A);
}

int main_test()
{
    size_t N = 42;
    struct ArrayList *A = CreateArrayList(N);
    size_t i = 0;

    while (i < N)
        invariant(wrapped(A))
        invariant(Length(A)==i)
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
