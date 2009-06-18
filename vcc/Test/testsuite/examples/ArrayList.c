// ----------------------------------------------------------------------------
// ArrayList.c
//
// The capacity of a ArrayList is the number of elements the ArrayList can hold. 
// As elements are added to a ArrayList, the capacity is automatically increased as required through reallocation. 
// Elements in this collection can be accessed using an integer index. Indexes in this collection are zero-based.
//
// ----------------------------------------------------------------------------

#include <stdlib.h>
#include "vcc.h"

#define bool _Bool

struct ArrayList{
    size_t capacity;
    size_t length;
    int *array;

    invariant(
       this->capacity >=0 
    && keeps(as_array(this->array, this->capacity))
    && typed(as_array(this->array, this->capacity))
    && is_malloc_root(this)
    && is_malloc_root(as_array(this->array, this->capacity))
    && this->length>=0 
    && this->length<= this->capacity )
};

ispure 
size_t Length(struct ArrayList * A)
    reads(A)
    requires (wrapped(A))
    ensures (result == A->length)
{
    return A->length;
}

struct ArrayList * CreateArrayList(size_t InitialCapacity)
  requires(0<InitialCapacity)
  ensures (wrapped(result))
  ensures (Length(result)==0)
  ensures (is_fresh(result))
{
    struct ArrayList *A;
    spec(obj_t arr;)

    A = malloc(sizeof(struct ArrayList));

    A->capacity = InitialCapacity;
    A->length = 0;
    A->array = malloc(sizeof(int) * InitialCapacity);
    
    arr = as_array(A->array, InitialCapacity);
    wrap(arr);
    set_owner(arr, A);
    wrap(A);
    return A;
}

void MakeEmpty(struct ArrayList * A)
    requires(wrapped(A))
    ensures(wrapped(A))
    ensures(Length(A)==0)
    writes(A)
{
    unwrap(A);
        A->length = 0;
    wrap(A);
}

int Select(struct ArrayList * A, size_t i)
  requires (i>=0 && i < Length(A))
  requires (wrapped(A))
  reads(A)
{
    assert(inv(A));
    return A->array [i];
}

void Update(struct ArrayList * A, size_t i, int v)
    requires (i>=0 && i < Length(A))
    requires (wrapped(A))
    writes(A)
{   unwrap(A);
        unwrap(as_array(A->array, A->capacity));
            A->array[i] = v;
        wrap(as_array(A->array, A->capacity));
    wrap(A);
}



void DisposeArrayList(struct ArrayList * A)
    requires(wrapped(A))
    writes(A)
{   
    spec(obj_t arr;)

    unwrap(A);
    arr = as_array(A->array, A->capacity);
        unwrap(arr) ;
        // giveup_owner(arr, A);
        speconly(free((void *)arr);)
        A->array = (int *) NULL;
        free(A);
}

void Add(struct ArrayList * A, int v)
    requires(wrapped(A))  
    requires(Length(A) < 100000)
    ensures (wrapped(A))
    ensures (Length(A)==old(Length(A))+1)
    writes(A)
{   
    unwrap(A);
        unwrap(as_array(A->array, A->capacity));

        if (A->capacity == A->length) {
            size_t i;
            int *tmp;
            size_t k;
            spec(obj_t newArr;)
            spec(obj_t oldArr;)
            size_t newCapacity;

            i = 0;

            newCapacity = A->capacity * 2 + 1;

            tmp = malloc(sizeof(int) * newCapacity);

            while (i < A->length) 
              invariant(i >= 0)
              writes (array_range(tmp, A->length))
            {
              tmp[i] = A->array[i];
              i = i + 1;
            }
            oldArr = as_array(A->array, A->capacity);
            A->capacity = newCapacity;
            A->array = tmp;
            newArr = as_array(A->array, A->capacity);
            giveup_owner(oldArr, A);
            free((void *)oldArr);
            set_owner(newArr, A);
        }

        A->array[A->length] = v;
        A->length = A->length + 1;
        wrap(as_array(A->array, A->capacity));

    wrap(A);        
}


int main_test()
{
    struct ArrayList * A;
    size_t N;
    size_t i; 
    int j;

    N = 1;
    A = CreateArrayList(N);
    assert(Length(A)==0);
    i = 0;
    while(i < N)
        invariant (wrapped(A))       
    {
        assume(Length(A) < 100000);
        Add(A, unchecked((int)i));
        i = unchecked(i + 1);
    }
    //assert (Length(A)==N);

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
