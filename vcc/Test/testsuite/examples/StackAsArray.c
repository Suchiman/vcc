#include <stdlib.h>
#include <limits.h>

#include <vcc.h>

#ifdef VERIFY
#define bool _Bool
#else
#define bool int
#endif

spec(
struct AbsStack {
	size_t high_mark;
	size_t capacity;
    
    spec(size_t entries[size_t];)
    invariant( this->high_mark <= this->capacity )
    invariant( is_malloc_root(this) )
};
)


struct Stack{
    size_t topOfStack;
    size_t capacity;

    size_t *elements;

    spec( struct AbsStack ^abs; )
    spec( obj_t elementsAsArray;)

    invariant( this->capacity >= 0 )
    invariant( elementsAsArray == as_array(elements, capacity ) )

    invariant( keeps(abs, elementsAsArray) )

    invariant( is_malloc_root(this) )
    invariant( is_malloc_root(elementsAsArray) )

    invariant( topOfStack>=0 && topOfStack <= capacity )
    invariant( AbstractionRelation( this ) )
};

spec(
ispure
bool AbstractionRelation( struct Stack *stack ) 
//	reads( stack, stack->abs, array_range(stack->elements, stack->capacity) )
	reads( set_universe() )
	returns ((stack->capacity == stack->abs->capacity) &&
			 (stack->topOfStack == stack->abs->high_mark) &&
             (forall( size_t idx ; { stack->abs->entries[idx] }
					  (0 <= idx) && ( idx < stack->topOfStack ) 
						  ==> (stack->elements[idx] == stack->abs->entries[idx] ) ) ) )
	;
)


frameaxiom
bool IsEmpty(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == (S->abs->high_mark == 0))
{
	assert(inv(S));
	return S->topOfStack == 0;
}

ispure 
bool IsFull(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == (S->abs->high_mark == S->abs->capacity))
{
	assert(inv(S));
	assert(inv(S->abs));
    return S->topOfStack == S->capacity;
}

size_t Length(struct Stack * S)
    reads(S)
    requires(wrapped(S))
    ensures(result == S->abs->high_mark)
{
	assert(inv(S));
	return S->topOfStack;
}

struct Stack * CreateStack( unsigned int max_capacity )
    requires( max_capacity > 0 )
    ensures(result == NULL || wrapped(result))
    ensures(result == NULL || old(nested(result)))
    ensures(result == NULL || IsEmpty(result))
    ensures(result == NULL || max_capacity == result->abs->capacity)
    ensures(result == NULL || is_fresh(result))
    writes(set_empty())
{
	struct Stack * S;
	
    S = (struct Stack*)0;

	S = malloc(sizeof(struct Stack));
	if (S != NULL) {
    S->topOfStack = 0;
    S->capacity   = max_capacity;
    S->elements    = malloc(sizeof(size_t) * S->capacity );
    if (S->elements != NULL) {

speconly(

      S->elementsAsArray = as_array(S->elements, S->capacity);
      S->abs = spec_malloc<struct AbsStack>();
      S->abs->high_mark = S->topOfStack ; 
      S->abs->capacity  = S->capacity;
      
      set_owns( S->abs, set_empty() );
      assert( S->abs->high_mark == S->topOfStack );

      wrap( S->elementsAsArray );
      set_owner( S->elementsAsArray, S );
        
      wrap( S->abs );
      set_owner( S->abs, S );
)
      wrap(S);
    } 
    else return NULL;
	}
  return S;
}


void DisposeStack(struct Stack * S)
    requires(wrapped(S))
    writes(S)
{
    assert( in_domain( S->elementsAsArray, S ) );
    unwrap(S);

    assert( set_in( S->elementsAsArray, old(owns(S))));
    unwrap( S->elementsAsArray );
    assert( mutable( S->elementsAsArray ) );
    free( (void*) S->elementsAsArray  );


speconly(
    unwrap( S->abs );
)

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
speconly(
	unwrap( S->abs );
	S->abs->high_mark = S->topOfStack;
	wrap( S->abs );
)
    wrap(S);
}


size_t Top(struct Stack * S)
    requires(!IsEmpty(S))
    requires(wrapped(S))
	returns( S->abs->entries[S->abs->high_mark - 1] )
    reads(S)
{
    assert( in_domain(S->elementsAsArray, S));
    return S->elements[ S->topOfStack-1 ];
}

void Pop(struct Stack * S)
    requires(wrapped(S))  
    ensures(wrapped(S))
    requires(!IsEmpty(S))
    writes(S)
    ensures(!IsFull(S))
    ensures( S->abs->high_mark == old(S->abs->high_mark) - 1 )
	ensures( forall( size_t idx;
					 (0 <= idx) && ( idx < S->abs->high_mark ) 
						  ==> (old(S->abs->entries[idx]) == S->abs->entries[idx] ) ) )
{
    unwrap(S);
    S->topOfStack = S->topOfStack-1;
speconly(
	unwrap( S->abs );
	S->abs->high_mark--;
	wrap( S->abs );
)
    wrap(S);
}

void Push(struct Stack * S, size_t X)
    requires(wrapped(S))  
    ensures(wrapped(S))
    requires(!IsFull(S))
    ensures(!IsEmpty(S))
	ensures( old(S->abs->high_mark) + 1 == S->abs->high_mark ) 
	ensures( forall( size_t idx;
					 (0 <= idx) && ( idx < old(S->abs->high_mark) ) 
						  ==> (old(S->abs->entries[idx]) == S->abs->entries[idx] ) ) )
	ensures( S->abs->entries[S->abs->high_mark - 1] == X )
    writes(S)
{
    assert( S->topOfStack < S->capacity );
    assert( in_domain(S->elementsAsArray, S));
    unwrap(S);
    assert( set_in( S->elementsAsArray, old(owns(S))));
    unwrap( S->elementsAsArray );
    S->elements[ S->topOfStack ] = X;
    wrap( S->elementsAsArray );
    S->topOfStack = S->topOfStack+1;
speconly(
	unwrap( S->abs );
	assert( old( S->topOfStack ) == S->abs->high_mark );
	S->abs->entries[S->abs->high_mark] = X;
	S->abs->high_mark = S->topOfStack;
	wrap( S->abs );
)
    wrap(S);      
}


no_reads_check ispure
bool IsIn(struct Stack * S, size_t value)
    requires(wrapped(S))
    reads(S)
    ensures(result <==> exists(size_t v; (0 <= v) && (v < S->abs->high_mark) && S->abs->entries[v] == value))
{
    unsigned int index;
    bool return_value = false;

    assert( in_domain(S->elementsAsArray, S));
    assert( in_domain(S->abs, S) );

    for ( index = 0; index < S->topOfStack; index++ ) 
        invariant( 0 <= index )
        invariant( index <= S->topOfStack )
        invariant( return_value ==> (exists(size_t v; (0 <= v) && (v < index); (S->abs->entries[v] == value))))
        invariant( !return_value ==> (forall(size_t v; (0 <= v) && (v < index); (S->abs->entries[v] != value))))
    {
        if ( S->elements[index] == value ) {
            assert( S->abs->entries[index] == value);
            return_value = true;
        }
    }
    return return_value;
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
