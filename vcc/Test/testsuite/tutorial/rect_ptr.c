#include "vcc.h"

struct point {
    int x;
    int y;
};

struct rect {
    struct point *ll;
    struct point *ur;
	
    invariant( inv_rect(this) )
    invariant( keeps( ll ) )
    invariant( keeps( ur ) )
    invariant( ll != ur )
};

spec( ispure bool inv_rect(struct rect * r)
    reads(r)
    returns(r->ll->x <= r->ur->x && r->ll->y <= r->ur->y);
)

spec(ispure bool within_bounds(__in struct rect* r, int dx, int dy)
    reads(r)
    returns( 0 <= r->ll->x + dx && r->ll->x + dx < 1024 &&
             0 <= r->ur->x + dx && r->ur->x + dx < 1024 &&
             0 <= r->ll->y + dy && r->ll->y + dy < 1024 &&
             0 <= r->ur->y + dy && r->ur->y + dy < 1024 );
)

void move(__inout struct rect* r, int dx, int dy)
    maintains(wrapped(r))
    requires(within_bounds(r, dx, dy))
	writes(r)
{	
    unwrap(r);
    unwrap(r->ll);
    unwrap(r->ur);
    r->ll->x = unchecked(r->ll->x + dx);
    r->ll->y = unchecked(r->ll->y + dy);
    r->ur->x = unchecked(r->ur->x + dx);
    r->ur->y = unchecked(r->ur->y + dy);
    wrap(r->ur);
    wrap(r->ll);
    wrap(r);
}

/*`
Verification of rect#adm succeeded.
Verification of move succeeded.
Verification of within_bounds#reads succeeded.
Verification of inv_rect#reads succeeded.
`*/
