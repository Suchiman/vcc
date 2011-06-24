#include "vcc.h"

struct point {
    int x;
    int y;
};

struct rect {
    struct point ll;
    struct point ur;
	
    invariant( ll.x <= ur.x && ll.y <= ur.y )
    invariant( keeps( &ll ) )
    invariant( keeps( &ur ) )
};

isadmissibilitycheck
void custom_admissibility_check_rect( struct rect *r ) 
{
    // .. assert() and assume() 
    havoc_others( r );
    // .. assert() and assume() are allowed here. 
    // The state or meta must not be modified.
}

/*`
Verification of rect#adm succeeded.
Verification of custom_admissibility_check_rect succeeded.
`*/
