//`/newsyntax
#include <vcc.h>

struct F {
	int value;
	
	_(group  _(claimable) g)
	
	_(:g)
	volatile int vol;
	
	_(invariant \mine((struct F::g*)\this))
};

int test(struct F *f)
	_(requires \wrapped(f::g))
	_(writes f::g)
{
	_(ghost \claim c)
	
	_(assert ((struct F::g*)f)->\consistent)
	
	_(ghost c = \make_claim({(struct F::g*)f}, ((struct F::g*)f)->\consistent))
}

/*`
Verification of F#adm succeeded.
Verification of test succeeded.
`*/
