#include <vcc.h>

struct F {
	int value;
	
	def_group(g, vcc(claimable))
	
	in_group(g)
	volatile int vol;
	
	invariant(keeps((struct F::g*)this))
};

int test(struct F *f)
	requires(wrapped(f::g))
	writes(f::g)
{
	spec(claim_t c;)
	
	assert(closed((struct F::g*)f));
	
	spec(c = claim((struct F::g*)f, closed((struct F::g*)f));)
}

/*`
Verification of F#adm succeeded.
Verification of test succeeded.
`*/
