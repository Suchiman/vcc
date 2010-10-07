TARGETS = $(wildcard *.c)
NEWSYNTAX_TARGETS = $(wildcard *NewSyntax.c) bakery.c
STD_TARGETS = $(filter-out $(NEWSYNTAX_TARGETS),$(TARGETS))

default: verify

# TODO check Client.c and SpinLock.c for the non-SIMPLE_SPIN_LOCKS case

verify: newsyntax_targets std_targets
	
std_targets:
	vcc /s $(STD_TARGETS)

newsyntax_targets:
	vcc /s /2 $(NEWSYNTAX_TARGETS)
