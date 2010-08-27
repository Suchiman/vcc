all:
	$(MAKE) -C ..

TARGETS = $(filter-out *vcc_split*,$(wildcard *.c))
NOTARGETS = 01_min_assert.c 02_div_assert.c
ITTARGETS = 04_insertionsort_perm.c 04_bubblesort_perm.c 07_table.c
STDTARGETS = $(filter-out $(NOTARGETS) $(ITTARGETS),$(TARGETS))

verify: stdtargets ittargets

stdtargets:
	vcc /2 /s $(STDTARGETS)

ittargets:
	vcc /2 /s /it $(ITTARGETS)

clean:
	rm -rf *.i *.bpl *.vccmodel log out actual.txt expected.txt
