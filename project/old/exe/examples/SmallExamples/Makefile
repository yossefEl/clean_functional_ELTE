#	Makefile for the Clean examples programs
#

#
#		make (= make all):
#			compiles examples
#
#		make <name>:
#			compile one example
#
#		make cleanup:
#			remove all files that can be recreated
#


CLEAN_EXAMPLES = \
	hamming pascal squeen war_seq acker invperm reverse \
	str_arit copyfile lqueen revtwice stwice e mulmat \
	rfib tak fsieve nfib sieve twice

all: $(CLEAN_EXAMPLES)

revtwice: revtwice.icl
	$(CLM) $(CLMFLAGS) -h 4m -s 2m revtwice -o revtwice

twice: twice.icl
	$(CLM) $(CLMFLAGS) -h 4m -s 2m twice -o twice

pascal: pascal.icl
	$(CLM) $(CLMFLAGS) -b pascal -o pascal

copyfile: copyfile.icl
	$(CLM) $(CLMFLAGS) -nr copyfile -o copyfile

#
# Cleanup
#

cleanup:
	$(RM) $(CLEAN_EXAMPLES)
	$(RM) $(CLEAN_EXAMPLES:%=Clean\ System\ Files/%.abc)
	$(RM) $(CLEAN_EXAMPLES:%=Clean\ System\ Files/%.o)

#
# Commands
#	

CLM = clm
CLMFLAGS = -nw

.SUFFIXES: .icl

.icl:
	$(CLM) $(CLMFLAGS) $* -o $*
