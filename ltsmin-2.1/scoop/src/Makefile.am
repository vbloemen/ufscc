
.NOTPARALLEL :

if HAVE_HASHMAP
bin_PROGRAMS = scoop
scoop_SOURCES =
endif

BUILT_SOURCES  = ParserExpressionsGlobals.hs
BUILT_SOURCES += ParserExpressions.hs
BUILT_SOURCES += ParserUntil.hs
BUILT_SOURCES += Parser.hs

GHC_SCOOP = -O -XBangPatterns -outputdir. -i${top_srcdir}/src
GHC_SCOOP_STATIC = $(GHC_SCOOP)
GHC_SCOOP_SHARED = -osuf dyn_o -hisuf dyn_hi -dynamic -fPIC $(GHC_SCOOP) -XCPP

.PHONY: scoop libscoop.so

scoop$(EXEEXT):
	ghc --make $(GHCFLAGS) $(GHC_SCOOP_STATIC) -o scoop ${top_srcdir}/src/scoop.hs

%.hs: %.y
	happy -o $@ $<

#%.o: %.hs
#	ghc -c -O -o $@ $<

clean::
	$(RM) scoop libscoop.so $(BUILT_SOURCES) *.hi *.o *.dyn_hi *.dyn_o LibScoop_stub.h

libscoop.so: LibScoop.hs lib_scoop.c
	ghc --make $(GHCFLAGS) $(GHC_SCOOP_SHARED) -c ${top_srcdir}/src/LibScoop.hs
	ghc $(GHCFLAGS) $(GHC_SCOOP_SHARED) -optc-O -optc-std=c99 -I. -I${top_srcdir}/src -o lib_scoop.o -c ${top_srcdir}/src/lib_scoop.c
# the test below is necessary for older versions of GHC.
# older versions of GHC generate LibScoop_stub.c which is then necessary in the compilation. 
	if test -f  LibScoop_stub.c ; then \
		ghc --make $(GHCFLAGS) $(GHC_SCOOP_SHARED) -shared -L$(GHC_LIBDIR) ${top_srcdir}/src/LibScoop.hs -o libscoop.so LibScoop_stub.dyn_o lib_scoop.o -lffi -lHSrts_thr-$(GHC_VERSION) ; \
	else \
		ghc --make $(GHCFLAGS) $(GHC_SCOOP_SHARED) -shared -L$(GHC_LIBDIR) ${top_srcdir}/src/LibScoop.hs -o libscoop.so lib_scoop.o -lffi -lHSrts_thr-$(GHC_VERSION); \
	fi

all-local: libscoop.so

install-exec-local: libscoop.so
	mkdir -p $(DESTDIR)$(libdir) && $(INSTALL) libscoop.so $(DESTDIR)$(libdir)/libscoop.so

EXTRA_DIST = \
 Auxiliary.hs \
 DataSpec.hs \
 InputParser.hs \
 Linearise.hs \
 MLPPE.hs \
 ParserExpressionsGlobals.y \
 Parser.y \
 scoop.h \
 StepPA.hs \
 ToPA.hs \
 Usage.hs \
 Confluence.hs \
 DeadVariable.hs \
 lib_scoop.c \
 LPPE.hs \
 Parelm.hs \
 ParserExpressions.y \
 Processes.hs \
 scoop.hs \
 Sumelm.hs \
 ToPRISM.hs \
 Constelm.hs \
 Expressions.hs \
 LibScoop.hs \
 MaximalProgress.hs \
 ParserAux.hs \
 ParserUntil.y \
 Regularise.hs \
 Simplify.hs \
 ToAUT.hs \
 ToTrans.hs
