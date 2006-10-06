EXECS	  = ecc random-graph gnmp

SOURCES   = \
	Util.ml		\
	IntMap.ml	\
	IntSet.ml	\
	PSQueue.ml	\
	Graph.ml	\
	Cliques.ml	\
	ECC.ml		\
	Branch.ml	\
	Sweep.ml	\
	KSW.ml		\
	InsertAbsorb.ml

C_SOURCES = \


INCLUDES  = #-I extlib-dev
LIBS	  = unix.cmxa

CC	  = gcc
OCAMLC    = ocamlc -g -warn-error A $(INCLUDES)
OCAMLOPT  = ocamlopt -warn-error A $(INCLUDES)
OCAMLDEP  = ocamldep $(INCLUDES)

#PROFILE  = -p

CFLAGS	  = -std=c99 -g -I $(HOME)/include
CFLAGS	 += -O3 $(shell gcc-arch) $(PROFILE) -funroll-all-loops -ffast-math
CFLAGS	 += -W -Wall -Wno-unused -Werror
#CFLAGS	 += -DNDEBUG

all: depend $(EXECS) doc/index.html

OBJS = $(SOURCES:.ml=.cmx)
DBG_OBJS = $(SOURCES:.ml=.cmo)
C_OBJS = $(C_SOURCES:.c=.o)
MLIS := $(wildcard *.mli)
DBG_LIBS = $(LIBS:.cmxa=.cma)

ecc: $(OBJS) Main.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

ecc-dbg: $(DBG_OBJS) Main.cmo $(C_OBJS)
	$(OCAMLC) $(PROFILE) -o $@ $(DBG_LIBS) $^

random-graph: $(OBJS) Random-graph.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

gnmp: $(OBJS) Gnmp.cmx
	$(OCAMLOPT) $(PROFILE) -o $@ $(LIBS) $^

doc/index.html: $(MLIS)
	mkdir -p doc
	ocamldoc -html -d doc $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) $(PROFILE) -c $<

VER	= 1.1
DIR	= ecc-$(VER)

dist: all
	rm -rf $(DIR)
	mkdir $(DIR)
	cp COPYING README $(DIR)
	cp .depend $(DIR)
	cp Makefile *.ml *.mli $(DIR)
	GZIP=--best tar -cvvzf $(DIR).tar.gz $(DIR)

clean:
	rm -f $(EXECS) ecc-dbg core gmon.out
	rm -f *.cm[iox] *.o $(PROJ)

realclean: clean
	rm -f *~ *.old *.bak

.depend: depend

depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend
#	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend
