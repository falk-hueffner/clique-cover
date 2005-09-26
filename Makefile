EXEC	  = ecc

SOURCES   = \
	Util.ml		\
	IntMap.ml	\
	IntSet.ml	\
	PSQueue.ml	\
	Graph.ml	\
	Cliques.ml	\
	ECC.ml		\
	Branch.ml	\
	KSW.ml		\
	Test.ml		\
	Main.ml

C_SOURCES = \


INCLUDES  = #-I extlib-dev
LIBS	  = unix.cmxa

CC	  = gcc
OCAMLC    = ocamlc -w A -warn-error A $(INCLUDES)
OCAMLOPT  = ocamlopt -warn-error A $(INCLUDES)
OCAMLDEP  = ocamldep $(INCLUDES)

#PROFILE  = -p

CFLAGS	  = -std=c99 -g -I $(HOME)/include
CFLAGS	 += -O3 $(shell gcc-arch) $(PROFILE) -funroll-all-loops -ffast-math
CFLAGS	 += -W -Wall -Wno-unused -Werror
#CFLAGS	 += -DNDEBUG

all: depend $(EXEC) doc/index.html

OBJS = $(SOURCES:.ml=.cmx)
C_OBJS = $(C_SOURCES:.c=.o)
MLIS := $(wildcard *.mli)

ecc: $(OBJS) $(C_OBJS)
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

clean:
	rm -f $(EXEC) core gmon.out
	rm -f *.cm[iox] *.o $(PROJ)

realclean: clean
	rm -f *~ *.old *.bak

.depend: depend

depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend
#	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend
