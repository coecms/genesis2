FC=ifort
LD=$(FC)
COPTS=-c -g -O0
LOPTS=-g -O0

default: genesis2

.SUFFIXES:
.SUFFIXES: .f90 .o

OBJS=mod_indata.o mod_rundata.o mod_cntlscm.o
SRCS=$(subst .o,.f90,$(OBJS))
MODS=$(subst .o,.mod,$(OBJS))

%.o : %.f90
	$(FC) $(COPTS) -o $@ $<

genesis2.o : $(OBJS)

genesis2: genesis2.o $(OBJS)
	$(LD) $(LOPTS) -o $@ $^

clean:
	rm -rf $(OBJS) $(MODS) genesis2.o genesis2

.PHONY: default clean
