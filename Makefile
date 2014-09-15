FC=ifort
LD=$(FC)
COPTS=-c -g -O0
LOPTS=-g -O0

default: genesis2

.SUFFIXES:
.SUFFIXES: .f90 .o

FIRST_MODS=mod_parameters.o mod_dimensions.o
OBJS=mod_indata.o mod_rundata.o mod_cntlscm.o mod_logic.o mod_inprof.o
OBJS+=mod_inmoses.o mod_inprof.o mod_inobsfor.o mod_radcloud.o
SRCS=$(subst .o,.f90,$(OBJS))
MODS=$(subst .o,.mod,$(OBJS) $(FIRST_MODS))

%.o : %.f90
	$(FC) $(COPTS) -o $@ $<

genesis2.o : $(OBJS) $(FIRST_MODS)

genesis2: genesis2.o $(OBJS) $(FIRST_MODS)
	$(LD) $(LOPTS) -o $@ $^

$(OBJS) : $(FIRST_MODS)

clean:
	rm -rf $(OBJS) $(FIRST_MODS) $(MODS) genesis2.o genesis2

.PHONY: default clean
