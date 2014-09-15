FC=ifort
LD=$(FC)
COPTS=-c -g -O0
LOPTS=-g -O0

default: genesis2

.SUFFIXES:
.SUFFIXES: .f90 .o

FIRST_OBJS=mod_parameters.o mod_dimensions.o

NML_OBJS=mod_indata.o mod_rundata.o mod_cntlscm.o mod_logic.o mod_inprof.o
NML_OBJS+=mod_inmoses.o mod_inprof.o mod_inobsfor.o mod_radcloud.o mod_physwitch.o
NML_OBJS+=mod_ingeofor.o

OBJS=$(FIRST_OBJS) $(NML_OBJS) mod_namelist.o

SRCS=$(subst .o,.f90,$(OBJS))
MODS=$(subst .o,.mod,$(OBJS))

%.o : %.f90
	$(FC) $(COPTS) -o $@ $<

genesis2.o : $(OBJS)

genesis2: genesis2.o $(OBJS)
	$(LD) $(LOPTS) -o $@ $^

$(NML_OBJS) : $(FIRST_OBJS)

mod_namelist.o : $(NML_OBJS)

clean:
	rm -rf $(OBJS) $(MODS) genesis2.o genesis2

.PHONY: default clean
