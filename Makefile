FC=ifort
LD=$(FC)
COPTS=-c -g -O0
LOPTS=-g -O0

default: genesis2

.SUFFIXES:
.SUFFIXES: .f90 .o

OBJS=
SRCS=$(OBJS,.o=.f90)

.f90.o:
	$(FC) $(COPTS) -o $< $^

genesis2: genesis2.o $(OBJS)
	$(LD) $(LOPTS) -o $@ $^

clean:
	rm -rf $(OBJS) $(MODS) genesis2

.PHONY: default clean
