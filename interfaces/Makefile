# flips/interfaces Makefile
# Part of FLIPS

#    FLIPS - Fortran Linear Inverse Problem Solver
#    Copyright (C) 2006--2009 University of Oulu
#    Written by Mikko Orispaa <mikko.orispaa@oulu.fi>
#    Licensed under BSD License.


include ../Make.defs

LIBFLIPS=../flips.o
FLIPS_INC=-I..

IFACE=flipseng32

TESTS=gulips_manual_test comp_test comp_test_odet
RTESTS=real_comp real_test cplx_comp real_test_odet real_test2

all: $(IFACE)

$(IFACE): %: %.f90
	$(FC) $(FLIPS_INC) $(FFLAGS) $< -o $@ $(LIBFLIPS)

clean:
	rm -f $(IFACE)
	rm -f *.dat 
	rm -f *.o *.mod
	rm -f a.out

