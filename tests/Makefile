# flips/tests Makefile
# Part of FLIPS

#    FLIPS - Fortran Linear Inverse Problem Solver
#    Copyright (C) 2005 University of Oulu
#    Written by Mikko Orispaa <mikko.orispaa@oulu.fi>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

include ../Make.defs

LIBFLIPS=../flips.o
FLIPS_INC=-I../

#TESTS=gulips_manual_test rbuf_size_test_d inv_mat_test comp_test fobcopytest get_test comp_test_odet
#RTESTS=rbuf_size_test real_comp cplx_comp real_test real_comp2 real_test2 real_test_odet ilkkatest

TESTS=flipsdeletetest flipsdeletetest_s fdel2 cholspeedtest cholfactest cholspeed_vec resize_test_s resize_test_z gulips_manual_test bwtest bw_remove_test comp_test resizetest10
RTESTS=real_test2 real_comp2 cplx_comp real_test_odet real_test2_new cvresizetest resize_test2_d

#TESTS=
#RTESTS=cvresizetest

#TESTS=gulips_manual_test gulips_manual_test2 fullcovtest cholspeed_vec
#RTESTS=

# Uncomment STREAMTESTS line only if you have compiled
# FLIPS with DF2003_STREAM (they use F2003 extension for stream I/O)
#STREAMTESTS=resize_test_s resize_test_d resize_test_c resize_test_z rinv_speed_test


all: $(TESTS) $(RTESTS) $(STREAMTESTS)

$(RTESTS): rand.o flipstest.o

rand.o: rand.f90
	$(FC) $(FLIPS_INC) $(FFLAGS) -c $< -o $@ $(LIBFLIPS)

flipstest.o: flipstest.f90
	$(FC) $(FLIPS_INC) $(FFLAGS) -c $< -o $@ $(LIBFLIPS)

$(TESTS): %: %.f90
	$(FC) $(FLIPS_INC) $(FFLAGS) $< -o $@ $(LIBFLIPS)

$(RTESTS): %: %.f90
	$(FC) $(FLIPS_INC) $(FFLAGS) $< flipstest.o rand.o -o $@ $(LIBFLIPS)

clean:
	rm -f $(TESTS) $(RTESTS) $(STREAMTESTS)
	rm -f *.dat 
	rm -f *.o *.mod
	rm -f a.out
	rm -f rio_sol.data
