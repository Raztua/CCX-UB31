
CFLAGS = -Wall -O2 -g -I ../../../SPOOLES.2.2 -DARCH="Linux" -DSPOOLES -DARPACK -DMATRIXSTORAGE -DNETWORKOUT
FFLAGS = -Wall -O2 -fopenmp -cpp

CC=cc
FC=gfortran

.c.o :
	$(CC) $(CFLAGS) -c $<
.f.o :
	$(FC) $(FFLAGS) -c $<

include Makefile.inc

SCCXMAIN = ccx_2.23.c

OCCXF = $(SCCXF:.f=.o)
OCCXC = $(SCCXC:.c=.o)
OCCXMAIN = $(SCCXMAIN:.c=.o)

DIR=../../../SPOOLES.2.2

LIBS = \
       $(DIR)/spooles.a \
	../../../ARPACK/libarpack_INTEL.a \
       -lpthread -lm -lc

ccx_2.23: $(OCCXMAIN) ccx_2.23.a  $(LIBS)
	./date.pl; $(CC) $(CFLAGS) -c ccx_2.23.c; $(FC) -fopenmp -Wall -O2 -static-libgcc -static-libgfortran -o $@ $(OCCXMAIN) ccx_2.23.a $(LIBS)

ccx_2.23.a: $(OCCXF) $(OCCXC)
	ar vr $@ $?

calinput.o: usercomb_module.o codecheck_module.o
userloadcombinations.o: usercomb_module.o
usercodecheck.o: codecheck_module.o ub31_module.o
e_c3d_ub31.o: ub31_module.o
resultsmech_ub31.o: ub31_module.o codecheck_module.o
getub31stx.o: ub31_module.o codecheck_module.o
userbeamreleases.o: ub31_module.o
userbeamoutputs.o: ub31_module.o
mafillsm.o: ub31_module.o
e_c3d_uconn6.o: uconn6_module.o
resultsmech_uconn6.o: uconn6_module.o
userconnectors.o: uconn6_module.o
userconnectoroutputs.o: uconn6_module.o
uconn_plasticity.o: uconn6_module.o

