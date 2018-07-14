#
# Makefile for smoke-ring.
#
#     Akira Kageyama (kage@port.kobe-u.ac.jp)
#            2018.04.12: Copied from boxfluid.
#

.SUFFIXES:
.SUFFIXES: .f90 .o

FC = gfortran
FFLAGS =
FLIB =


.f90.o:
	$(FC) $(FFLAGS) -c $<


OBJS = constants.o \
       ut.o \
       grid.o \
       namelist.o \
       debug.o \
       field.o \
       slicedata.o \
       solver.o \
       main.o

smoke-ring: main.o
	$(FC) $(FFLAGS) -o smoke-ring ${OBJS} ${FLIB}

data: smoke-ring sample.namelist
	./smoke-ring sample.namelist

grid.o: constants.o debug.o ut.o
ut.o: constants.o
namelist.o: constants.o ut.o
debug.o: constants.o ut.o namelist.o
field.o: constants.o grid.o
slicedata.o: ut.o field.o namelist.o debug.o solver.o
solver.o: constants.o grid.o ut.o field.o debug.o namelist.o



main.o: constants.o \
        ut.o \
        grid.o \
        namelist.o \
        debug.o \
        field.o \
        slicedata.o \
        solver.o

clean:
	rm -f smoke-ring core ${OBJS} *.mod *.L
	rm -f _data_slice*
