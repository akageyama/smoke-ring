#
# Makefile for smoke-ring.
#
#     Akira Kageyama (kage@port.kobe-u.ac.jp)
#            2018.04.12: Copied from boxfluid.
#
#

.PHONY: data clean

.SUFFIXES:
.SUFFIXES: .f90 .o

FC = gfortran
FFLAGS =


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

data: smoke-ring sample.namelist
	./smoke-ring sample.namelist

smoke-ring: $(OBJS)
	$(FC) $(FFLAGS) -o smoke-ring $(OBJS)

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
