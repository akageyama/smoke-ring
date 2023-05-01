#-------------------------------------------------------------------
# class-hpc-smoke-ring: A simple sample field solver.
#
#    by Akira Kageyama, Kobe University, Japan.
#       email: sgks@mac.com
#
#    Copyright 2018 Akira Kageyama
#
#    This software is released under the MIT License.
#
#-------------------------------------------------------------------
#    src/Makefile
#-------------------------------------------------------------------

.PHONY: data clean

.SUFFIXES:
.SUFFIXES: .f90 .o

FC = gfortran
FFLAGS = -Ofast


.f90.o:
	$(FC) $(FFLAGS) -c $<


OBJS = constants.o \
       ut.o \
       grid.o \
       job.o \
       params.o \
       debug.o \
       field.o \
       slicedata.o \
       solver.o \
       main.o

data: smoke_ring params.namelist
	./smoke_ring params.namelist

smoke_ring: $(OBJS)
	$(FC) $(FFLAGS) -o smoke_ring $(OBJS)

grid.o: constants.o debug.o ut.o
ut.o: constants.o
params.o: constants.o ut.o
debug.o: constants.o ut.o params.o
field.o: constants.o grid.o
slicedata.o: ut.o field.o params.o debug.o solver.o
solver.o: constants.o grid.o ut.o field.o debug.o params.o job.o
job.o: constants.o ut.o
main.o: constants.o \
        ut.o \
        grid.o \
        job.o \
        params.o \
        debug.o \
        field.o \
        slicedata.o \
        solver.o

clean:
	rm -f smoke_ring core ${OBJS} *.mod *.L
	rm -f _data_slice*
