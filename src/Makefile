#-------------------------------------------------------------------
# class-hpc-smoke-ring: A simple sample field solver.
#
#    by Akira Kageyama, Kobe University, Japan.
#       email: sgks@mac.com
#
#    Copyright 2023 Akira Kageyama
#
#    This software is released under the MIT License.
#
#-------------------------------------------------------------------
#    src/Makefile
#-------------------------------------------------------------------

.PHONY: data clean line runme

.SUFFIXES:

eflist := $(shell ls *.ef)   # e.g., example.ef
filebase := $(basename $(notdir $(eflist))) # => example
f90list := $(addsuffix .F90, $(filebase)) # => example.F90
modlist := $(addsuffix .mod, $(filebase)) # => example.mod
objlist := $(addsuffix .o, $(filebase)) # => example.o

mpi_nprocs := $(shell ../bin/grep_process_num_from_src.sh)

.SECONDARY: $(f90list)  # to avoid deleting F90 files.
       # .SECONDARY: obj/%.F90 does not work (GNU Make 3.81).


%.F90: %.ef
	$(EFPP) $< > $@

%.o: %.F90
	$(FC) $(FFLAGS) -o $@ -c $<

EFPP = ../bin/efpp.py

VIS2DLIB_NAME = vv  # <= libvv.a 
VIS2DLIB_DIR  = /Users/kage/VCS/GitHub/slice-svg/lib

FC = gfortran
FC = mpifort

FFLAGS := -I$(VIS2DLIB_DIR) -L$(VIS2DLIB_DIR) -l$(VIS2DLIB_NAME)
# FFLAGS += -fopenmp
FFLAGS += -fcheck=all
FFLAGS += -Wall
FFLAGS += -fbounds-check
FFLAGS += -fcheck-array-temporaries
FFLAGS += -O0
# FFLAGS += -Wuninitialized

runme: smoke_ring params.namelist
	./smoke_ring params.namelist

smoke_ring: $(objlist)
	mkdir -p ../data/vis2d
	$(FC) $(FFLAGS) -o smoke_ring $(objlist)

-include depend_list.mk

depend_list.mk: *.ef
	../bin/gendep.sh > $@ 

line:
	@echo "="{1..100} | sed 's/[ 0-9]//g' # bash one-liner for a line

clean:
	rm -f smoke_ring core ${objlist} *.mod *.L
	rm -rf depend_list.mk
	rm -rf *.F90
	rm -rf ../data/vis2d
