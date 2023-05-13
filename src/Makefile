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

VIS2DLIB_NAME = vv  # <= libvv.a 
VIS2DLIB_DIR  = ../../slice-svg/lib

FLIBS := -I$(VIS2DLIB_DIR) -L$(VIS2DLIB_DIR) -l$(VIS2DLIB_NAME)

this_host := $(shell hostname | cut -c1-3)

ifeq ($(this_host),alf)  # Alfven
        EFPP = ../bin/efpp.py2
        FC = mpinfort
        FFLAGS = 
else ifeq ($(this_host),pif)  # Pi-computer
        EFPP = ../bin/efpp.py
	# FC = mpif90
	FC = mpiifort
        FFLAGS = -xAVX
        FFLAGS = -O0
        FFLAGS += -check all -traceback -debug
else ifeq ($(this_host),ofp)  # Oakforest PACS
        EFPP = ../bin/efpp.py
        FC = mpiifort
        FFLAGS := -O3
        FFLAGS += -qopenmp
        FFLAGS += -axMIC-AVX512
        FFLAGS += -fpe0
        FFLAGS += -ftrapuv
        FFLAGS += -align array64byte
        FFLAGS += -qopt-threads-per-core=1
        #FFLAGS += -CB  # check bounds
        #FFLAGS += -check all
        #FFLAGS += -warn all
        #FFLAGS += -traceback
else ifeq ($(this_host),fes)  # NIFS
        EFPP = ../bin/efpp.py2
        FC = mpifrtpx
        FFLAGS := -Kopenmp
        # FFLAGS += -X03 -Free -NRtrap -Qt -Koptmsg=2
        # FFLAGS += -Haefosux
        # FFLAGS += -g
        # FFLAGS += -Nquickdbg=argchk
        # FFLAGS += -Nquickdbg=subchk
else ifeq ($(HOME),$(ES3_HOME)) # JAMSTEC
        EFPP = ../../../bin/efpp.py2
        #EFPP = ../bin/efpp.py2
        FC = sxmpif03 # at cg-mhd, must sxmpif03, not sxmpif90
        #FC = sxmpif90 # at yyz-relax, must sxmpif90, not sxmpif03
        FFLAGS := -P openmp
        FFLAGS += -ftrace
        FFLAGS += -R transform fmtlist
        FFLAGS += -pvctl fullmsg
         #FFLAGS += -R2
         #FFLAGS += -Wf"-pvctl fullmsg"
         #FFLAGS += -eR
else 
        EFPP = ../bin/efpp.py
        FC = gfortran
        FC = mpif90
        FFLAGS := 
        # FFLAGS += -fopenmp
        FFLAGS += -fcheck=all
        FFLAGS += -Wall
        FFLAGS += -fbounds-check
        FFLAGS += -fcheck-array-temporaries
        FFLAGS += -O0
        FFLAGS += -Wuninitialized
        FFLAGS += -ffpe-trap=invalid,zero,overflow
        FFLAGS += -g
        FFLAGS += -fbacktrace
endif


%.F90: %.ef
	echo $(EFPP)
	$(EFPP) $< > $@

%.o: %.F90
	$(FC) $(FFLAGS) -o $@ -c $< $(FLIBS)

smoke_ring: $(objlist)
	mkdir -p ../data/vis2d
	mkdir -p ../data/restart
	$(FC) $(FFLAGS) -o smoke_ring $(objlist) $(FLIBS)

runme: smoke_ring params.namelist
	mpiexec -n $(mpi_nprocs) ./smoke_ring params.namelist

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
