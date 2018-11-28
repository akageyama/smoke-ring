#
# Makefile for analyzier of kindanb
#   Created by:  Akira Kageyama (kage@port.kobe-u.ac.jp)
#           on:  2008.06.04
#   revised on: 2018.04.12 for smoke-ring.
#


.PHONY: figs clean

.SUFFIXES:
.SUFFIXES: .f90 .o

# FC = pgf90
FC = gfortran

# When you simulate on the machine vector (SX-8R), you need this.
# FFLAGS = -byteswapio
# When you simulate on the machine scalar,
# FFLAGS =

.f90.o:
	$(FC) $(FFLAGS) -c $<



figs: nloop_list.txt gnuplot_data_generator Workfiles
	for i in `awk '{print $$2}' nloop_list.txt`; \
        do \
          ./make_plotscript.sh $$i; \
          gnuplot 'Workfiles/plot.gp'; \
        done



Workfiles:
	mkdir -p Workfiles


nloop_list.txt: extract_nloop_list
	./extract_nloop_list > nloop_list.txt

extract_nloop_list: extract_nloop_list.o
	$(FC) $(FFLAGS) -o extract_nloop_list  \
		constants.o  \
		extract_nloop_list.o



gnuplot_data_generator: gnuplot_data_generator.o
	$(FC) $(FFLAGS) -o gnuplot_data_generator  \
		constants.o  \
		ut.o  \
		grid.o  \
		turtle.o  \
		gnuplot_data_generator.o


gif:
	cd Workfiles; \
        for i in `ls *.eps`; \
        do \
          echo converting $$i; \
          convert -alpha Remove -density 300x300 $$i $$i.gif; \
        done; \
        echo 'making animation.gif'; \
        convert -delay 10 *.eps.gif animation.gif

view:
	open -a Safari Workfiles/animation.gif



gnuplot_data_generator.o: constants.o ut.o grid.o turtle.o

extract_nloop_list.o: constants.o

ut.o: constants.o

turtle.o: constants.o ut.o		

grid.o:	constants.o ut.o 

clean:
	rm -f *.o *.mod 
	rm -f gnuplot_data_generator extract_nloop_list nloop_list.txt
	rm -f gnuplot_data_generator
	rm -rf Workfiles
