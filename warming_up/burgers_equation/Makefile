.PHONY: data graph clean

FC = gfortran
FFLAGS =

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $<

OBJS = 	constants.o	\
	ut.o		\
	namelist.o	\
	rk4.o		\
	main.o

graph: plot.gp
	gnuplot 'plot.gp'

plot.gp: data burgers params.namelist generate-plot-script.sh
	./generate-plot-script.sh > plot.gp

burgers: $(OBJS)
	$(FC) $(OBJS) -o burgers

data: burgers params.namelist
	./burgers params.namelist


ut.o: constants.o
namelist.o: ut.o
rk4.o: constants.o namelist.o ut.o
main.o: constants.o namelist.o rk4.o

clean:
	rm -rf $(OBJS) burgers plot.gp *.mod output.data.*
