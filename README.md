# class-hpc-smoke-ring

A simple sample field solver, or a CFD (Computational Fluid Dynamics)
code for the class "HPC", which is for undergraduate students of
Department of Engineering, Kobe University, Japan.

## Physical Model
A gas contained in a rectangular box is driven by a localized force
near a side plane of the box. The force drives the gas fluid toward
the other side, rsulting in the formation of well-known vortex-ring,
or smoke-ring.

## Program structure

--+--src (Simulation code. The output is a file "_data_slice".)
  |
  +--slice_grapher (Visualization code. It reads "_data_slice"
                    and makes an animated gif of the flow in a
                    a cross section.)

## Prerequisite

- Fortran 2003 compiler, for the simulation.
- gnuplot and ImageMagic (convert command), for the visualization.
- An animated gif viewer Here we use Safari.

## Usage

    1. cd src
    2. make  (for simulation)
    3. cd ../slice_grapher
    4. make (for visualization)

## Easy parametes survey

- Change the grid size (NX,NY,NZ) in src/constants.f90
- Change dissipation params in src/params.namelist


## Basic equation

Compressible Navier-Stokes equations for an ideal gas.


## Numerical method

Second-order central difference with 4-th order Runge-Kutta integration.

## Boundary condition

Periodic boundary in all (three) dimensions.

## Programing language

Fortran 2003.

## Author
Akira Kageyama, Kobe Univ., Japan
 email: sgks@mac.com | kage@port.kobe-u.ac.jp

## License

This software is released under the MIT License, see LICENSE.txt.
