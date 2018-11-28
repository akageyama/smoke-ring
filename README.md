# class-hpc-smoke-ring

A simple sample field solver, or a CFD (Computational Fluid Dynamics)
code for the class "HPC", which is for undergraduate students of 
Department of Engineering, 
Kobe University, Japan.

## Physical Model
A gas contained in a rectangular box is driven by a localized force
near the end of box. The force drives the gas to flow toward
the other end, rsulting in the formation of the well-known vortex-ring,
or smoke-ring.


## Prerequisite
- Fortran compiler
- gnuplot, for the post-process (2-D) visualization.
- ImageMagic (convert command), for the post-process (2-D) visualization.

## Usage

    cd src
    kvsmake -g smoke-ring
    kvsmake
    ./smoke-ring < sample.namelist
    cd ../slice_grapher
    make
    make gif  # ImageMagic (convert command) is used.
    make view  # Here we use Safari browser for gif animation.

## Parameters

- Change (NX,NY,NZ) in src/constants.f90 and slice_grapher/constants.f90
- Change values in src/sample.namelist


## Basic equation

Compressible Navier-Stokes equations for an ideal gas.

## Numerical method

Second-order central difference with 4-th order Runge-Kutta integration.

## Boundary condition

Periodic boundary condition in all (three) dimensions.

## Programing language

Fortran95.

## Author
Akira Kageyama, Kobe Univ., Japan
 email: sgks@mac.com, kage@port.kobe-u.ac.jp

## License
This software is released under the MIT License, see LICENSE.txt.
