# smoke-ring

A simple sample CFD (Computational Fluid Dynamics) code.

## Physical Model
A gas contained in a rectangular box is driven by a localized force
near a side plane of the box. The force drives the gas fluid toward
the other side, rsulting in the formation of well-known vortex-ring,
or smoke-ring.

## Program structure

    --+--src (For the smoke-ring simulation. The output is a file
      |       named "_data_slice".)
      |
      +--data/vis2d (Cross section visualization in SVG format)
      |
      +--warming_up (A supplementary, simple, sample, and stand-alone,
                     simulation program. It solves 1-D Burgers
                     equation with the same numerical scheme (finite
                     difference + Runge-Kutta integration) and with
                     the same visualization method (gnuplot). Read
                     and run this program before you proceed to the
                     main smoke-ring ring codes (src and slice_grapher).


## Usage

    1. cd src
    2. make  (for simulation)

## Easy experiments

- Change the grid size (NX,NY,NZ) in src/constants.ef
- Change dissipation params in src/params.namelist


## Basic equation

Compressible Navier-Stokes equations for an ideal gas.


## Numerical method

Second-order central difference with 4-th order Runge-Kutta integration.

## Boundary condition

Periodic boundary in all (three) dimensions.

## Programing language

eFortran.

## Author
Akira Kageyama, Kobe Univ., Japan
 email: sgks@mac.com | kage@port.kobe-u.ac.jp

## License

This software is released under the MIT License, see LICENSE.txt.
