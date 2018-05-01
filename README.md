# fortran_proj_1

autor: Albert Pawula

Required:
 - cmake (minimum 3.5)
 - fortran compiler (gfortran (linux) or ifort (windnows))
 - pdf reader to view the line charts

The cmake configuration via github user "macwozni"

To bulid the executable file, in console,:
- enter the ./build folder and type "cmake .." [in order to create makefile]
- "make"
- go to ./execs and "./test1 4"  the argument is the precision


KNOWN ISSUES (TODO):
The precision needs to be configured inside the source code as parameter.
Passed argument can be read inside the program, but there is an issue with passing the variable as parameter
to the subroutine
