cmake_minimum_required(VERSION 3.5)
project (ADS)
enable_language (Fortran)

set(EXECUTABLE_OUTPUT_PATH ../execs)
# Default to Release build
if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "No build type selected, default to Release")
  set(CMAKE_BUILD_TYPE "Release")
#  set(CMAKE_BUILD_TYPE "Debug")
endif()


# make sure that the default is a RELEASE
#if (NOT CMAKE_BUILD_TYPE)
#  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
#      "Choose the type of build, options are: None Debug Release."
#      FORCE)
#endif (NOT CMAKE_BUILD_TYPE)


# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none -fpp -Wall -pedantic")
    set(bounds "-fbounds-check")
    set(realeaseopts "-O3  -pthread -funroll-all-loops")
    set(debugopts "-O0 -g3 -fbacktrace -fcheck=all -fdump-fortran-optimized")
#    -DIINFO -DIDEBUG -DIPRINT")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-std08  -module . -implicitnone -fpp -Wall -pedantic")
    set(bounds " -check bounds")
    set(realeaseopts "-funroll-all-loops -O3")
    set(debugopts "-O0 -g -traceback -check all -debug extended -debug-parameters all")
#    -DIINFO -DIDEBUG -DIPRINT")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${debugopts}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_RELEASE_FLAGS} ${realeaseopts}")

#set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DPERFORMANCE=1")
#set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -DPERFORMANCE=1")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")

add_executable(test1 test.f90 gauss_utils.f90 heat_transport_test.f90)
