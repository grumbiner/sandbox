cmake_minimum_required(VERSION 3.19)
project(Tutorial
        VERSION 1.0
       )
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Compiler flags for C++
set(CMAKE_CXX_FLAGS "-DLINUX -O2")

#------------------------------------
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  #set(CMAKE_INSTALL_PREFIX "/usr/local")
  set(CMAKE_INSTALL_PREFIX "/Users/rmg3/sandbox/cmake.demos")
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  #set(CMAKE_INSTALL_PREFIX "/my/default" CACHE PATH "..." FORCE)
  set(CMAKE_INSTALL_PREFIX "/Users/rmg3/sandbox/cmake.demos")
endif()

set(CMAKE_INSTALL_PREFIX "/Users/rmg3/sandbox/cmake.demos")
#------------------------------------

# Using a remote include directory -- specifying a path to check for include files
include_directories( mmablib/include )


# ? Do these after setting up environments --

add_subdirectory(mmablib)

add_executable(Tutorial tutorial.c++ )

# Importing configuration parameters
configure_file(TutorialConfig.h.in TutorialConfig.h)

target_include_directories(Tutorial PUBLIC ".")

add_subdirectory(ssmi.Cd)

# Libraries to link against:
target_link_libraries(Tutorial ombf_4 )

install(TARGETS Tutorial DESTINATION exec)
