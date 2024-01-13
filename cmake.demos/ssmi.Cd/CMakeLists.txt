cmake_minimum_required(VERSION 3.19)
project(ssmi
        VERSION 1.0
       )
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Compiler flags for C++
set(CMAKE_CXX_FLAGS "-DLINUX -O2")

# ? Do these after setting up environments --

add_executable(ssmi ssmi.C )

# Libraries to link against:
target_link_libraries(ssmi ombf_4 )

install(TARGETS ssmi DESTINATION exec)
