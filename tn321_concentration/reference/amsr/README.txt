This BUFR Converter Toolkit has been developed by NOAA/NESDIS/STAR Walter Wolf's team. If any person want to use it, please contact Walter Wolf at Walter.Wolf@noaa.gov.

This package includes BUFR encoding programs for AMSR2 1B data. It was developed on IBM AIX machine with xlf90/xlC compiler. It has been test with GNU gfortran/GCC and ifort compilers under Linux-x86-64 environment.  

This file explains how to compile and use this AMSR2 1B BUFR converter Toolkit. 

Package name: 
AMSR2_1B_bufr.tar

What are inside in this package:
1. All the source code in F90 and C in code directory.
2. The running script NPR.pl in run directory.
3. The BUFR table AMSR2_1B_BUFR_Table in run subdirectory.
4. Test data: there are two sets sample data for AMSR2 1B in run subdirectory.
5. Sample BUFR files: There are two samples BUFR files and their corresponding hdf5 files in run subdirectory.
AMSR2-MBT-LR_v1r0_GW1_s201303120008596_e201303120017581_c201312061727150.bufr
AMSR2-MBT-89A_v1r0_GW1_s201303120008596_e201303120017581_c201312061727150.bufr
AMSR2-MBT-89B_v1r0_GW1_s201303120008596_e201303120017581_c201312061727150.bufr
AMSR2-MBT-LR_v1r0_GW1_s201303112359597_e201303120008581_c201312061729410.bufr
AMSR2-MBT-89A_v1r0_GW1_s201303112359597_e201303120008581_c201312061729410.bufr
AMSR2-MBT-89B_v1r0_GW1_s201303112359597_e201303120008581_c201312061729410.bufr
GW1AM2_201303120008_179D_L1DLBTBR_0000000.h5
GW1AM2_201303112359_179D_L1DLBTBR_0000000.h5

How to install the package?
gunzip < AMSR2_1B_bufr.tar.gz | tar xvf -

Required libraries:
1. NCEP BUFR library
2. hdf5 library

How to compile this converter?
1. cd code/main
2. update the HDF5, NetCDF4 and NCEP BUFR library path in Makefile
3. make clean
4. make 

The encoding program main_npr will be generated.

How to run the encoding program?
1. cd run
2. update the data path and program path in NPR.pl.PCF
3. perl ../NPR.pl &
4. the log file NPR.pl.log will be generated. If everything is ok, the BUFR file will be generated.

