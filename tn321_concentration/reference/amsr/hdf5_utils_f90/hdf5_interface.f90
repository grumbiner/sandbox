!=============================================================================
!
! (C) QSS Group
!
!
! NAME:
!    hdf5_interface.f90
!
! DESCRIPTION:
!    Subroutines to read/write hdf5 data file
!
! FUNCTION:
!    HDF5 utilities (reader, writer, dimensions, and ...)
!
! REFERENCE:
!    None
!
! CALLING SEQUENCE:
!    USE hdf5_interface
!    CALL read_hdf5(file_id, dataset_name, data, dims)
!    CALL write_hdf5(file_id, dataset_name, data, dims)
!    CALL get_hdf5_dimensions(file_name, &
!         dataset_name, which_dimension, dimension_size)
!
! INPUTS:
!
!    file_id       -- the HDF5 file ID
!    dataset_name  -- the dataset fullname with path(reader)
!    data(writer)  -- the data array to be written into HDF5 file
!    dims          -- the dimensions of the data (seems useless for reader)
!    file_name     -- the HDF5 filename
!
! OUTPUTS:
!
!    data(reader)  -- the data array holding the input HDF5 data
!
! CALLS:
!    h5dopen_f
!    h5dread_f   (reader)
!    h5dwrite_f  (writer)
!    h5dclose_f
!
!    h5open_f
!    h5fopen_f
!    h5dget_space_f
!    h5sget_simple_extent_dims_f
!    h5sclose_f
!    h5fclose_f
!    h5close_f
!
! DEPENDENCIES:
!    HDF5
!    errormsg_module
!    type_kinds
!
! RESTRICTIONS:
!    None known
!
! SIDE EFFECTS:
!    None known
!
! ERROR CODES/EXCEPTIONS:
!    error_messaging
!
! MODIFICATION HISTORY:
!
!    Chen Zhang, PSGS, 09/08/08
!    Chen.Zhang@noaa.gov
!
!
!    $Date:$
!    $Id:$
!    $Log:$
!
!-----------------------------------------------------------------------------
!

MODULE hdf5_interface

   INTEGER, PARAMETER :: MAX_HDF_DIMS = 7

   INTERFACE read_hdf5
      MODULE PROCEDURE read_hdf5_short
      MODULE PROCEDURE read_hdf5_long
      MODULE PROCEDURE read_hdf5_dlong
      MODULE PROCEDURE read_hdf5_float
      MODULE PROCEDURE read_hdf5_double
   END INTERFACE read_hdf5

   INTERFACE write_hdf5
      MODULE PROCEDURE write_hdf5_short
      MODULE PROCEDURE write_hdf5_long
      MODULE PROCEDURE write_hdf5_dlong
      MODULE PROCEDURE write_hdf5_float
      MODULE PROCEDURE write_hdf5_double
   END INTERFACE write_hdf5


   CONTAINS

   SUBROUTINE get_hdf5_dimensions(file_name, dataset_name, &
      which_dimension, dimension_size)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: file_name
      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(LONG), INTENT(IN) :: which_dimension
      INTEGER(LONG), INTENT(OUT) :: dimension_size

      INTEGER(HSIZE_T), DIMENSION(MAX_HDF_DIMS) :: dims 
      INTEGER(HSIZE_T), DIMENSION(MAX_HDF_DIMS) :: maxdims
      INTEGER(HID_T) :: file_id
      INTEGER(HID_T) :: dataset_id
      INTEGER(HID_T) :: dataspace_id
      INTEGER :: error, i, ndims

      ! Initialize HDF5 FORTRAN interface.
      CALL h5open_f(error)
      IF(error == -1)THEN
         CALL error_messaging('read_hdf5_interface', &
         'Error Initialize FORTRAN INTERFACE', FATAL)
      ENDIF

      ! Open an existing HDF5 data file.
      ! H5F_ACC_RDWR_F -- Read&Write, H5F_ACC_RDONLY_F -- ReadOnly
      CALL h5fopen_f (file_name, H5F_ACC_RDONLY_F, file_id, error)
      IF(error == -1)THEN
         CALL error_messaging('read_hdf5_interface', &
         'Error OPEN ' // file_name, FATAL)
      ENDIF

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      CALL h5dget_space_f(dataset_id, dataspace_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error get dataspace: ' // dataset_name, FATAL)
      ENDIF

      CALL h5sget_simple_extent_ndims_f(dataspace_id, ndims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error get_dims dataset: ' // dataset_name, FATAL)
      ENDIF

      CALL h5sget_simple_extent_dims_f(dataspace_id, dims, maxdims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error get_dims dataset: ' // dataset_name, FATAL)
      ENDIF
!    do i = 1, ndims
!      write(*,*) i, dims(i), maxdims(i)
!    enddo

      dimension_size = dims(which_dimension)

      ! Close the dataspace.
      CALL h5sclose_f(dataspace_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error CLOSE dataspace: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface get_hdf5_dimensions', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the HDF5 data file.
      CALL h5fclose_f(file_id, error)
      IF(error == -1)THEN
         CALL error_messaging('read_hdf5_interface', &
         'Error CLOSE ' // file_name, FATAL)
      ENDIF

      ! Close HDF5 FORTRAN interface.
      CALL h5close_f(error)
      IF(error == -1)THEN
         CALL error_messaging('read_hdf5_interface', &
         'Error CLOSE FORTRAN INTERFACE', FATAL)
      ENDIF

   END SUBROUTINE get_hdf5_dimensions

   ! Read SHORT data
   ! F90 SHORT integer(2)
   ! NetCDF SHORT
   ! HDF5 H5T_STD_I16BE
   SUBROUTINE read_hdf5_short(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(SHORT), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(LONG), DIMENSION(:), POINTER :: dataTemp
      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_short', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Read the dataset
      ! There are problems to read "SHORT" data directly
      ! Need to read the data as "LONG"

      ALLOCATE(dataTemp(SIZE(DATA)), stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_short', &
         'Error ALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF
      ! CALL h5dread_f(dataset_id, H5T_STD_I32BE, dataTemp, dims, error)
      CALL h5dread_f(dataset_id, H5T_NATIVE_INTEGER, dataTemp, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_short', &
         'Error READ dataset: ' // dataset_name, FATAL)
      ENDIF

      DATA = dataTemp
      DEALLOCATE(dataTemp, stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_short', &
         'Error DEALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)

   END SUBROUTINE read_hdf5_short


   ! Read LONG data
   ! F90 LONG integer(4)
   ! NetCDF LONG/INT
   ! HDF5 H5T_STD_I32BE H5T_NATIVE_INTEGER
   SUBROUTINE read_hdf5_long(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(LONG), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, trim(dataset_name), dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Read the dataset
      ! CALL h5dread_f(dataset_id, H5T_STD_I32BE, data, dims, error)
print *, "start1"
      CALL h5dread_f(dataset_id, H5T_NATIVE_INTEGER, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error READ dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE read_hdf5_long


   ! Read DLONG data
   ! F90 DLONG integer(8)
   ! NetCDF INT64
   ! HDF5 H5T_STD_I64BE
   SUBROUTINE read_hdf5_dlong(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(DLONG), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      REAL(DOUBLE), DIMENSION(:), POINTER :: dataTemp
      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_dlong', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Read the dataset
      ! There are problems to read "DLONG" data directly
      ! Need to read the data as "DOUBLE"

      ALLOCATE(dataTemp(SIZE(DATA)), stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error ALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      ! CALL h5dread_f(dataset_id, H5T_STD_I64BE, data, dims, error)
      ! CALL h5dread_f(dataset_id, H5T_IEEE_F64BE, dataTemp, dims, error)
      CALL h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, dataTemp, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_dlong', &
         'Error READ dataset: ' // dataset_name, FATAL)
      ENDIF

      DATA = dataTemp
      DEALLOCATE(dataTemp, stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error DEALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_long', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE read_hdf5_dlong


   ! Read FLOAT data
   ! F90 SINGLE real(4)
   ! NetCDF FLOAT/REAL
   ! HDF5 H5T_IEEE_F32BE H5T_NATIVE_REAL
   SUBROUTINE read_hdf5_float(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      REAL(SINGLE), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_float', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Read the dataset
      ! CALL h5dread_f(dataset_id, H5T_IEEE_F32BE, data, dims, error)
      CALL h5dread_f(dataset_id, H5T_NATIVE_REAL, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_float', &
         'Error READ dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_float', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE read_hdf5_float


   ! Read DOUBLE data
   ! F90 DOUBLE real(8)
   ! NetCDF DOUBLE
   ! HDF5 H5T_IEEE_F64BE H5T_NATIVE_DOUBLE
   SUBROUTINE read_hdf5_double(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      REAL(DOUBLE), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_double', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Read the dataset
      ! CALL h5dread_f(dataset_id, H5T_IEEE_F64BE, data, dims, error)
      CALL h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_double', &
         'Error READ dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface read_hdf5_double', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE read_hdf5_double

   ! Write SHORT data
   ! F90 SHORT integer(2)
   ! NetCDF SHORT
   ! HDF5 H5T_STD_I16BE
   SUBROUTINE write_hdf5_short(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(SHORT), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(LONG), DIMENSION(:), POINTER :: dataTemp
      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_short', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Write the dataset
      ! There are problems to write "SHORT" data directly
      ! Need to write the data as "LONG"

      ALLOCATE(dataTemp(SIZE(DATA)), stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_short', &
         'Error ALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      dataTemp = DATA

      ! CALL h5dwrite_f(dataset_id, H5T_STD_I32BE, dataTemp, dims, error)
      CALL h5dwrite_f(dataset_id, H5T_NATIVE_INTEGER, dataTemp, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_short', &
         'Error WRITE dataset: ' // dataset_name, FATAL)
      ENDIF

      DEALLOCATE(dataTemp, stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_short', &
         'Error DEALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)

   END SUBROUTINE write_hdf5_short


   ! Write LONG data
   ! F90 LONG integer(4)
   ! NetCDF LONG/INT
   ! HDF5 H5T_STD_I32BE H5T_NATIVE_INTEGER
   SUBROUTINE write_hdf5_long(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(LONG), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Write the dataset
      ! CALL h5dwrite_f(dataset_id, H5T_STD_I32BE, data, dims, error)
      CALL h5dwrite_f(dataset_id, H5T_NATIVE_INTEGER, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error WRITE dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE write_hdf5_long


   ! Write DLONG data
   ! F90 DLONG integer(8)
   ! NetCDF INT64
   ! HDF5 H5T_STD_I64BE
   SUBROUTINE write_hdf5_dlong(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      INTEGER(DLONG), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      REAL(DOUBLE), DIMENSION(:), POINTER :: dataTemp
      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_dlong', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Write the dataset
      ! There are problems to write "DLONG" data directly
      ! Need to write the data as "DOUBLE"

      ALLOCATE(dataTemp(SIZE(DATA)), stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error ALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      dataTemp = DATA

      ! CALL h5dwrite_f(dataset_id, H5T_STD_I64BE, data, dims, error)
      ! CALL h5dwrite_f(dataset_id, H5T_IEEE_F64BE, dataTemp, dims, error)
      CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, dataTemp, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_dlong', &
         'Error WRITE dataset: ' // dataset_name, FATAL)
      ENDIF

      DEALLOCATE(dataTemp, stat=error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error DEALLOCATE dataTemp ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_long', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE write_hdf5_dlong


   ! Write FLOAT data
   ! F90 SINGLE real(4)
   ! NetCDF FLOAT/REAL
   ! HDF5 H5T_IEEE_F32BE H5T_NATIVE_REAL
   SUBROUTINE write_hdf5_float(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      REAL(SINGLE), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_float', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Write the dataset
      ! CALL h5dwrite_f(dataset_id, H5T_IEEE_F32BE, data, dims, error)
      CALL h5dwrite_f(dataset_id, H5T_NATIVE_REAL, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_float', &
         'Error WRITE dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_float', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE write_hdf5_float


   ! Write DOUBLE data
   ! F90 DOUBLE real(8)
   ! NetCDF DOUBLE
   ! HDF5 H5T_IEEE_F64BE H5T_NATIVE_DOUBLE
   SUBROUTINE write_hdf5_double(file_id, dataset_name, DATA, dims)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      REAL(DOUBLE), DIMENSION(:), POINTER :: DATA
      INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims

      INTEGER(HID_T) :: dataset_id
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_double', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Write the dataset
      ! CALL h5dwrite_f(dataset_id, H5T_IEEE_F64BE, data, dims, error)
      CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, DATA, dims, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_double', &
         'Error WRITE dataset: ' // dataset_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_interface write_hdf5_double', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE write_hdf5_double



END MODULE hdf5_interface



