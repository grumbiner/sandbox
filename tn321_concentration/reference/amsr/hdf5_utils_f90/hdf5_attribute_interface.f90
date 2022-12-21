!=============================================================================
!
! NAME:
!    hdf5_attribute_interface.f90
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
!    USE hdf5_attribute_interface
!
! INPUTS:
!
!    file_id       -- the HDF5 file ID
!    dataset_name  -- the dataset fullname with path(reader)
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
!    Yi Song, IMSG, 10/08/13
!    Yi.Song@noaa.gov
!
!
!    $Date:$
!    $Id:$
!    $Log:$
!
!-----------------------------------------------------------------------------
!

MODULE hdf5_attribute_interface

   INTEGER, PARAMETER :: MAX_HDF_DIMS = 7

   INTERFACE read_hdf5_attribute
      MODULE PROCEDURE read_hdf5_attribute
   END INTERFACE read_hdf5_attribute

   CONTAINS

   ! Read FLOAT data
   ! F90 SINGLE real(4)
   ! NetCDF FLOAT/REAL
   ! HDF5 H5T_IEEE_F32BE H5T_NATIVE_REAL

   SUBROUTINE read_hdf5_attribute(file_id, dataset_name, attribute_name, attr_value)

      USE HDF5
      USE errormsg_module
      USE type_kinds

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: dataset_name
      CHARACTER(*), INTENT(IN) :: attribute_name
      INTEGER(HID_T), INTENT(IN) :: file_id
      REAL(SINGLE), INTENT(OUT) :: attr_value

      INTEGER(HID_T) :: dataset_id
      INTEGER(HID_T) :: attribute_id
      INTEGER(HID_T) :: type_id
      INTEGER(HID_T) :: space_id
      INTEGER(HSIZE_T), DIMENSION(MAX_HDF_DIMS) :: dims
      INTEGER(HSIZE_T), DIMENSION(MAX_HDF_DIMS) :: maxdims
      INTEGER :: error

      ! Open an existing dataset.
      CALL h5dopen_f(file_id, dataset_name, dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_attribute_interface read_hdf5_attribute', &
         'Error OPEN dataset: ' // dataset_name, FATAL)
      ENDIF

!     Open the attribute
      CALL h5aopen_name_f(dataset_id, attribute_name, attribute_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_attribute_interface read_hdf5_attribute', &
         'Error OPEN attribute: ' // attribute_name, FATAL)
      ENDIF

!     Get the Attribute DataType id (need for reading the attribute info.)

      CALL h5aget_type_f(attribute_id, type_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_attribute_interface read_hdf5_attribute', &
         'Error getting attribute type: ' // attribute_name, FATAL)
      ENDIF

!     Get the Attribute DataSpace id (needed for getting the dimensions of the
!     attribute)

      CALL h5aget_space_f(attribute_id, space_id, error)
      IF ( error /= 0 ) THEN
         CALL error_messaging('hdf5_read_attribute_defines', &
            'Error getting attribute space', FATAL)
      ENDIF

!     Get the Attribute Dims
      CALL h5sget_simple_extent_dims_f(space_id, dims, maxdims, error)

!     Get the attribute value.

      CALL h5aread_f(attribute_id, type_id, attr_value, dims(1:1), error)
      IF ( error /= 0 ) THEN
         CALL error_messaging('hdf5_read_attribute_defines', &
            'Error Reading Attriubte: ' // attribute_name, FATAL)
      ENDIF

!     Close the attribute

      CALL h5aclose_f(attribute_id, error)
      IF ( error /= 0 ) THEN
         CALL error_messaging('hdf5_read_attribute_defines', &
            'Error Closing Attriubte: ' // attribute_name, FATAL)
      ENDIF

      ! Close the dataset.
      CALL h5dclose_f(dataset_id, error)
      IF(error == -1)THEN
         CALL error_messaging('hdf5_attribute_interface read_hdf5_attribute', &
         'Error CLOSE dataset: ' // dataset_name, FATAL)
      ENDIF

   END SUBROUTINE read_hdf5_attribute

END MODULE hdf5_attribute_interface



