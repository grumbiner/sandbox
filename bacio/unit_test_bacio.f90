! This is a test for the Fortran API of the NCEPLIBS-bacio library.
!
! Ed Hartnett 10/5/21
program test_bacio
  use bacio_module
  implicit none

  character(len=14) :: filename = 'test_bacio.bin'
  character (len = 4) :: data
  character (len = 4) :: new_data
  character (len = 4) :: data_in
  character (len = 8) :: data_in_2
  character (len = 12) :: data_in_3
  integer :: lu = 1
  integer :: ka
  integer(kind=8) :: ka8, ib8, nb8
  integer :: stat
  integer :: iret

  ! Delete the test file, if it remains from previous runs.
  open(unit = 1234, iostat = stat, file = filename, status='old')
  if (stat == 0) close(1234, status='delete')  

  print *, 'Testing bacio.'
  
  print *, 'Testing simple write - error messages are expected...'

  ! Try to create a test file - won't work, bad lu.
  call baopen(0, filename, iret)
  if (iret .ne. 6) stop 1
  call baopen(FDDIM + 1, filename, iret)
  if (iret .ne. 6) stop 2

  ! Create a test file.
  call baopen(lu, filename, iret)
  if (iret .ne. 0) stop 3

  ! Try to write some data - won't work, negative size.
  call bawrite(lu, 0, -4, ka, data)
  if (ka .ne. 0) stop 4

  ! Write some data.
  data = 'test'
  call bawrite(lu, 0, 4, ka, data)
  if (ka .ne. 4) stop 5

  ! Try to close the test file - won't work, bad lu.
  call baclose(0, iret)
  if (iret .ne. 6) stop 10
  call baclose(FDDIM + 1, iret)
  if (iret .ne. 6) stop 11

  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 12

  print *, 'Testing reread of simple write - error messages are expected...'

  ! Try to reopen the test file - won't work, bad lu.
  call baopenr(0, filename, iret)
  if (iret .ne. 6) stop 100
  call baopenr(FDDIM + 1, filename, iret)
  if (iret .ne. 6) stop 101

  ! Reopen the test file.
  call baopenr(lu, filename, iret)
  if (iret .ne. 0) stop 102

  ! Try to read some data - won't work, negative size.
  call baread(lu, 0, -4, ka, data_in)
  if (ka .ne. 0) stop 103

  ! Read some data.
  call baread(lu, 0, 4, ka, data_in)
  if (ka .ne. 4) stop 104
  if (data_in .ne. data) stop 105

  ! Reread with l function.
  ib8 = 0
  nb8 = 4
  call bareadl(lu, ib8, nb8, ka8, data_in)
  if (ka8 .ne. 4) stop 110
  if (data_in .ne. data) stop 110

  ! Try to reread with l function - won't work, negative bytes to
  ! skip, if blocked reading option is on.
  call baseto(1, 1)
  call bareadl(lu, -1_8, nb8, ka8, data_in)
  if (ka8 .ne. 0) stop 112
  call baseto(1, 0)

  ! Try to reread with l function - won't work, negative bytes to read.
  call bareadl(lu, ib8, -1_8, ka8, data_in)
  if (ka8 .ne. 0) stop 113

  ! Try to reread with l function - won't work, bad lu.
  call bareadl(0, ib8, nb8, ka8, data_in)
  if (ka8 .ne. 0) stop 32
  call bareadl(FDDIM + 1, ib8, nb8, ka8, data_in)
  if (ka8 .ne. 0) stop 32

  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 114

  print *, 'Testing appending to test file - error messages are expected...'

  ! Try to reopen the test file for writing - won't work, bad lu.
  call baopenw(0, filename, iret)
  if (iret .ne. 6) stop 200
  call baopenw(FDDIM + 1, filename, iret)
  if (iret .ne. 6) stop 201

  ! Reopen the test file for writing.
  call baopenw(lu, filename, iret)
  if (iret .ne. 0) stop 202

  ! Try to append the data to the existing file - won't work, negative nb.
  call bawritel(lu, 4_8, -1_8, ka8, data)
  if (ka8 .ne. 0) stop 203
  
  ! Append the data to the existing file.
  call bawritel(lu, 4_8, 4_8, ka8, data)
  if (ka8 .ne. 4) stop 204
  
  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 205

  ! Reopen the test file.
  call baopenr(lu, filename, iret)
  if (iret .ne. 0) stop 210

  ! Reread with l function.
  ib8 = 0
  nb8 = 8
  call bareadl(lu, ib8, nb8, ka8, data_in_2)
  if (ka8 .ne. 8) stop 211
  if (data_in_2 .ne. 'testtest') stop 212

  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 213

  print *, 'Testing appending again to test file - error messages are expected...'

  ! Try to reopen the test file for writing with append - won't work, bad lu.
  call baopenwa(0, filename, iret)
  if (iret .ne. 6) stop 300
  call baopenwa(FDDIM + 1, filename, iret)
  if (iret .ne. 6) stop 301

  ! Reopen the test file for writing with append.
  call baopenwa(lu, filename, iret)
  if (iret .ne. 0) stop 302

  ! Append the data to the existing file.
  call bawritel(lu, 0_8, 4_8, ka8, data)
  if (ka8 .ne. 4) stop 303
  
  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 304

  ! Reopen the test file.
  call baopenr(lu, filename, iret)
  if (iret .ne. 0) stop 305

  ! Reread with l function.
  ib8 = 0
  nb8 = 12
  call bareadl(lu, ib8, nb8, ka8, data_in_3)
  if (ka8 .ne. 12) stop 306
  if (data_in_3 .ne. 'testtesttest') stop 307

  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 308

  print *, 'Testing truncating to test file - error messages are expected...'

  ! Try to reopen the test file for writing with truncate - won't work, bad lu.
  call baopenwt(0, filename, iret)
  if (iret .ne. 6) stop 400
  call baopenwt(FDDIM + 1, filename, iret)
  if (iret .ne. 6) stop 401

  ! Reopen the test file for writing with truncate.
  call baopenwt(lu, filename, iret)
  if (iret .ne. 0) stop 402

  ! Append the data to the existing file.
  new_data = 'code'
  call bawritel(lu, 0_8, 4_8, ka8, new_data)
  if (ka8 .ne. 4) stop 403
  
  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 404

  ! Reopen the test file.
  call baopenr(lu, filename, iret)
  if (iret .ne. 0) stop 410

  ! Read some data.
  call baread(lu, 0, 4, ka, data_in)
  if (ka .ne. 4) stop 411
  if (data_in .ne. new_data) stop 412

  ! Close the test file.
  call baclose(lu, iret)
  if (iret .ne. 0) stop 413

  print *, 'SUCCESS!'
end program test_bacio
