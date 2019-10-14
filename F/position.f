      PROGRAM a
      IMPLICIT none

      INTEGER fseek, lunit
! For small (< 2 Gb) files, this is sufficient:
!      INTEGER ftell, n
!      INTEGER from, offset
! For large files, use this:
      INTEGER*8 ftelli8, n
      INTEGER*8 from, offset

      offset = 2**40 ! number of bytes from 'from'
      from   = 2 ! 0 = beginning of file, 2 = end of file, 1 = current position
    
      lunit = 11
      OPEN(UNIT=lunit, FILE="test")

      PRINT *,'seeking to byte 2^40'
      n = fseek(lunit, offset, from)
      PRINT *,'n = ',n

      n = ftelli8(lunit)
      PRINT *,'n = ',n
  
      END
