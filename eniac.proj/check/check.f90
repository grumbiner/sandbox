PROGRAM check
!  check results between different runs/implementations
!  26 March 2019
  IMPLICIT none

!  Non-dimensional constants
  INTEGER p, q, n, ip, jp
  PARAMETER (p = 18)
  PARAMETER (q = 15)
  PARAMETER (n = 24)
  PARAMETER (ip = 9)
  PARAMETER (jp = 13)

!  Declare data, physical info.
  REAL z1(0:p,0:q), z2(0:p,0:q)
  REAL delta(0:p,0:q)
  CHARACTER a(4), b
  
!  Declare computational variables
  INTEGER i, j, k, tk
  
! BEGIN THE EXECUTION HERE
  OPEN (UNIT=1, FILE="ENIAC.REF", FORM="FORMATTED", STATUS="OLD")
  OPEN (UNIT=2, FILE="ENIAC.OUT", FORM="FORMATTED", STATUS="OLD")
  
!***********************************************************!!
!  BEGIN THE ITERATIVE SOLUTION OF THE EQUATIONS
  DO k = 0, 3*n-1

    READ (1,9003) a, tk
    READ (1,9001) ((z1(i,j),j=0,q),i=0,p)
    READ (1,9002) b

    READ (2,9003) a, tk
    READ (2,9001) ((z2(i,j),j=0,q),i=0,p)
    READ (2,9002) b

    delta = z1 - z2
    WRITE (*,9004) k, tk, MAXVAL(delta), MINVAL(delta)
    WRITE (*,9001) ((delta(i,j),j=0,q),i=0,p)
    WRITE (*,9002) b
  
  ENDDO
    
 9001 FORMAT (16F7.2)
 9002 FORMAT (A1)
 9003 FORMAT (4AI3)
 9004 FORMAT ('k = ',2I3, 2F9.2)
 
!***********************************************************!! 

  END
