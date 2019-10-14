C This program takes 3 files with lists of numbers and puts
C the all of ths numbers into one file with 7 columns

C Subprograms needed: subr1.f subr2.f subr3.f 

      PROGRAM AB test
      IMPLICIT NONE
 
      REAL a(7),b(7),c(7),d(7),e(7),f(7),g(7)
      INTEGER n,i


      CALL GET1 (a,b,c, n)
      CALL GET2 (d,e, n)
      CALL GET3 (f,g, n)

      OPEN ( 15, FILE = "sd.9703")

      DO 2000 i = 1,n
      WRITE (15,9001) a(i),b(i),c(i),d(i),e(i),f(i),g(i)
 2000 CONTINUE   

 9001 FORMAT ('  ',7F8.4)     

      END
