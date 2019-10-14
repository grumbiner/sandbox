      PROGRAM formin
C     Program to take data in the Ross Sea data format and transfer it to 
C       the form used in the NOS tidal analysis program.
C     Bob Grumbine 1-5-87.

      REAL x(10000)
      INTEGER i, j, n
      INTEGER m, ma, mb
      CHARACTER*4 name
      INTEGER y(10000)

C     OPEN (9, FILE='dummydata', FORM='FORMATTED', STATUS='OLD')
      OPEN (8, FILE='fakedata', FORM='UNFORMATTED', STATUS='NEW')
     
      PRINT *, 'How many data points do you have?'
      READ (*,9001) n
 9001 FORMAT (I5)

C     DO 1000 i = 1, n/12
C       READ (9,9002) m, name, ma, mb, (x(12*(i-1)+j), j = 1, 12)
C1000 CONTINUE
C     
C     WRITE (8) x
C
C     DO 2000 i = 1, n/12
C       WRITE (*, 9002) m, name, ma, mb, (y(12*(i-1)+j), j = 1, 12)
C2000 CONTINUE

      CALL readin (x, n)

      DO 1000 i = 1, n-12, 12
        WRITE (8,9004) 123,'Ross',82,1234,(INT(100.*x(j)),j=i,i+11)
 1000 CONTINUE

 9002 FORMAT (12X, I3, A4, I2, I4, 7X, 12F4.2)
 9003 FORMAT (I3, A4, I2, I4, 12F5.1)
 9004 FORMAT (12X, I3, A4, I2, I4, 7X, 12I4) 
      CLOSE (8,STATUS='KEEP')

      END
