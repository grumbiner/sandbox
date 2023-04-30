      PROGRAM scaniers
      IMPLICIT none

      INTEGER dtg(4)
      REAL obs(12)

      INTEGER i, j, k

      DO i = 1, 17595
        READ(*,9001) (dtg(j),j=1,4), (obs(k),k=1,12)
        WRITE (11,*) obs(1) 
        WRITE (12,*) obs(2) 
        WRITE (13,*) obs(4) 
        WRITE (14,*) sqrt(obs(1)*obs(1) + obs(2)*obs(2))
      ENDDO
 9001 FORMAT(3(I4),I7,2(F11.6),2(F12.7),2(F11.6),2(F11.6),2(F11.7),
     1           2F12.6)

      END
