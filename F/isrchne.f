      FUNCTION ISRCHNE(N,X,INCX,TARGET)
      INTEGER X(*), TARGET
      J=1
      ISRCHNE=0
      IF(N.LE.0) RETURN
      IF(INCX.LT.0) J=1-(N-1)*INCX
      DO I=1,N
        IF(X(J).NE.TARGET) THEN
          ISRCHNE=I
          RETURN
        ENDIF
        J=J+INCX
      ENDDO
      RETURN
      END
