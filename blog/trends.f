      PROGRAM trends
C     compute forward, centered, backward trends on monthly data
C     Robert Grumbine 21 Dec 2008

      IMPLICIT none
      INTEGER npts
      PARAMETER (npts = 1543) !monthly NCDC data
      REAL anomalies(npts)
      INTEGER i, length, start, max_span
      REAL mean_b, slope_b, mean_c, slope_c, mean_f, slope_f

      DO i = 1, npts
        READ (*,*) anomalies(i)
!        PRINT *,i,anomalies(i)
      ENDDO

      max_span = 31*12 !longest period a trend will be computed over
      DO length = 36, max_span, 24 
        !backwards
        DO start = max_span + 1, npts - max_span, 12
          CALL fit(anomalies, npts, start,          length, 
     1                      mean_b, slope_b)
          CALL fit(anomalies, npts, start-length/2, length, 
     1                      mean_c, slope_c)
          CALL fit(anomalies, npts, start-length  , length, 
     1                      mean_f, slope_f)
          WRITE (*,9001) length, start, mean_b, mean_c, mean_f, 
     1            start,   slope_b*1200, slope_c*1200, slope_f*1200
 9001     FORMAT (I5,",",I5,",", 3(F8.4,","), I5,",",3(F9.4,",") )
        ENDDO
      ENDDO

      length = 25*12
      DO start = length/2+1, npts - length/2 
        CALL fit(anomalies, npts, start-length/2, length, 
     1                  mean_c, slope_c)
        WRITE (*,9002) start, mean_c, start, slope_c*120
      ENDDO 
 9002 FORMAT("center,",I5,",",F8.4,",",I5,",",F9.4)

      END 
      SUBROUTINE fit(anomalies, npts, start, length, mean, slope)
      IMPLICIT none
      INTEGER npts, start, length
      REAL anomalies(npts), mean, slope
      DOUBLE PRECISION delta, sy, sxy, sx, sx2
      INTEGER i

      sx = 0
      sy = 0
      sxy = 0
      sx2 = 0
      DO i = start, start+length-1
        sy = sy + anomalies(i)
        sxy = sxy + anomalies(i)*(i-start)
        sx  = sx + (i-start)
        sx2 = sx2 + (i-start)*(i-start)
      ENDDO

      slope = (length*sxy - sx*sy) / (length*sx2 - sx*sx)
      mean = (sy - slope*sx)/length

      RETURN
      END
