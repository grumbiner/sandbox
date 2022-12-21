      PROGRAM cohere
      INTEGER npts
      PARAMETER (npts = 3499)
      REAL amp1(npts), phase1(npts), amp2(npts), phase2(npts) 
      INTEGER i
      REAL freq, per
      REAL radperdeg

      radperdeg = 3.141592654 / 180.

      OPEN (10, FILE="out.circuman", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="dist.circuman", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, npts
        READ (10,*) freq, amp1(i), phase1(i), per
        READ (11,*) freq, amp2(i), phase2(i), per
        phase1(i) = phase1(i) * radperdeg
        phase2(i) = phase2(i) * radperdeg
        PRINT *,freq,
     1             amp1(i)*amp2(i)* (
     1               ( cos(phase1(i))*cos(phase2(i)) + 
     2                   sin(phase1(i))*sin(phase1(i)) )**2
     1             + ( sin(phase1(i))*cos(phase2(i)) - 
     2                   cos(phase1(i))*sin(phase2(i)) )**2 ) /
     1   (amp1(i)*amp2(i)), 
     3            (phase1(i)-phase2(i))/radperdeg, per 
      ENDDO
 
      END
