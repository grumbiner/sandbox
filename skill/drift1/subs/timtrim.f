      SUBROUTINE timtrim(delh, iunit, ounit)
C     Trim buoy files so that only have 1 'ob' per forecast point/step.
C     Trim by averaging on to the time of obs.
C     Bob Grumbine 21 April 1994.
C                  30 March 1995.

      IMPLICIT none


      INTEGER delh, iunit, ounit
!      INCLUDE "buoy.inc"

      INTEGER nmax
      PARAMETER (nmax = 2000)
      CHARACTER*7 name(nmax)
      REAL rlat(nmax), rlong(nmax), along, long2

      INTEGER rdp(nmax), rp(nmax), rdt(nmax), rt(nmax)
      INTEGER rddir(nmax), rdir(nmax), rdsp(nmax), rsp(nmax)
      INTEGER rdsst(nmax), rsst(nmax)
      INTEGER rdate(nmax)
      INTEGER datedh, tdate, tdh

      INTEGER ntot, nrun, i
      INTEGER iret, delay1

 
      i = 1
      nrun = 1
  100 CONTINUE
        CALL getboy( name(i), code, rlat(i), rlong(i), rdate(i), 
     1    rdp(i), rp(i), rdt(i), rt(i), rddir(i), rdir(i), rdsp(i),
     2    rsp(i), rdsst(i), rsst(i),
     3    iunit, iret)
      IF (iret .EQ. RERROR) GO TO 100
      IF (iret .EQ. ENDDAT) GO TO 300
      tdate = rdate(i)
      tdh = 0

 1000 CONTINUE
        i = i + 1
  200 CONTINUE
        CALL getboy( name(i), code, rlat(i), rlong(i), rdate(i), 
     1    rdp(i), rp(i), rdt(i), rt(i), rddir(i), rdir(i), rdsp(i),
     2    rsp(i), rdsst(i), rsst(i),
     3    iunit, iret)
        IF (iret .EQ. RERROR) GO TO 200
        IF (iret .EQ. ENDDAT) GO TO 300

        IF (name(i) .NE. name(i-1)) THEN
          tdh = INT ( 0.5 + FLOAT(tdh)/FLOAT(nrun) )
          tdate = datedh(tdate, tdh)
          CALL putboy( name(i-1), code, rlat(i-1), rlong(i-1), 
     1                 tdate, rdp(i-1), rp(i-1), rdt(i-1), 
     2                 rt(i-1), rddir(i-1), rdir(i-1), rdsp(i-1), 
     3                 rsp(i-1), rdsst(i-1), rsst(i-1), ounit, iret)

          name(1) = name(i)
          rlat(1) = rlat(i)
          rlong(1) = rlong(i)
          rdate(1) = rdate(i)
          rdp(1)   = rdp(i)
          rp(1)    = rp(i)
          rdt(1)   = rdt(i)
          rt(1)    = rt(i)
          rddir(1) = rddir(i)
          rdir(1)  = rdir(i)
          rdsp(1)  = rdsp(i)
          rsp(1)   = rsp(i)
          rdsst(1) = rdsst(i)
          rsst(1)  = rsst(i)
          i = 1
          nrun = 1
          tdate    = rdate(1)
          tdh      = 0
CD          PRINT *,'Initialized date ',tdate


         ELSE IF ( ABS(delay1(rdate(i), rdate(i-1) ) ) .LE. delh) THEN
          nrun = nrun + 1
          along = rlong(i-1)
          long2 = rlong(i)
          IF (ABS(along) .GT. 180.) along = along - SIGN(360., along)
          IF (ABS(long2) .GT. 180.) long2 = long2 - SIGN(360., long2)
          IF ( ABS(along - long2) .GT. 180 ) THEN
            IF (along .GT. 0.) THEN
              long2 = long2 + 360.
             ELSE
              long2 = long2 - 360.
            ENDIF
          ENDIF
          rlong(i) = (along*(nrun-1)+long2 )/FLOAT(nrun)
CD          rlong(i) = (rlong(i-1)*(nrun-1)+rlong(i))/FLOAT(nrun)

          tdh = tdh + delay1(rdate(i), tdate)
CD          PRINT *,'tdh = ',tdh

          rp(i)    = (rp(i-1)*(nrun-1)+rp(i))/FLOAT(nrun)

C         Things which can be averaged straightforwardly
          rlat(i)  = (rlat(i-1)*(nrun-1)+rlat(i))/FLOAT(nrun)
          rdp(i)   = (rdp(i-1)*(nrun-1)+ rdp(i))/FLOAT(nrun)
          rdt(i)   = (rdt(i-1)*(nrun-1)+ rdt(i))/FLOAT(nrun)
          rt(i)    = (rt(i-1)*(nrun-1)+ rt(i))/FLOAT(nrun)
          rddir(i) = (rddir(i-1)*(nrun-1)+ rddir(i))/FLOAT(nrun)
          rdir(i)  = (rdir(i-1)*(nrun-1)+ rdir(i))/FLOAT(nrun)
          rdsp(i)  = (rdsp(i-1)*(nrun-1)+ rdsp(i))/FLOAT(nrun)
          rsp(i)   = (rsp(i-1)*(nrun-1) + rsp(i)) /FLOAT(nrun) 
          rdsst(i) = (rdsst(i-1)*(nrun-1) + rdsst(i)) / FLOAT(nrun)
          rsst(i)  = (rsst(i-1)*(nrun-1) + rsst(i)) /FLOAT(nrun) 

         ELSE
          tdh = INT ( 0.5 + FLOAT(tdh)/FLOAT(nrun) )
          tdate = datedh(tdate, tdh)

          CALL putboy( name(i-1), code, rlat(i-1), rlong(i-1), 
     1                 tdate, rdp(i-1), rp(i-1), rdt(i-1), 
     2                 rt(i-1), rddir(i-1), rdir(i-1), rdsp(i-1), 
     3                 rsp(i-1), rdsst(i-1), rsst(i-1), ounit, iret)
          name(1) = name(i)
          rlat(1) = rlat(i)
          rlong(1) = rlong(i)
          rdate(1) = rdate(i)
          rdp(1)   = rdp(i)
          rp(1)    = rp(i)
          rdt(1)   = rdt(i)
          rt(1)    = rt(i)
          rddir(1) = rddir(i)
          rdir(1)  = rdir(i)
          rdsp(1)  = rdsp(i)
          rsp(1)   = rsp(i)
          rdsst(1) = rdsst(i)
          rsst(1)  = rsst(i)
          nrun = 1
          i = 1
          tdate    = rdate(1)
          tdh      = 0
CD          PRINT *,'Initialized date ',tdate

        ENDIF

        GO TO 1000


  300 CONTINUE

      RETURN
      END
