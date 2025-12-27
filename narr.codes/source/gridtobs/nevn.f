C----------------------------------------------------------------------
C----------------------------------------------------------------------
      FUNCTION nevn(node,lun,inv1,inv2,i1,i2,i3,user)
 
      COMMON /usrint/ nval(32), inv(15000,32), val(15000,32)
 
      DIMENSION user(i1,i2,i3)
      REAL*8 val, user
 
      DATA bmiss /10E10/
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
      nevn = 0
 
C     FIND THE ENCLOSING EVENT STACK DESCRIPTOR
C     -----------------------------------------
 
      ndrs = lstjpb(node,lun,'DRS')
      IF (ndrs.le.0) RETURN
 
      invn = invwin(ndrs,lun,inv1,inv2)
      IF (invn.gt.0) THEN
 
        nevn = val(invn,lun)
        IF (nevn.gt.i3) GO TO 10
 
C       SEARCH EACH STACK LEVEL FOR THE REQUESTED NODE AND COPY THE VALUE
C       -----------------------------------------------------------------
 
        n2 = invn + 1

        DO l = 1, nevn
          n1 = n2
          n2 = n2 + val(n1,lun)
          DO n = n1, n2
            IF (inv(n,lun).eq.node) user(1,1,l) = val(n,lun)
          END DO
        END DO
 
        RETURN
      END IF
      print*,'NEVN - CANT FIND THE EVENT STACK!!!!!!'
      CALL abort
   10 print*,'NEVN - TOO MANY EVENTS FOR USER ARRAY!' 
      CALL abort
      END
