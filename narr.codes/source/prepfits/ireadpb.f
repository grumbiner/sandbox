C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION ireadpb(lunit)

      REAL*8 hdr(10), cat(255), obs(10,255), qms(10,255)

      COMMON /observ/ hdr, cat, obs, qms, nlev
      COMMON /obstrs/ headr, obstr, qmstr, subset, idate, nsub
 
      CHARACTER*80 headr, obstr, qmstr
      CHARACTER*8 subset,sid
      REAL*8 hdn(10), can(255), obn(10,255), qmn(10,255), 
     +            tob(2,255,10)
 
      dimension mand(12)
      equivalence(sid,hdr(1))

      DATA bmiss /10E10/
      DATA ievn /10/
      data mand /1000,925,850,700,600,500,400,300,250,200,150,100/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  READ AND UNPACK THE NEXT SUBSET FROM LUNIT
C  ------------------------------------------
 
      CALL readsb(lunit,ireadpb)
      IF (ireadpb.ne.0) RETURN
 
      CALL ufbint(lunit,hdr,10,1,iret,headr)
      CALL ufbint(lunit,cat,1,255,nlev,'CAT')
      CALL ufbint(lunit,qms,10,255,nlev,qmstr)
      CALL ufbint(lunit,obs,10,255,nlev,obstr)
      CALL ufbevn(lunit,tob,2,255,10,nlev,'TOB TPC')

      imand=0

      if(subset.eq.'ADPUPA') then
      do i=1,nlev
c      cat(i)=1.
       if(cat(i).eq.100000000000.) then
c      print*,'i,cat(i),obs(1,i)=',i,cat(i),obs(1,i)
       do j=1,12
        if(nint(obs(1,i)).eq.mand(j)) then
c        print*,'nint(obs(1,i)),mand(j)=',
c    *     nint(obs(1,i)),mand(j)
         imand=1
         goto 90
        endif
       enddo
       imand=0
90     continue
       if(imand.eq.1) then
        cat(i)=1.
       else
        cat(i)=2.
       endif
       endif
c     print*,'i,cat(i),obs(1,i)=',i,cat(i),obs(1,i)
      enddo
      endif

C     PEEL BACK EVENTS TO STORE SENSIBLE TEMP IN CASE TEMP IS VIRTUAL
C     ---------------------------------------------------------------

      DO i = 1, nlev
        DO j = 1, ievn
          IF (tob(2,i,j).eq.8) THEN
            obs(3,i) = tob(1,i,j+1)
            GO TO 10
          ELSE IF (tob(2,i,j).ge.bmiss) THEN
            GO TO 10
          END IF
        END DO
   10   CONTINUE
      END DO
 
C     CHECK TO SEE IF A COMPANION MASS OR WIND PIECE FOLLOWS
C     ------------------------------------------------------
   20 CALL ufbget(lunit,hdn,10,iret,headr)
      IF (iret.lt.0) THEN
        CALL readmg(lunit,subset,idate,ireadpb)
        IF (ireadpb.ne.0) RETURN
        nsub = nmsub(lunit)
        GO TO 20
      END IF
 
      DO i = 1, 5
        IF (hdr(i).ne.hdn(i)) RETURN
      END DO
 
C     COMBINE A COMPANION PIECE WITH ITS MATE
C     ---------------------------------------
 
      CALL readsb(lunit,iret)
c     IF (iret.ne.0) CALL abort('READPB - SUBSET SYNC ERROR')
      IF (iret.ne.0) then
           print*,'READPB - SUBSET SYNC ERROR'
           CALL abort
      END IF
 
      CALL ufbint(lunit,can,1,255,nlen,'CAT')
      CALL ufbint(lunit,qmn,10,255,nlen,qmstr)
      CALL ufbint(lunit,obn,10,255,nlen,obstr)
      CALL ufbevn(lunit,tob,2,255,10,nlen,'TOB TPC')

C     PEEL BACK EVENTS TO STORE SENSIBLE TEMP IN CASE TEMP IS VIRTUAL
C     ---------------------------------------------------------------

      DO i = 1, nlen
        DO j = 1, ievn
          IF (tob(2,i,j).eq.8) THEN
            obn(3,i) = tob(1,i,j+1)
            GO TO 30
          ELSE IF (tob(2,i,j).ge.bmiss) THEN
            GO TO 30
          END IF
        END DO
   30   CONTINUE
      END DO
 
C     MERGE OR ADD THE NEW LEVELS TO THE ORIGINAL ONES
C     ------------------------------------------------
 
      DO 40 l2 = 1, nlen
        DO l1 = 1, nlev
          IF (obs(1,l1).eq.obn(1,l2)) THEN
            DO i = 1, 10
              IF (obs(i,l1).ge.bmiss) THEN
                obs(i,l1) = obn(i,l2)
                qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 40
          ELSE IF (obn(1,l2).gt.obs(1,l1).or.l1.eq.nlev) THEN
            nlev = nlev + 1
            DO i = 1, 10
              obs(i,nlev) = obn(i,l2)
              qms(i,nlev) = qmn(i,l2)
            END DO
            cat(nlev) = can(l2)
            GO TO 40
          END IF
        END DO
   40   CONTINUE
 
C       RETURN WITH A COMBINED REPORT
C       -----------------------------
 
        RETURN
        END
