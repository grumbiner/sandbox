      SUBROUTINE readcntl(numodel,numfcst,numvfdate,numvfyobs,numarea,
     +            numstat,numvarbl,numlevel,numvector)
C
      INCLUDE 'parm.inc'
C
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), 
     +            namvfdate(mxdate), namvfyobs(maxobs), 
     +            namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)
      CHARACTER*80 input, substr (3)
C
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      LOGICAL	  vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
      CHARACTER*1 blank
      DATA blank /' '/
C
C   READ NUMBER OF MODELS TO BE VERIFIED, first model name, and
C   optional wind rotation grid # for the model.
C
C   Note:  no adherence to format is necessary for this input.
C
        READ (5,'(A)') input
        CALL ST_CLST ( input, ' ', ' ', 3, substr, num, ier )
	CALL ST_NUMB ( substr (1), numodel, ier )
	CALL ST_RMBL ( substr (2), namodel (1), lng, ier )
	nchrmodel (1) = lng
	IF ( substr (3) .ne. ' ' ) THEN
	   CALL ST_NUMB ( substr (3), nmbgrd (1), ier )
	ELSE
	   nmbgrd (1) = -1
        END IF
C
C     NUMBER OF VERIFYING MODELS IS LIMITED TO MAXMOD
      IF (numodel.gt.maxmod) THEN
        PRINT '("  NUMBER OF VERIFYING MODELS EXCEEDS LIMIT OF",2I5)', 
     +              maxmod, numodel
        STOP 16
      END IF
C     
C     READ NUMODEL MODEL PNEMONICS AND GET CHARACTER COUNT FOR EACH
C     Also read in optional wind rotation flag (false if missing).
C     
C   Note:  no adherence to format is necessary for this input.
C
      DO 30 n = 1, numodel
        IF ( n .ne. 1 ) THEN
	    READ (5,'(A)') input
      	    CALL ST_CLST ( input, ' ', ' ', 2, substr, num, ier )
	    CALL ST_RMBL ( substr (1), namodel (n), lng, ier )
	    nchrmodel (n) = lng
	    IF ( substr (2) .ne. ' ' ) THEN
	       CALL ST_NUMB ( substr (2), nmbgrd (n), ier )
	    ELSE
	       nmbgrd (n) = -1
            END IF
	END IF
	CALL SETMODEL(N,NAMODEL(N),NCHRMODEL(N))
	PRINT *, ' GRD # for wind rotation = ', nmbgrd (n)
   30 CONTINUE
C     
C     READ NUMBER OF FORECAST HOURS TO BE VERIFIED AND first hour.
C     
	CALL ST_READ  ( namfcst, nchrfcst, numfcst, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading forecast hours.'
	    STOP
	END IF
C
C*      NUMBER OF FORECAST HOURS IS LIMITED TO MXFCST
        IF (numfcst.gt.mxfcst) THEN
          PRINT '("  NUMBER OF FORECAST HOURS EXCEEDS LIMIT OF",2I5)', 
     +              mxfcst, numfcst
          STOP 16
        END IF
C     
C       SET NUMFCST FORECAST HOUR PNEMONICS
C     
        DO 60 n = 1, numfcst
          CALL setfcst(n,namfcst(n),nchrfcst(n))
   60   CONTINUE
C     
C     READ NUMBER OF VERIFICATION DATES
C     
	CALL ST_READ  ( namvfdate, nchrvfdate, numvfdate, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verifying dates.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING DATES IS LIMITED TO MXDATE
        IF (numvfdate.gt.mxdate) THEN
          PRINT '("  NUMBER OF VERIFYING DATES EXCEEDS LIMIT OF",2I5)', 
     +              mxdate, numvfdate
          STOP 16
        END IF
C     
C       READ NUMVFDATE VERIFICATION PNEMONICS AND GET CHARACTER COUNT
C     
C     READ NUMBER OF VERIFYING OB TYPES
C     
	CALL ST_READ  ( namvfyobs, nchrvfyobs, numvfyobs, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verifying ob types.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING OBS IS LIMITED TO MAXOBS
        IF (numvfyobs.gt.maxobs) THEN
          PRINT '("  NUMBER OF VERIFYING OBS EXCEEDS LIMIT OF",2I5)', 
     +              maxobs, numvfyobs
          STOP 16
        END IF
C     
C       SET NUMVFYOBS VERIFYING OBS PNEMONICS
C     
        DO 110 n = 1, numvfyobs
          CALL setobtyp(n,namvfyobs(n),nchrvfyobs(n))
  110   CONTINUE
C     
C     READ NUMBER OF VERIFICATION AREAS
C     
	CALL ST_READ  ( namarea, nchrarea, numarea, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verification areas.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING AREAS IS LIMITED TO MXAREA
        IF (numarea.gt.mxarea) THEN
          PRINT '("  NUMBER OF VERIFYING AREAS EXCEEDS LIMIT OF",2I5)', 
     +              mxarea, numarea
          STOP 16
        END IF
C     
C       SET NUMAREA DOMAIN PNEMONICS
C     
        DO 140 n = 1, numarea
          CALL setarea(n,namarea(n),nchrarea(n))
  140   CONTINUE
C     
C     READ NUMBER OF STATISTICAL SCORES
C     
	CALL ST_READ  ( namstat, nchrstat, numstat, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading statistic types.'
	    STOP
	END IF
C
C       NUMBER OF STATISTICS IS LIMITED TO MXSTAT
        IF (numstat.gt.mxstat) THEN
          PRINT '("  NUMBER OF STATISTICS EXCEEDS LIMIT OF",2I5)',
     +		    mxstat, numstat
          STOP 16
        END IF
C     
C     READ VARIABLES TO BE VERIFIED
C     
	CALL ST_READ  ( namvarbl, nchrvarbl, numvarbl, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading parameters.'
	    STOP
	END IF
C
C       NUMBER OF VARIABLES IS LIMITED TO MXVRBL
        IF (numvarbl.gt.mxvrbl) THEN
          PRINT '("  NUMBER OF VARIABLES EXCEEDS LIMIT OF",2I5)',
     +		mxvrbl, numvarbl
          STOP 16
        END IF
C     
C       FIND VECTOR VARIABLE (must be last in list)
C     
        numvector = 0
	
	DO ivr = 1, numvarbl
          IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'VWND') THEN
            numvector = ivr
	    IF ( numvector .ne. numvarbl ) THEN
		WRITE (6,*) ' VWND must be last in parm list.'
		STOP 16
	    END IF
            numvarbl = numvarbl + 1
            namvarbl(ivr+1) = namvarbl(ivr) (1:1)
            nchrvarbl(ivr+1) = 1
          END IF
	END DO
C     
C     READ NUMBER OF LEVEL DESCRIPTIONS
C     
	CALL ST_READ  ( namlevel, nchrlevel, numlevel, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verifying levels.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING LEVELS IS LIMITED TO MAXLVL
        IF (numlevel.gt.maxlvl) THEN
          PRINT '(" NUMBER OF VERIFYING LEVELS EXCEEDS LIMIT OF",2I5)', 
     +              maxlvl, numlevel
          STOP 16
        END IF
C     
C       READ NUMLEVEL LEVEL PNEMONICS AND GET CHARACTER COUNT
C     
        DO 220 n = 1, numlevel
          CALL setlevel(n,namlevel(n),nchrlevel(n))
  220   CONTINUE
C*
      RETURN
      END
