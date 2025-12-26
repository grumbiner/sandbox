	PROGRAM NGM2BUFR
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: NGM_NGM2BUFR
C   PRGMMR: ROGERS           ORG: NP22        DATE: 2000-02-14
C
C ABSTRACT: This programs converts the sequential sounding data
C     to the BUFR format.
C
C PROGRAM HISTORY LOG:
C   96-02-14 Keith Brill 
C   96-05-03 Keith Brill - Changes to correct for precip accumulated ove
C			   12-hr intervals (output is hourly precip)
C			   Change precip units to mm (kg/m**2)
C			   Change lat/lon to degrees
C   96-10-30 Keith Brill - Changes to write to individual station files
C   98-07-29 Keith Brill - Changes for Y2K; remove W3LOG
C   00-02-09 Eric Rogers - Converted to run on IBM-SP
C
C USAGE:
C   INPUT FILES:
C     FT40F001 - Table file
C     FT47F001 - Input sequential file
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     FT89F001 - Output BUFR file.  Assigned internally using ISHELL.
C     FT06F001 - print error messages
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - GUVTQP, BFRIZE
C     LIBRARY:
C       W3LIB    -
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C*** PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   PROGRAM:    NGM2BUF
C   PRGMMR: K. Brill         ORG: W/NP22     DATE: 96-02-07
C
C ABSTRACT: THIS PROGRAM BUFRIZES THE NGM SOUNDING OUTPUT.
C
C*	Assign fort.40 to the BUFR TABLE file (input).
C*
C*	    assign -a bufr_ngm.tbl fort.40
C*
C*	Assign fort.47 to the input sequential file:
C*
C*	    assign -a ngm.YYMMDDHH.RFwvsnd -F cos -C ascii -N ibm fort.47
C*
C*	fort.89 is automatically assigned to the output BUFR file:
C*
C*	    The output file is named ${DIRD}#####.YYMMDDHH, where
C*	    DIRD is an environmental variable, ##### is the station
C*	    number, and YYMMDDHH is the date time stamp.
C
C$$$
C***********************************************************************
C*   Main program to convert NGM sequential sounding output files to	*
C*   BUFR files.							*
C***********************************************************************
	PARAMETER	(MXSTN=500,MXFHR=49,MXDAT=134)
	REAL(8)   	qsndat (256), wrkspc (256)
	LOGICAL		done, more, debug
	CHARACTER	seqnam (5)*8, clist (5)*80, sbset*8
	CHARACTER*80	ASSIGN, FMTO
	LOGICAL		seqflg (5), first
	INTEGER		npr (5)
C*
        REAL    	prcpg (MXSTN), prcpc (MXSTN)
        INTEGER		istns (MXSTN), npstn
        COMMON		prcpg, prcpc, istns, npstn
	REAL(8)		sndat (MXSTN,MXFHR,MXDAT)
	COMMON/dat/	sndat
C*
	DATA		seqnam / 'HEADR', 'PROFILE', 'CLS0', 'FLXRAD',
     +				 'MISC' /
	DATA		seqflg / .false., .true., .false., .false.,
     +				 .false. /
	DATA		sbset / 'NGM' /
C-----------------------------------------------------------------------
C
      CALL W3TAGB('NGM_NGM2BUFR',2000,0045,0080,'NP22')
C
        FMTO = '("ln -s ${DIRD}",I5.5,".",4I2.2,'//
     + 		'" fort.",I2.2)'
c	debug = .true.
        debug = .false.
	iskip = 1
	luntbl = 40
	lunseq = 47
	lunbuf = 89
	npstn = 0
	clist (1) = ' '
	done = .false.
	first = .true.
	iqstn = 0
	iqhr = 1
	stnout = 72403
	DO WHILE ( .not. done )
	    CALL GUVTQP ( lunseq, iyr, imn, idy, ihr, qsndat, ier )
	    IF ( ier .lt. 0 ) THEN
		WRITE (6,*) ' READ failed.'
		STOP
	    ELSE IF ( ier .gt. 0 ) THEN
		WRITE (6,*) ' Normal END OF FILE encountered.'
		done = .true.
	    ENDIF
	    IF ( .not. done ) THEN
	      iqstn = iqstn + 1
	      IF ( first ) THEN
		stn1 = qsndat (2)
		first = .false.
	      ELSE IF ( stn1 .eq. qsndat (2) ) THEN
		iqstn = 1
		iqhr = iqhr + 1
	      ENDIF
	      IF ( iqstn .le. MXSTN .and. iqhr .le. MXFHR ) THEN
C*	-------------------------DBUG PRINTS-------------------------
	          IF ( debug )
     +    		print *, ' Processing frcst time ',
     +			qsndat(1), ' for ',
     +			qsndat (2), ' qsndat(20) = ', qsndat(20)
C*	-------------------------------------------------------------
	          DO i = 1, MXDAT
		    sndat (iqstn,iqhr,i) = qsndat (i)
	      	  END DO
	      ELSE
		  IF ( iqstn .gt. MXSTN ) WRITE (6,*)
     +		  ' Number of stations = ', iqstn, ' exceeds ',
     +		  MXSTN
		  IF ( iqhr .gt. MXFHR ) WRITE (6,*)
     +		  ' Number of times = ', iqhr, ' exceeds ', MXFHR
	      END IF
	    END IF
	END DO
C*
	DO istn = 1, iqstn, iskip
	    ista = NINT ( sndat ( istn, 1, 2 ) )
    	    IF ( ista .ne. istns (istn) ) THEN
		WRITE (6,*) ' Station count error'
		STOP
	    END IF
	    jyr = MOD ( iyr, 100 )
            print *,'before assign ',sndat(istn,1,2),ista,jyr,imn,idy,
     +           ihr,lunbuf
            WRITE (ASSIGN,FMTO) ista,jyr,imn,idy,ihr,lunbuf
            CALL SYSTEM(ASSIGN)
c           IF(ISHELL(ASSIGN).NE.0) CALL ABORT('BAD ASSIGN:'//ASSIGN)
	    DO jhr = 1, iqhr
	        DO i = 1, MXDAT
		    qsndat (i) = sndat (istn,jhr,i)
	      	END DO
	        CALL BFRIZE ( luntbl, lunbuf, sbset,
     +			      iyr, imn, idy, ihr,
     +			      seqnam, seqflg, 5, .true., qsndat,
     +			      16, clist, npr, wrkspc, ier )
C	------------------DBUG PRINTS-----------------------------------
		    IF ( MOD (jhr,10) .eq. 0 .and. debug ) THEN
			DO jdb = 3, MXDAT
			    print *, sndat (istn,jhr,1),
     +				sndat(istn,jhr,2), sndat (istn,jhr,jdb)
			END DO
		    END IF	
C*	----------------------------------------------------------------	
	    	IF ( ier .ne. 0 ) THEN
		    WRITE (6,*) ' BFRIZE failed.'
	    	END IF
	    END DO
	    CALL BFRIZE ( 0, lunbuf, sbset, iyr, imn, idy, ihr,
     +		      seqnam, seqflg, 5, .true., sndat, 16,
     +		      clist, npr, wrkspc, ier )
	END DO
      CALL W3TAGE('NGM_NGM2BUFR')
	STOP
	END
