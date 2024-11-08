      PROGRAM bufrout
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C 
C MAIN PROGRAM: SSMIBUFR    SELECTIVE BUFR -> BINARY TRANSLATION
C   PRGMMR: GRUMBINE         ORG: NP21        DATE: 97-09-05  
C
C ABSTRACT: READ IN BUFR MESSAGES, SELECT THOSE MESSAGES WHICH
C    CORRESPOND TO A SATELLITE OF INTEREST, AND WRITE THEM OUT
C    AS UNFORMATTED BINARY FOR LATER USE.
C
C PROGRAM HISTORY LOG:
C   97-08-15  Robert Grumbine     ORIGINAL AUTHOR
C   98-07-21  Robert Grumbine  Y2K and F90 fixes
C   99-12-28  Robert Grumbine  Transfer to IBM-SP, requires changes to type
C                                sizes imposed by BUFRLIB
C 2000-08-15  Robert Grumbine  High resolution ssmi channels added
C 2001-05-24  Robert Grumbine  Changes for running on LINUX as well
C
C USAGE:
C   INPUT FILES:
C     FTNF14 - BUFR SOURCE DATA TANK
C   OUTPUT FILES:
C     FTNF51 - UNFORMATTED DATA FROM DESIRED SATELLITE(S)
C
C SUBPROGRAMS CALLED:
C    UNIQUE: NONE
C    BUFRLIB: OPENBF, UFBINT, UFBREP AND THEIR DESCENDANTS
C
C EXIT STATES:
C    COND = 0 - SUCCESSFUL RUN
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C   MACHINE: CRAY
C
C$$$ 
C     Program core from Vera Gerald 3/25/97.  
C     Read in bufr files and write out unformatted binary for
C       use by other programs
C     Robert Grumbine 4 June 1997.

      IMPLICIT none

      CHARACTER*80  XIDST,XLCST,BRTST,XL85ST, BR85ST
      CHARACTER*8 inout,subset
      REAL     XIDENT(9),xloc(4,64),BRT(2,7,64),temps(7)
      REAL*8   XIDENT8(9), xloc8(4,64), BRT8(2,7,64), temps8(7)
      REAL     xloc85(4, 192), brt85(2, 2, 192)
      REAL*8   xloc858(4, 192), brt858(2, 2, 192)
      INTEGER index, index2, index3
      INTEGER iy, id, im, imins, isad, iunt, jcnt, jret, iret
      INTEGER kret, k2ret, kwrit, k
      INTEGER kscan, posn, sftg
      REAL tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7
      REAL xlat, xlon 
      INTEGER isec, iscan, ihr, idate, icnt
      INTEGER  ireadsb, ireadmg
      INTEGER creturns, openout, shortout, longout, hiresout
C-----------------------------------------------------------------------
C
C  THE LIST OF SSMI DATA AS CONVERTED AND WRITTEN TO BUFR FOLLOWS:
C
C     ARRAY            MNEMONIC   DESCRIPTOR   DESCRIPTION
C     -----            --------   ----------   -----------
C
C     XIDENT(1)        SAID       001007       SATELLITE ID
C     XIDENT(2)        YEAR       004001       YEAR
C     XIDENT(3)        MNTH       004002       MONTH
C     XIDENT(4)        DAYS       004003       DAY
C     XIDENT(5)        HOUR       004004       HOUR
C     XIDENT(6)        MINU       004005       MINUTE
C     XIDENT(7)        SECO       004006       SECOND
C     XIDENT(8)        ORBN       005040       ORBIT NUMBER
C     XIDENT(9)        SCNN       005201       SCAN NUMBER
C
C     xloc(1,1:64)     CLAT       005002       LATITUDE
C     xloc(2,1:64)     CLON       006002       LONGITUDE
C     xloc(3,1:64)     SFTG       020217       SURFACE TAG
C     xloc(4,1:64)     POSN       005202       POSITION NUMBER
C
C     BRT(1,1:7,1:64)  CHNM       005042       CHANNEL NUMBER
C     BRT(2,1:7,1:64)  TMBR       012063       RADIANCE TEMPERATURE
C
C     xloc85(1,1:192)  CLAT85     005255       Lat
C     xloc85(2,1:192)  CLON85     005255       Lon
C     xloc85(3,1:192)  SFTG85     005255       Surface Tag
C     xloc85(4,1:192)  POSN85     005255       Position Number
C
C     brt85(1,1:2,1:192) CHNM85   005254       Channel number
C     brt85(2,1:2,1:192) TMBR85   012255       Radiance Temperature
C     
C
C-----------------------------------------------------------------------
C
      REAL hiresobs(4,3)
      INTEGER i, j, ount, count
      INTEGER F11, F13, F14, F15, SATNO
      PARAMETER (F11 = 244)
      PARAMETER (F13 = 246)
      PARAMETER (F14 = 247)
      PARAMETER (F15 = 248)
      DATA XIDST,XLCST,BRTST,XL85ST, BR85ST
     1/'SAID YEAR MNTH DAYS HOUR MINU SECO ORBN SCNN               ',
     2 'CLAT CLON SFTG POSN                                        ',
     3 'CHNM TMBR                                                  ',
     4 'CLAT85 CLON85 SFTG85 POSN85                                ',
     5 'CHNM85 TMBR85                                              '/
cc  where IUNT is the FORTRAN I/O UNIT NUMBER for the tank of
cc  SSM/I brightness data.

      icnt = 0
      jcnt = 0
      iunt = 14
      ount = 51
      inout = 'IN'
      DO 2000 j = 1, 3 
      DO 2100 i = 1, 4
        hiresobs(i,j) = 0.0
 2100 CONTINUE
 2000 CONTINUE


      count = 0
CD      CALL W3TAGB('SSMIBUFR',1999,0248,0070,'NP21   ')
       OPEN (iunt, FORM="UNFORMATTED", STATUS="OLD")
       OPEN (ount, FORM="UNFORMATTED", STATUS="NEW")
       CALL openbf(iunt,inout,iunt)
       creturns = openout(ount)

       DO WHILE (ireadmg(iunt,subset,idate).eq.0) 
         DO WHILE (ireadsb(iunt).eq.0)

           count = count + 1

           CALL UFBINT(IUNT,XIDENT8,9,  1,iret,XIDST)
           DO index = 1, 9
             XIDENT(index) = XIDENT8(index)
           ENDDO
           IF (XIDENT(1) .GT. F15 .OR. XIDENT(1) .LT. F11 ) GO TO 1000
           SATNO = XIDENT(1)
           CALL UFBINT(IUNT,xloc8  ,4, 64,iret,XLCST)
           DO index = 1, 4
           DO index2 = 1, 64
             xloc(index, index2) = xloc8(index, index2)
           ENDDO
           ENDDO
           CALL UFBREP(IUNT,BRT8  ,2,448,jret,BRTST)
           DO index = 1, 2
           DO index2 = 1, 7
           DO index3 = 1, 64
             BRT(index, index2, index3)= BRT8(index, index2, index3)
           ENDDO
           ENDDO
           ENDDO
           CALL UFBINT(IUNT, xloc858, 4, 192, kret, XL85ST)
           DO index = 1, 4
           DO index2 = 1, 192
             xloc85(index, index2) = xloc858(index, index2)
           ENDDO
           ENDDO
           CALL UFBREP(IUNT, brt858, 2, 384, k2ret, BR85ST)
           DO index = 1, 2
           DO index2 = 1, 2
           DO index3 = 1, 192
             brt85(index, index2, index3) =
     1            brt858(index, index2, index3)
           ENDDO
           ENDDO
           ENDDO

           icnt = icnt + 1
           IF (jret .GT. 1) THEN
             jcnt = jcnt + 1 

             iy = XIDENT(2)
             im = XIDENT(3)
             id = XIDENT(4)
             ihr = XIDENT(5)
             imins = XIDENT(6)
             isad = XIDENT(1)
             iscan = XIDENT(9)
             isec = XIDENT(7)
             creturns = shortout(SATNO, iy,im,id,ihr,imins,isec,iscan)
CD             IF (count .LT. 100) THEN
CD               WRITE (*,*) SATNO, iy,im,id,ihr,imins,isec,iscan
CD             ENDIF

            DO 10 kscan = 1,iret
              xlat = xloc(1,kscan)
              xlon = xloc(2,kscan)
              sftg = xloc(3,kscan)
              posn = xloc(4,kscan)
              tmp1 = brt(2,1,kscan)
              tmp2 = brt(2,2,kscan)
              tmp3 = brt(2,3,kscan)
              tmp4 = brt(2,4,kscan)
              tmp5 = brt(2,5,kscan)
              tmp6 = brt(2,6,kscan)
              tmp7 = brt(2,7,kscan)
              creturns = longout(kscan,xlat,xlon,sftg,posn,
     1               tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7)

C             Now write out the hires info:
CLORES              DO 20 k = 1, 3
CLORES                kwrit = (kscan-1)*3 + k
CLORES                xlat = xloc85(1, kwrit)
CLORES                xlon = xloc85(2, kwrit)
CLORES                sftg = xloc85(3, kwrit)
CLORES                posn = xloc85(4, kwrit)
CLORES                tmp1 = brt85(2, 1, kwrit)
CLORES                tmp2 = brt85(2, 2, kwrit)
CLORES                creturns = hiresout(kwrit, xlat, xlon, sftg, posn, 
CLORES     1                              tmp1, tmp2)
CLORES  20          CONTINUE
  10        CONTINUE

         ENDIF
       
 1000    CONTINUE 
         ENDDO
       ENDDO

CD      CALL W3TAGE('SSMIBUFR')

      STOP
      END
