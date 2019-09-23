      SUBROUTINE THERMAL
      INCLUDE "glgrid.inc"

      DIMENSION TICE (NX,NY), CN (NX,NY), DNT (NX,NY), DTT(NX,NY),
     *          JO (NX,NY), JOFN (NX,NY), A (NX,NY), B (NX,NY)
      COMMON / WINTEMP / A , B
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / DELTCN / DNT , DTT
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / CONSTAN / DL , DT , ETA
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
C
      DATA TALPHA, TBETA, TGAMA, TZETA / 3.0E-06 , 6.0E-05 ,
     *     6.0E-06 , 1.0E-03 /
C
      DO 100 I = 1 , NM1
      DO 100 J = 1 , MM1
      IF ( JOFN (I,J) .EQ. 0 ) GO TO 100
      CNIJ = CN (I,J)
      TICEIJ = TICE (I,J)
      WINDIJ = A (I,J)
      TEMPIJ = B (I,J)
      DELTEMP = 32.0 - TEMPIJ
      IF ( TEMPIJ .GT. 32.0 ) GO TO 10
      EM =  DELTEMP * ( TALPHA * CNIJ / TICEIJ + TBETA * WINDIJ *
     *       ( 1.0 - CNIJ ) )
      EA = TBETA * DELTEMP * WINDIJ * (1.0 - CNIJ) / ( RHOICE * TICEIJ )
      IF ( CNIJ .LE. 0. ) CNIJ = 0.001
      GO TO 20
   10 IF ( CNIJ .LT. 0.10 .OR. TICEIJ .LT. 0.03 ) GO TO 100
      EM = TGAMA * WINDIJ * DELTEMP * CNIJ
      EA = TZETA * EM / TICEIJ
   20 DTT (I,J) = DTT (I,J) + DT * ( EM / ( CNIJ * RHOICE ) -
     *             EA * TICEIJ / CNIJ )
      DNT (I,J) = DNT (I,J) + DT * EA
  100 CONTINUE
      RETURN
      END
