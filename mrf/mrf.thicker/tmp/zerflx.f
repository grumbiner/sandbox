      SUBROUTINE ZERFLX(DUSFC,DVSFC,DTSFC,DQSFC,DLWSFC,ULWSFC,
     1 BENGSH,GFLUX,RUNOFF,EP,CLDWRK,
     2 DUGWD,DVGWD,PSMEAN)
      DIMENSION DUSFC( 384 , 47 ),DVSFC( 384 , 47 )
      DIMENSION DTSFC( 384 , 47 ),DQSFC( 384 , 47 )
      DIMENSION DLWSFC( 384 , 47 ),ULWSFC( 384 , 47 )
      DIMENSION BENGSH( 384 , 47 ),GFLUX( 384 , 47 )
      DIMENSION RUNOFF( 384 , 47 ),EP( 384 , 47 )
      DIMENSION CLDWRK( 384 , 47 )
      DIMENSION DUGWD( 384 , 47 ),DVGWD( 384 , 47 )
      DIMENSION PSMEAN( 384 , 47 )
      DO 23500 L=1, 47
      DO 23500 J=1, 384
       DUSFC(J,L)=0. E 0
       DVSFC(J,L)=0. E 0
       DTSFC(J,L)=0. E 0
       DQSFC(J,L)=0. E 0
      DLWSFC(J,L)=0. E 0
      ULWSFC(J,L)=0. E 0
      BENGSH(J,L)=0. E 0
       GFLUX(J,L)=0. E 0
       RUNOFF(J,L) = 0. E 0
       EP(J,L) = 0. E 0
       CLDWRK(J,L) = 0. E 0
       DUGWD(J,L)=0. E 0
       DVGWD(J,L)=0. E 0
       PSMEAN(J,L)=0. E 0
23500 CONTINUE
C--------------------------------------------------------------
      RETURN
      END
