                          SUBROUTINE invTLLv
     1 (ALMD,APHD,TLMOD,DTR,CTPH0,STPH0,TLM,TPH,n)
c---------
c--------- inverse of TLLv:  input ALMD,APHD is grid lon,lat
c---------                   output is earth lon,lat, TLM,TPH
c---------
         dimension almd(n),aphd(n),tlm(n),tph(n)
C---------------------------------------------------------------------
         do i=1,n
      RELM=ALMD(i)*DTR
      SRLM=SIN(RELM)
      CRLM=COS(RELM)
      APH=APHD(i)*DTR
      SPH=SIN(APH)
      CPH=COS(APH)
      CC=CPH*CRLM
      ANUM=CPH*SRLM
      DENOM=CTPH0*CC-STPH0*SPH
      TLM(i)=tlmod+ATAN2(ANUM,DENOM)/DTR
      TPH(i)=ASIN(CTPH0*SPH+STPH0*CC)/DTR
         end do
      RETURN
      END
