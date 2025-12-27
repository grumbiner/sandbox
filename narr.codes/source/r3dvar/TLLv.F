                          SUBROUTINE TLLv
     1 (ALMD,APHD,TLMOD,DTR,CTPH0,STPH0,TLM,TPH,n)
c---------
         dimension almd(n),aphd(n),tlm(n),tph(n)
C---------------------------------------------------------------------
         do i=1,n
      RELM=(ALMD(i)-TLMOD)*DTR
      SRLM=SIN(RELM)
      CRLM=COS(RELM)
      APH=APHD(i)*DTR
      SPH=SIN(APH)
      CPH=COS(APH)
      CC=CPH*CRLM
      ANUM=CPH*SRLM
      DENOM=CTPH0*CC+STPH0*SPH
      TLM(i)=ATAN2(ANUM,DENOM)/DTR
      TPH(i)=ASIN(CTPH0*SPH-STPH0*CC)/DTR
         end do
      RETURN
      END
