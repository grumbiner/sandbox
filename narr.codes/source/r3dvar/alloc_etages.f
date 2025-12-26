       subroutine alloc_etages

! allocate "common" space for reading in eta restart file

         include "PARMETA.comm"
         include "PARMTBL.comm"
         include "parmsoil"
         include "mpif.h"
         include "my_comm.h"

         include "LOOPS.comm"
         include "MASKS.comm"
         include "MASK1.comm"
         include "DYNAM.comm"
         include "PHYS2.comm"
         include "MAPOT1.comm"
         include "VRBLS.comm"
         include "CONTIN.comm"
         include "PVRBLS.comm"
         include "BOCO.comm"
         include "ACMCLH.comm"
         include "ACMCLD.comm"
         include "ACMPRE.comm"
         include "ACMRDL.comm"
         include "ACMRDS.comm"
         include "ACMSFC.comm"
         include "CLDWTR.comm"
         include "CNVCLD.comm"
         include "SOIL.comm"
         include "INDX.comm"
         include "Z0EFFT.comm"
         include "TEMPV.comm"
         include "PRFHLD.comm"
         include "QFLX.comm"
         include "PPTASM.comm"

         integer(4),pointer::lvl(:,:)
       COMMON/RD1TIM/K400,CTHK(3),LTOP(3),PTOPC(4),TAUCV(3),RAD1,LVL

!   allocate space for common/loops/

         allocate(ihla(jam)) ; allocate(ihha(jam))
         allocate(ivla(jam)) ; allocate(ivha(jam))
         allocate(jra(jam))
         allocate(lmh(idim1:idim2,jdim1:jdim2))
         allocate(lmv(idim1:idim2,jdim1:jdim2))

!   allocate space for common/masks/

         allocate(vbm2(idim1:idim2,jdim1:jdim2))
         allocate(vbm3(idim1:idim2,jdim1:jdim2))
         allocate(sm(idim1:idim2,jdim1:jdim2))
         allocate(sice(idim1:idim2,jdim1:jdim2))
         allocate(htm(idim1:idim2,jdim1:jdim2,lm))
         allocate(vtm(idim1:idim2,jdim1:jdim2,lm))
         allocate(hbm2(idim1:idim2,jdim1:jdim2))
         allocate(hbm3(idim1:idim2,jdim1:jdim2))

!    allocate space for common/mask1/

         allocate(hbm1(idim1:idim2,jdim1:jdim2))
         allocate(vbm1(idim1:idim2,jdim1:jdim2))

!    allocate space for common/dynam/

         allocate(deta(lm))
         allocate(rdeta(lm))
         allocate(aeta(lm))
         allocate(f4q2(lm))
         allocate(eta(lp1))
         allocate(dfl(lp1))
         allocate(em(jam))
         allocate(emt(jam))
         allocate(dx(idim1:idim2,jdim1:jdim2))
         allocate(wpdar(idim1:idim2,jdim1:jdim2))
         allocate(cpgfu(idim1:idim2,jdim1:jdim2))
         allocate(curv(idim1:idim2,jdim1:jdim2))
         allocate(fcp(idim1:idim2,jdim1:jdim2))
         allocate(fdiv(idim1:idim2,jdim1:jdim2))
         allocate(f(idim1:idim2,jdim1:jdim2))
         allocate(ddmpu(idim1:idim2,jdim1:jdim2))
         allocate(ddmpv(idim1:idim2,jdim1:jdim2))
         allocate(fad(idim1:idim2,jdim1:jdim2))

!   allocate space for common/phys/

         allocate(deta2(lm))
         allocate(aeta2(lm))
         allocate(dfrlg(lp1))
         allocate(qs0(jtb))
         allocate(sqs(jtb))
         allocate(the0(itb))
         allocate(sthe(itb))
         allocate(the0q(itbq))
         allocate(stheq(itbq))
         allocate(mxsnal(idim1:idim2,jdim1:jdim2))
         allocate(epsr(idim1:idim2,jdim1:jdim2))
         allocate(radin(idim1:idim2,jdim1:jdim2))
         allocate(radot(idim1:idim2,jdim1:jdim2))
         allocate(glat(idim1:idim2,jdim1:jdim2))
         allocate(glon(idim1:idim2,jdim1:jdim2))
         allocate(czen(idim1:idim2,jdim1:jdim2))
         allocate(htop(idim1:idim2,jdim1:jdim2))
         allocate(hbot(idim1:idim2,jdim1:jdim2))
         allocate(tg(idim1:idim2,jdim1:jdim2))
         allocate(gffc(idim1:idim2,jdim1:jdim2))
         allocate(sst(idim1:idim2,jdim1:jdim2))
         allocate(albase(idim1:idim2,jdim1:jdim2))
         allocate(hdac(idim1:idim2,jdim1:jdim2))
         allocate(hdacv(idim1:idim2,jdim1:jdim2))
         allocate(czmean(idim1:idim2,jdim1:jdim2))
         allocate(sigt4(idim1:idim2,jdim1:jdim2))
         allocate(ptbl(itb,jtb))
         allocate(ttbl(jtb,itb))
         allocate(ttblq(jtbq,itbq))

!   allocate space for common/mapot/

         allocate(deta1(lm))
         allocate(aeta1(lm))
         allocate(eta1(lp1))

!   allocate space for common/vrbls/

         allocate(pd(idim1:idim2,jdim1:jdim2))
         allocate(t(idim1:idim2,jdim1:jdim2,lm))
         allocate(u(idim1:idim2,jdim1:jdim2,lm))
         allocate(v(idim1:idim2,jdim1:jdim2,lm))
         allocate(q(idim1:idim2,jdim1:jdim2,lm))
         allocate(fis(idim1:idim2,jdim1:jdim2))
         allocate(res(idim1:idim2,jdim1:jdim2))

!   allocate space for common/contin/

         allocate(pdsl(idim1:idim2,jdim1:jdim2))
         allocate(psdt(idim1:idim2,jdim1:jdim2))
         allocate(rtop(idim1:idim2,jdim1:jdim2,lm))
         allocate(omgalf(idim1:idim2,jdim1:jdim2,lm))
         allocate(div(idim1:idim2,jdim1:jdim2,lm))
         allocate(etadt(idim1:idim2,jdim1:jdim2,lm-1))

!   allocate space for common/pvrbls/

         allocate(z0(idim1:idim2,jdim1:jdim2))
         allocate(ustar(idim1:idim2,jdim1:jdim2))
         allocate(uz0(idim1:idim2,jdim1:jdim2))
         allocate(vz0(idim1:idim2,jdim1:jdim2))
         allocate(thz0(idim1:idim2,jdim1:jdim2))
         allocate(qz0(idim1:idim2,jdim1:jdim2))
         allocate(ths(idim1:idim2,jdim1:jdim2))
         allocate(qs(idim1:idim2,jdim1:jdim2))
         allocate(akms(idim1:idim2,jdim1:jdim2))
         allocate(akhs(idim1:idim2,jdim1:jdim2))
         allocate(rf(idim1:idim2,jdim1:jdim2))
         allocate(twbs(idim1:idim2,jdim1:jdim2))
         allocate(qwbs(idim1:idim2,jdim1:jdim2))
         allocate(sno(idim1:idim2,jdim1:jdim2))
         allocate(si(idim1:idim2,jdim1:jdim2))
         allocate(cldefi(idim1:idim2,jdim1:jdim2))
         allocate(prec(idim1:idim2,jdim1:jdim2))
         allocate(acprec(idim1:idim2,jdim1:jdim2))
         allocate(accliq(idim1:idim2,jdim1:jdim2))
         allocate(cuprec(idim1:idim2,jdim1:jdim2))
         allocate(th10(idim1:idim2,jdim1:jdim2))
         allocate(q10(idim1:idim2,jdim1:jdim2))
         allocate(u10(idim1:idim2,jdim1:jdim2))
         allocate(v10(idim1:idim2,jdim1:jdim2))
         allocate(tshltr(idim1:idim2,jdim1:jdim2))
         allocate(qshltr(idim1:idim2,jdim1:jdim2))
         allocate(pshltr(idim1:idim2,jdim1:jdim2))
         allocate(q2(idim1:idim2,jdim1:jdim2,lm))
         allocate(afsi(idim1:idim2,jdim1:jdim2))
         allocate(th30(idim1:idim2,jdim1:jdim2))
         allocate(q30(idim1:idim2,jdim1:jdim2))
         allocate(u30(idim1:idim2,jdim1:jdim2))
         allocate(v30(idim1:idim2,jdim1:jdim2))

!   allocate space for common/boco/

         allocate(pdb(lb,2))
         allocate(tb(lb,lm,2))
         allocate(qb(lb,lm,2))
         allocate(ub(lb,lm,2))
         allocate(vb(lb,lm,2))
         allocate(q2b(lb,lm,2))
         allocate(cwmb(lb,lm,2))

!   allocate space for common/acmclh/

         allocate(train(idim1:idim2,jdim1:jdim2,lm))
         allocate(tcucn(idim1:idim2,jdim1:jdim2,lm))

!   allocate space for common/ACMCLD/

         allocate(acfrcv(idim1:idim2,jdim1:jdim2))
         allocate(ncfrcv(idim1:idim2,jdim1:jdim2))
         allocate(acfrst(idim1:idim2,jdim1:jdim2))
         allocate(ncfrst(idim1:idim2,jdim1:jdim2))

!   allocate space for common/acmpre/

         allocate(acsnow(idim1:idim2,jdim1:jdim2))
         allocate(acsnom(idim1:idim2,jdim1:jdim2))
         allocate(ssroff(idim1:idim2,jdim1:jdim2))
         allocate(bgroff(idim1:idim2,jdim1:jdim2))

!   allocate space for common/acmrdl/

         allocate(rlwin(idim1:idim2,jdim1:jdim2))
         allocate(rlwout(idim1:idim2,jdim1:jdim2))
         allocate(rlwtoa(idim1:idim2,jdim1:jdim2))
         allocate(alwin(idim1:idim2,jdim1:jdim2))
         allocate(alwout(idim1:idim2,jdim1:jdim2))
         allocate(alwtoa(idim1:idim2,jdim1:jdim2))
         allocate(rlwtt(idim1:idim2,jdim1:jdim2,lm))

!   allocate space for common/acmrds/

         allocate(rswin(idim1:idim2,jdim1:jdim2))
         allocate(rswout(idim1:idim2,jdim1:jdim2))
         allocate(rswtoa(idim1:idim2,jdim1:jdim2))
         allocate(aswin(idim1:idim2,jdim1:jdim2))
         allocate(aswout(idim1:idim2,jdim1:jdim2))
         allocate(aswtoa(idim1:idim2,jdim1:jdim2))
         allocate(rswtt(idim1:idim2,jdim1:jdim2,lm))

!   allocate space for common/acmsfc/

         allocate(sfcshx(idim1:idim2,jdim1:jdim2))
         allocate(sfclhx(idim1:idim2,jdim1:jdim2))
         allocate(subshx(idim1:idim2,jdim1:jdim2))
         allocate(snopcx(idim1:idim2,jdim1:jdim2))
         allocate(sfcuvx(idim1:idim2,jdim1:jdim2))
         allocate(sfcevp(idim1:idim2,jdim1:jdim2))
         allocate(potevp(idim1:idim2,jdim1:jdim2))
         allocate(potflx(idim1:idim2,jdim1:jdim2))

!   allocate space for common/cldwtr/

         allocate(cwm(idim1:idim2,jdim1:jdim2,lm))
         allocate(u00(idim1:idim2,jdim1:jdim2))
         allocate(ul(2*lm))
         allocate(lc(idim1:idim2,jdim1:jdim2))
         allocate(sr(idim1:idim2,jdim1:jdim2))

!   allocate space for common/cnvcld/

         allocate(cuppt(idim1:idim2,jdim1:jdim2))
         allocate(cfracl(idim1:idim2,jdim1:jdim2))
         allocate(cfracm(idim1:idim2,jdim1:jdim2))
         allocate(cfrach(idim1:idim2,jdim1:jdim2))

!   allocate space for common/soil/

         allocate(soiltb(idim1:idim2,jdim1:jdim2))
         allocate(sfcexc(idim1:idim2,jdim1:jdim2))
         allocate(smstav(idim1:idim2,jdim1:jdim2))
         allocate(smstot(idim1:idim2,jdim1:jdim2))
         allocate(grnflx(idim1:idim2,jdim1:jdim2))
         allocate(pctsno(idim1:idim2,jdim1:jdim2))
         allocate(ivgtyp(idim1:idim2,jdim1:jdim2))
         allocate(isltyp(idim1:idim2,jdim1:jdim2))
         allocate(islope(idim1:idim2,jdim1:jdim2))
         allocate(vegfrc(idim1:idim2,jdim1:jdim2))
         allocate(cmc(idim1:idim2,jdim1:jdim2))
         allocate(smc(idim1:idim2,jdim1:jdim2,nsoil))
         allocate(stc(idim1:idim2,jdim1:jdim2,nsoil))
         allocate(sh2o(idim1:idim2,jdim1:jdim2,nsoil))
         allocate(albedo(idim1:idim2,jdim1:jdim2))
         allocate(sldpth(nsoil))
         allocate(rtdpth(nsoil))

!   allocate space for common/indx/ and common/indxg/

         allocate(ihe(jdim1:jdim2))
         allocate(ihw(jdim1:jdim2))
         allocate(ive(jdim1:jdim2))
         allocate(ivw(jdim1:jdim2))
         allocate(irad(jdim1:jdim2))
         allocate(iheg(jm))
         allocate(ihwg(jm))
         allocate(iveg(jm))
         allocate(ivwg(jm))
         allocate(iradg(2*im-1))

!   allocate space for common/z0efft/

         allocate(zeffij(idim1:idim2,jdim1:jdim2,4))

!   allocate space for common/tempv/

         allocate(t0(idim1:idim2,jdim1:jdim2,lm))
         allocate(q0(idim1:idim2,jdim1:jdim2,lm))
         allocate(p0(idim1:idim2,jdim1:jdim2))
  
!   allocate space for common/rd1tim/

         allocate(lvl(idim1:idim2,jdim1:jdim2))

!       assign values to some constants in common/rd1tim/

         cthk(1:3)=20000.0
         taucv(1)=.16 ; taucv(2)=.14 ; taucv(3)=.12
         ltop(1:3)=0

!   allocate space for common/PRFHLD/

         allocate(tlmin(im,jm)) ; allocate(tlmax(im,jm))

!   allocate space for common/QFLX/

         allocate(FQU(im,jm))
         allocate(FQV(im,jm))
         allocate(DQFLX(im,jm))
         allocate(FCU(im,jm))
         allocate(FCV(im,jm))
         allocate(DCFLX(im,jm))
         allocate(FQU7(im,jm))
         allocate(FQV7(im,jm))
         allocate(DQFLX7(im,jm))
         allocate(FCU7(im,jm))
         allocate(FCV7(im,jm))
         allocate(DCFLX7(im,jm))
         allocate(DQADV(im,jm))
         allocate(FQNEV1(im,jm))
         allocate(FQSEV1(im,jm))

!   allocate space for common/PPTASM/

         allocate(VAPINC(im,jm))
         allocate(VAPINC7(im,jm))
         allocate(CLDINC(im,jm))
         allocate(CLDINC7(im,jm))

       return
       end subroutine alloc_etages

