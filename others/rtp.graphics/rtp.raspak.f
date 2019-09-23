cc---------------Machine dependent drawing routines----------------c      subroutine gwicol(width,kolor)      common /devinf/devdi,devdj,devx,devy      common /wicol/wwidth,iwidth,kkolor,kolorb      common /color/ncol,icol(100)      save /wicol/,/devinf/,/color/      wwidth=width      kkolor=kolor      iwidth=width/devdi      if(iwidth.lt.1) iwidth=1      call toolbx(Z'89B09000',iwidth,iwidth)      kolor=1+mod(kolor-1,ncol)      call toolbx(Z'86210000',icol(kolor))      end              subroutine gvect(x,y,np)      include k2:Fortran:includes:quickdraw.incc     **Note flipped vertical coordinate for Macintosh**      dimension x(1),y(1)      logical box      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      logical scale      common /limit/x1,x2,y1,y2,z1,z2,scale      save /limit/      if(np.lt.2) np1=1      if(np.ge.2) np1=np      if(scale) then        do 10 ipl=1,np1        x(ipl)= xorg + xsize*(x(ipl)-x1)/(x2-x1)10      y(ipl)= yorg + ysize*(y(ipl)-y1)/(y2-y1)      endif      i=idev(x(1))      j=jdev(y(1))      if(np.eq.0) then        call toolbx(MOVETO,i,j)      else if(np.eq.1) then        call toolbx(LINETO,i,j)      else if(np.gt.1) then        call toolbx(MOVETO,i,j)        do 1 ipl=2,np         i=idev(x(ipl))         j=jdev(y(ipl))1       call toolbx(LINETO,i,j)      endif      end            subroutine gdot(x,y,np)      include k2:Fortran:includes:quickdraw.incc     **Note flipped vertical coordinate for Macintosh**      logical box      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      logical scale      common /limit/x1,x2,y1,y2,z1,z2,scale      common /wicol/wwidth,iwidth,kkolor,kolorb      dimension x(1),y(1)      integer*2 rect(4)      save /limit/,/wicol/,/vport/      iiw=iwidth/2      if(iiw.lt.1) iiw=1      if(scale) then        do 10 ipl=1,np        x(ipl)= xorg + xsize*(x(ipl)-x1)/(x2-x1)10      y(ipl)= yorg + ysize*(y(ipl)-y1)/(y2-y1)      endif      do 1 ipl=1,np         i=idev(x(ipl))         j=jdev(y(ipl))                  rect(1)=j-iiw         rect(2)=i-iiw         rect(3)=j+iiw         rect(4)=i+iiw1     call toolbx(paintoval,rect)      end              subroutine rsurf(x,y,np,kolor,frame)        include k2:Fortran:includes:quickdraw.inc        common /color/ncol,icol(100)        common /wicol/wwidth,iwidth,kkolor,kkolorb        logical clset        common /class/zcl(100),nrcl,clset,iop        save /color/,/wicol/,/class/        integer mypoly,toolbx        dimension x(1),y(1)        nnp=np        if((x(1).ne.x(np)).or.(y(1).ne.y(np))) then          x(np+1)=x(1)          y(np+1)=y(1)          nnp=np+1        endif        mypoly=toolbx(openpoly)        call gvect(x,y,nnp)             call toolbx(closepoly)        kolor=1+mod(kolor-1,ncol)        call toolbx(Z'86210000',icol(kolor))        call toolbx(paintpoly,mypoly)        if(frame.gt.0.) then          call gwicol(frame,kkolorb)          call toolbx(framepoly,mypoly)        endif        call toolbx(Z'86210000',icol(kkolor))        call toolbx(killpoly,mypoly)        end              subroutine rsurf4(x,y,z,frame)        include k2:Fortran:includes:quickdraw.inc        common /color/ncol,icol(100)        common /wicol/wwidth,iwidth,kkolor,kkolorb        logical clset        common /class/zcl(100),nrcl,clset,iop        save /color/,/wicol/,/class/        integer mypoly,toolbx        dimension x(4),y(4),z(4)        dimension xx(5),yy(5)        zz=(z(1)+z(2)+z(3)+z(4))/4.        do 1 i=1,4        xx(i)=x(i)1       yy(i)=y(i)        xx(5)=x(1)        yy(5)=y(1)        mypoly=toolbx(openpoly)        call gvect(xx,yy,5)             call toolbx(closepoly)        kolor=1        do 2 i=1,nrcl2       if(zz.gt.zcl(i)) kolor=i        kolor=1+mod(kolor-1,ncol)        call toolbx(Z'86210000',icol(kolor))        call toolbx(paintpoly,mypoly)        if(frame.gt.0.) then          call gwicol(frame,kkolorb)          call toolbx(framepoly,mypoly)        endif        call toolbx(Z'86210000',icol(kkolor))        call toolbx(killpoly,mypoly)        end                subroutine rrect(x1,y1,x2,y2,kolor,frame)        include k2:Fortran:includes:quickdraw.inc        common /color/ncol,icol(100)        common /wicol/wwidth,iwidth,kkolor,kkolorb        logical box        common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box        logical scale        common /limit/xx1,xx2,yy1,yy2,zz1,zz2,scale        save /color/,/wicol/,/limit/,/vport/        integer*2 rect(4)        if(scale) then          x1= xorg + xsize*(x1-xx1)/(xx2-xx1)          y1= yorg + ysize*(y1-yy1)/(yy2-yy1)          x2= xorg + xsize*(x2-xx1)/(xx2-xx1)          y2= yorg + ysize*(y2-yy1)/(yy2-yy1)        endif        i1=idev(x1)        j1=jdev(y1)        i2=idev(x2)        j2=jdev(y2)        rect(1)=min(j1,j2)        rect(2)=min(i1,i2)        rect(3)=max(j1,j2)        rect(4)=max(i1,i2)        kolor=1+mod(kolor-1,ncol)        call toolbx(forecolor,icol(kolor))        call toolbx(paintrect,rect)        if(frame.gt.0.) then          call gwicol(frame,kkolorb)          call toolbx(framerect,rect)        endif        call toolbx(forecolor,icol(kkolor))        end              subroutine rrect4(x1,y1,x2,y2,z,frame)        logical clset        common /class/zcl(100),nrcl,clset,iop        save /class/        dimension z(4)        integer*2 rect(4)        zz=(z(1)+z(2)+z(3)+z(4))/4.        kolor=1        do 2 i=1,nrcl2       if(zz.gt.zcl(i)) kolor=i        call rrect(x1,y1,x2,y2,kolor,frame)        end                  function idev(x)      common /devrec/ii1,jj1,ii2,jj2      common /devinf/devdi,devdj,devx,devy      idev=nint(float(ii1) + x/devdi)            end            function jdev(y)      common /devrec/ii1,jj1,ii2,jj2      common /devinf/devdi,devdj,devx,devy      jdev=nint(float(jj2)-(float(jj1) + y/devdj))      end            subroutine polop      include k2:Fortran:includes:quickdraw.inc      integer mypoly,toolbx      common /polptr/mypoly      save /polptr/c      Opens a polygon object      mypoly=toolbx(openpoly)      call toolbx(showpen)      call toolbx(piccomment,160,0,mypoly)	!160-openpoly      end            subroutine polcl      include k2:Fortran:includes:quickdraw.inc      integer mypoly,toolbx      common /polptr/mypoly      save /polptr/c       Closes a polygon object      call toolbx(piccomment,161,0,mypoly)	!Endpoly      call toolbx(hidepen)      call toolbx(closepoly)      call toolbx(killpoly,mypoly)      end            subroutine polcls      include k2:Fortran:includes:quickdraw.inc      integer mypoly,toolbx      common /polptr/mypoly      save /polptr/** The Piccomment #164 is the Poly-smoothig Piccomment.c       Closes a smoothed polygon object      call toolbx(piccomment,164,1,mypoly)	!smooth it      call toolbx(piccomment,161,0,mypoly)	!Endpoly      call toolbx(hidepen)      call toolbx(closepoly)      call toolbx(killpoly,mypoly)      endc---------------------RASPAK text routines----------------------              subroutine gnumb(fnumbr,x,y,height,idecml)        character*256 buf1       format(i24)2       format(F24.0)3       format(F24.1)4       format(F24.2)5       format(F24.3)6       format(F24.4)        if(idecml.gt.4) idecml=4        if(idecml.lt.0) then        inumbr=fnumbr        write(buf,1) inumbr        else if(idecml.eq.0) then        write(buf,2) fnumbr        else if(idecml.eq.1) then        write(buf,3) fnumbr        else if(idecml.eq.2) then        write(buf,4) fnumbr        else if(idecml.eq.3) then        write(buf,5) fnumbr        else if(idecml.eq.4) then        write(buf,6) fnumbr        endifc       trim leading and trailing blanks        do 10 i=1,256        ii=i10      if(buf(i:i).ne.' ') go to 1111      continue        do 12 i=1,256        if(i.le.(256-ii+1)) then          buf(i:i)=buf((i+ii-1):(i+ii-1))        else          buf(i:i)=' '        endif12      continue        buf=trim(buf)        call gchar(buf,x,y,height)        end                subroutine gcharf(ifont)        include k2:Fortran:includes:quickdraw.inc        call toolbx(textfont,ifont)        call toolbx(textface,1)        end                subroutine gchara(iangin)        common /texcom/just,kolor,iangle        save /texcom/        iangle=iangin        end        subroutine gcharj(justin)        common /texcom/just,kolor,iangle        save /texcom/        just=justin        end        subroutine gchar(text,x,y,height)        include k2:Fortran:includes:quickdraw.inc        character*(*) text        character*256 str,buf        common /texcom/just,kolor,iangle        common /devinf/devdi,devdj,devx,devy        integer*2 fontinfo(4)        integer toolbx        dimension ifsize(4)                save /texcom/,/devinf/        nfsize=4        ifsize(1)=9;ifsize(2)=12;ifsize(3)=18;ifsize(4)=24        do 1 i=1,nfsize        call toolbx(textsize,ifsize(i))        call toolbx(getfontinfo,fontinfo)1       if(devdj*float(fontinfo(1)).ge.height) go to 22       height=devdj*float(fontinfo(1))        buf=str(text)        istring=toolbx(stringwidth,buf)        call gvect(x,y,0)c       shift position to allow for justification        if((just.eq.0).or.(just.eq.1).or.(just.eq.2)) then          idy=0        else if((just.eq.3).or.(just.eq.4).or.(just.eq.5)) then          idy=fontinfo(1)/2        else if((just.eq.6).or.(just.eq.7).or.(just.eq.8)) then          idy=fontinfo(1)        endif        if((just.eq.0).or.(just.eq.3).or.(just.eq.6)) then          idx=0        else if((just.eq.1).or.(just.eq.4).or.(just.eq.7)) then          idx=-istring/2        else if((just.eq.2).or.(just.eq.5).or.(just.eq.8)) then          idx=-istring        endif        call toolbx(move,idx,idy)                call toolbx(drawstring,buf)                        end c---------------RASPAK routines-----------------------------------      subroutine lmac      common /devrec/ii1,jj1,ii2,jj2      common /devinf/devdi,devdj,devx,devy      common /color/ncol,icol(100)      save /devrec/,/devinf/,/color/      ii1=1;jj1=1;ii2=850;jj2=850c     Pixel width in millimeters      devdi=0.3472222222      devdj=0.3472222222      devx=devdi*abs(float(ii2-ii1))      devy=devdj*abs(float(jj2-jj1))c     color information      ncol=8      icol(1)=33      icol(2)=409      icol(3)=273      icol(4)=341      icol(5)=137      icol(6)=205      icol(7)=69      icol(8)=30      end            subroutine gopen      include k2:Fortran:includes:quickdraw.inc      logical scale,box      integer*2 rect(4)      integer mypict,toolbx      common /advcom/iadv      common /picptr/mypict      common /devrec/ii1,jj1,ii2,jj2      common /limit/x1,x2,y1,y2,z1,z2,scale      common /devinf/devdi,devdj,devx,devy      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      save /limit/,/devinf/,/vport/,/devrec/,/picptr/,/advcom/      scale=.false.      box=.false.      iadv=0c         Initialize viewport to allow room for title and labelsc         (GEOPAK will override this)      xorg=30.      yorg=30.      xsize=devx-60.      ysize=devy-60.      call gwicol(0.,1)      call gsurfr(1)c     Start Macintosh picture      call toolbx(SETRECT, rect,ii1,jj1,ii2,jj2)      mypict= toolbx(openpicture, rect)      call toolbx(piccomment,130,0,mypict)	!BeginPictc     Initialize geopak parameters      call geinit      end                  subroutine geinitc     initializes geopak, sets default constants      logical clset      integer work      common /class/zcl(100),nrcl,clset,iop      common /wspace/work(5000)      common /ginfo/undef      save /class/,/wspace/,/ginfo/      clset=.false.      nrcl=9      undef=1.e30      iop=2      end	  	   subroutine gzcl(zclin,nrclin,iopin)      dimension zclin(1)      logical clset      common /class/zcl(100),nrcl,clset,iop      save /class/      iop=iopin      nrcl=nrclin      if(iop.eq.0) then            do 1 i=1,nrcl1           zcl(i)=zclin(i)            clset=.true.      else if(iop.eq.1) then            do 2 i=1,nrcl2           zcl(i)=zclin(i)      else if(iop.eq.3) then            do 4 i=1,nrcl            dz=(zclin(nrcl)-zclin(1))/float(nrcl-1)4           zcl(i)=zclin(1) + dz*float(i-1)            clset=.true.            endif            if((iop.lt.0).or.(iop.eq.1)) then            write(*,*) 'unimplemented feature in gzcl'            stop      endif      end            subroutine gclose      include k2:Fortran:includes:quickdraw.inc      integer mypict      integer*2 rect(4),vref      character*64 fname      logical stdfil,box      common /devrec/ii1,jj1,ii2,jj2      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      common /picptr/mypict      call toolbx(piccomment,131,0,mypict)	!Endpict      call toolbx(closepicture)c     Put picture into current grafport      call getrect(rect)            call toolbx(eraserect, rect)c     adjust rect to preserve aspect ratio      ratdev=float(abs(jj2-jj1))/float(abs(ii2-ii1))      ratscr=float(abs(rect(3)-rect(1)))/float(abs(rect(4)-rect(2)))         xshr=ratscr/ratdev         yshr=ratdev/ratscr		  if(xshr.lt.1.) then           xbs=xshr           ybs=1.		  else		     xbs=1.		     ybs=yshr		  endif         rect(3)=rect(1)+ybs*abs(rect(3)-rect(1))         rect(4)=rect(2)+xbs*abs(rect(4)-rect(2))      call toolbx(drawpicture, mypict, rect)	     c     Save picture to file (interactively or in batch)      pause      fname='contour.out'      if (stdfil(2,vref,' ',fname,1,'PICT')) then        call picsav(mypict,fname)      endifc     Free up stack space, unless picture is to be kept      call toolbx(killpicture, mypict)      end            subroutine gact(iop)      common /advcom/iadv      save /ginfo/      iadv=iop      end            subroutine gsurfr(kolorb)      common /wicol/wwidth,iwidth,kkolor,kkolorb      common /color/ncol,icol(100)      save /wicol/,/color/      kkolorb=kolorb      kolorb=1+mod(kolorb-1,ncol)      end                  subroutine glimit(xmin,xmax,ymin,ymax,zmin,zmax)      logical scale      common /advcom/iadv      common /limit/x1,x2,y1,y2,z1,z2,scale      save /limit/,/advcom/      if(iadv.eq.-1) then        xmin=x1        xmax=x2        ymin=y1        ymax=y2        zmin=z1        zmax=z2      else        x1=xmin        x2=xmax        y1=ymin        y2=ymax        z1=zmin        z2=zmax      endif      iadv=0      end            subroutine gscale      logical scale      common /limit/x1,x2,y1,y2,z1,z2,scale      save /limit/      scale=.true.      end            subroutine gscamm      logical scale      common /limit/x1,x2,y1,y2,z1,z2,scale      save /limit/      scale=.false.      end            subroutine gvport(xorg1,yorg1,xsize1,ysize1)      logical box      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      common /advcom/iadv      save /vport/,/advcom/      if(iadv=-1) then        xorg1=xorg        yorg1=yorg        xsize1=xsize        ysize1=ysize      else        xorg=xorg1        yorg=yorg1        xsize=xsize1        ysize=ysize1      endif      iadv=0      end            subroutine gwbox(xleng,yleng,zleng)      logical box      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      common /advcom/iadv      save /vport/,/advcom/      if(iadv=-1) then        xleng=xb        yleng=yb        zleng=zb        return      endif      iadv=0      box=.true.      xb=xleng      yb=yleng      zb=zlengc     Adjust viewport to preserve aspect ratioc     Call after call to vport      fac=xb      if(yb.gt.fac) fac=yb      xb=xb/fac      yb=yb/fac      xsize=xsize*xb      ysize=ysize*yb      end           subroutine vframec     utility to draw frame around viewport      logical box,scale,scale1      common /vport/xorg,yorg,xsize,ysize,xb,yb,zb,box      common /limit/x1,x2,y1,y2,z1,z2,scale      common /wicol/width,iwidth,kolor,kolorb      dimension x(5),y(5)      x(1)=xorg      y(1)=yorg      x(2)=xorg      y(2)=yorg+ysize      x(3)=xorg+xsize      y(3)=yorg+ysize      x(4)=xorg+xsize      y(4)=yorg      x(5)=xorg      y(5)=yorg      scale1=scale      scale=.false.      width1=width      kolor1=kolor      call gwicol(1.,1)      call polop      call gvect(x,y,5)      call polcl      call gwicol(width1,kolor1)      scale=scale1      endc---------------Auxiliary computation routines--------------------        function vmax(v,n)        real v(n)        vmax=-1.e30        do(i=1,n)        if(v(i).gt.vmax) vmax=v(i)        repeat        end                       function vmin(v,n)        real v(n)        vmin=1.e30        do(i=1,n)        if(v(i).lt.vmin) vmin=v(i)        repeat        end c------------Macintosh specific auxiliary routines        subroutine getrect(rect)        implicit none        include k2:Fortran:includes:quickdraw.inc        include k2:Fortran:includes:Grafport.inc        integer*2 rect(4)        integer ii1,jj1,ii2,jj2        integer grafptr        integer toolbx        call toolbx(GETPORT,grafptr)        jj1=WORD((grafptr+portrect))        ii1=WORD((grafptr+portrect)+2)        jj2=WORD((grafptr+portrect)+4)        ii2=WORD((grafptr+portrect)+6)        rect(1)=jj1;rect(2)=ii1;rect(3)=jj2;rect(4)=ii2        end                                character*256 function str(buf)        character*(*) buf        str=char(len(trim(buf)))//buf        end