        subroutine ncarxyt(corr,corre,inc,n,labl,nll,
     *  loc,nlc,xyt,nx,zz) 
        dimension corr(241),corre(241) 
        character*4 labl,xyt 
        character*7 ilab(3)   
        character*4 irr(5)
        character*6 loc
        character*62 head1
        character*5 head5  
        dimension x1(2),x2(2)   
        data ilab/'   DAYS','DEGREES','DEGREES'/  
c        data irr/'2.00','1.50','1.00','0.50','0.00'/
         data irr/'1.00','0.75','0.50','0.25','0.00'/
c
        save
c
c        CALL OPNGKS
c
c-----------------------------------------------
        call setusv('IN',5)  
c  red
        call stclrs (10000,    0, 1500,10000,ic1)
c  green
        call stclrs (    0,10000,    0,10000,ic2)
c  purple
        call stclrs ( 4000,    0,10000,10000,ic3) 
c light yellow
        call stclrs ( 1500,10000,10000,10000,ic4)
c black
        call stclrs (    0,    0,    0,    0,ic5) 
c-----------------------------------------------
        call setusv('II',ic5)
c
        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c         
        write(6,616) nx,zz
  616   format(1x,' NX',i4,'   ZZ',f7.4)                                                       
c set data     
c        idate = 20090505 
        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
        if(nx.eq.1)
     *  call wtstr (0.35+zz,0.99-zz,
     *  'ANOMALLY LAG CORRELATION     DATE 20090520',  
     *  2,0,0)  
c     *  'ANOMALLY LAG RMS Difference   DATE 20090520',  
c     *  'ANOMALLY LAG RMS Difference   DATE 20090520',  
c        
        if(nx.eq.1)      
     *  call wtstr (0.22+zz,0.96+zz,
     * ' ANOMALY = ANALYSIS - CLIMATOLOGY',1,0,0)  
c
        dinc = 1./float(inc) 

      write(6,61) xyt,loc,labl,dinc,ilab(nx) 
      write(head1,61) xyt,loc,labl,dinc,ilab(nx)   
  61  format(1x,a4,4x,a6,4x,a4,12x,'  INTERVAL ',f6.4,a8)
        if(nx.eq.1) call wtstr(0.35+zz,0.93+zz,head1,1,0,0) 
        if(nx.eq.2) call wtstr(0.35+zz,0.63+zz,head1,1,0,0) 
        if(nx.eq.3) call wtstr(0.35+zz,0.33+zz,head1,1,0,0) 

        if(nx.eq.1) 
     *  call set(0.05,0.95,0.65,0.90,-30.,30.,0.00,1.00,1)    
        if(nx.eq.2) 
     *  call set(0.05,0.95,0.35,0.60,-10.,10.,0.00,1.00,1)    
        if(nx.eq.3)
     *  call set(0.05,0.95,0.05,0.30,-10.,10.,0.00,1.00,1)    

        write(head5,62) irr(1)
  62    format(a5) 
         call wtstr(0.35+zz,0.93+zz,head5,1,0,0) 
        
        write(head5,62) irr(2)
         call wtstr(0.35+zz,0.78+zz,head5,1,0,0) 
        
        write(head5,62) irr(3)
         call wtstr(0.35+zz,0.53+zz,head5,1,0,0) 

        write(head5,62) irr(4)
         call wtstr(0.35+zz,0.28+zz,head5,1,0,0) 

        write(head5,62) irr(5)
         call wtstr(0.35+zz,0.05+zz,head5,1,0,0) 

        xmin = -10.0
        xmax = 10.0
        if(nx.eq.1) xmin=-30.
        if(nx.eq.1) xmax=30.
        nl=2
        x1(1) = xmin + zz     
        x2(1) = 0.0 + zz
        x1(2) = xmin + zz    
        x2(2) = 1.00 - zz
        call curve(x1,x2,nl) 
        x1(1) = xmin + zz
        x2(1) = 1.0 - zz
        x1(2) = xmax - zz
        x2(2) = 1.0 - zz  
        call curve(x1,x2,nl) 
        x1(1) = 0.0 + zz
        x2(1) = 0.0 + zz  
        x1(2) = 0.0 + zz
        x2(2) = 1.0 - zz  
        call curve(x1,x2,nl) 
        x1(1) = xmax - zz  
        x2(1) = 0.0 + zz
        x1(2) = xmax -zz
        x2(2) = 1.0 - zz
        call curve(x1,x2,nl) 
        x1(1) = xmin + zz
        x2(1) = 0.0 + zz
        x1(2) = xmax - zz     
        x2(2) = 0.0 + zz
        call curve(x1,x2,nl) 
        x1(1) = xmin + zz   
        x2(1) = 0.50 + zz
        x1(2) = xmax - zz 
        x2(2) = 0.50 + zz        
        call curve(x1,x2,nl) 
        x1(1) = xmin + zz    
        x2(1) = 0.75 + zz
        x1(2) = xmax - zz 
        x2(2) = 0.75 + zz       
        call curve(x1,x2,nl) 
        x1(1) = xmin + zz    
        x2(1) = 0.25 + zz   
        x1(2) = xmax - zz 
        x2(2) = 0.25 + zz       
        call curve(x1,x2,nl) 
        x1(1) = xmin     
        x2(1) = 0.24    
        x1(2) = xmax  
        x2(2) = 0.24        
        if(nx.eq.3) call curve(x1,x2,nl) 
c
        mid = (n-1)/2 + 1 
        write(6,609) n,inc,mid
  609   format(1x,'n, inc, mid',3i5)    
c
        do 20 i=1,n 
        xx = float(i-mid)/float(inc) 
        if(corr(i).gt.0.99) corr(i) = 0.995  
        yy = corr(i) 
        call points(xx+zz,yy+zz,1,ichar('*'),0)     
        write(6,611) i,xx,yy 
  611   format(1x,'i',i4,'  xx, yy',2f6.2) 
  20    continue     
c
        if(nx.ne.1) go to 51 

        n=13
        mid = (n-1)/2 + 1 
        write(6,609) n,inc,mid
        do 22 i=1,13 
        xx = float(i-mid)*5     
        if(corre(i).gt.0.99) corre(i) = 0.995  
        yy = corre(i) 
        call points(xx+zz,yy+zz,1,ichar('*'),0)     
        write(6,611) i,xx,yy 
  22    continue     
  51    continue

        
c       if(nx.eq.3) call frame 
       write(6,661) 
  661  format(1x,'-------------- END of   PLOT   ')   
c       CALL CLSGKS
c 
       return
       end   
