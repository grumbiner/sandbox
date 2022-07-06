        subroutine ncar_xyt(corr,inc,n,labl,nll,loc,nlc,
      *  xyt) 
        dimension corr(241)
        character*4 labl,xyt 
        character*6 loc
        character*50 head1
c
        save
c
        CALL OPNCKS
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

        call setusv('II',ic1)
c
        call set (0.0,1.0,0.0,1.0,0,0,1.0,0,0,1.0,1)
c
        call wtstr (0.20,0.99,' ANOMALLY CORRELATION',
     *  2,0,0)  
c        
        call wtstr (0.20,0.95,' ANOMALY = ANALYSIS -',
     *' CLIMATOLOGY',1,0,0)       
c
       
        write(head,61) xyt,loc,labl
  61    format(a4,4x,a6,4x,a4)
        call wtstr(0.20,91,1,0,0) 



       CALL CLSGKS
c 
       return
       end   
