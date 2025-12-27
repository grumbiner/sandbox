    real ::z(5)
    integer   gridNum, iVerf
    character verfTime*12,verfTime2*11
    character hr*2,year*4,mon1*3,mon2*2,day*2
    character A1*14,A2*16,valName*8

    open (11,file='lshtmp.dat',status='old')
    open (21,file='rrvsgr', status='replace')

    do i=1,4
      read(11,*,iostat=inIostatus)  valName
      if(inIostatus== 145) exit
      do iVerf=1,2
        read(11,*) verfTime
        hr=verfTime(1:2)
        day=verfTime(4:5)
        mon1=verfTime(6:8)
        year=verfTime(9:12)
        if(mon1== 'JAN') mon2='01'
        if(mon1== 'FEB') mon2='02'
        if(mon1== 'MAR') mon2='03'
        if(mon1== 'APR') mon2='04'
        if(mon1== 'MAY') mon2='05'
        if(mon1== 'JUN') mon2='06'
        if(mon1== 'JUL') mon2='07'
        if(mon1== 'AUG') mon2='08'
        if(mon1== 'SEP') mon2='09'
        if(mon1== 'OCT') mon2='10'
        if(mon1== 'NOV') mon2='11'
        if(mon1== 'DEC') mon2='12'
        verfTime2=year//mon2//day//hr
        read(11,*)  gridNum
        read(11,*) (z(j),j=1,5)
        A1='R01 ETA/221 03'
        A2='GREAN2 G98 SL1L2'
        write(21,100) A1,verfTime2, A2,valName, '=',gridNum, (z(j),j=1,5)
100     format(1x,a,1x,a,1x,a,1x,a,1x,a,1x,i4,5e18.9)
      enddo
    enddo
        close(11)
        close(21)
    END

