      subroutine readet5(rlat,rlontmp,iin,idep,ierr)
c  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     Read the Etopo-5 data set.

      INTEGER*2 idepth(4320)
      INTEGER k
c
      data lastrec / -99 /
c
      save
c
c  calculate record number
c
      irec = nint((90.0-rlat)*12) + 1
      if( irec.lt.1 .or. irec.gt.2160 ) then
        ierr = 1
        return
      endif
c
c  calculate element number
c
      if( rlontmp.lt.0 )then
        rlon = rlontmp + 360.
      else
        rlon = rlontmp
      endif
      jpos = nint(rlon*12) + 1
      if( jpos.eq.4321 )jpos=1
      if( jpos.lt.1 .or. jpos.gt.4320 )then
        ierr = 2
        return
      endif
c
c  read appropriate record
c
      if( irec.ne.lastrec )then
cc
CD        PRINT *,'Reached the read, iin, irec = ',iin, irec
CD        irec = 1
C        read(iin, rec=irec, err=100) (idepth(k),k=1,4320)
        read(iin, REC=irec) (idepth(k),k=1,4320)
        WRITE(*, 9001) (idepth(k),k=1,4320)
 9001   FORMAT (8I8)

        lastrec = irec
cc
      endif
c
      idep = -idepth(jpos)
c
c  rumour has it that the etopo5 bad data flag is 10
c
      if( idep.eq.10 )idep = 0
      ierr = 0
      return
c
c  read error occurred
c
  100 continue
cc
C      write(6,400) iin,irec,lastrec,lastrec
C  400 format(10i8)
C      write(6,400) (idepth(k),k=1,4320)
cc
      ierr = 3
      idep = 0
      return
      end
