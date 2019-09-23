c

       subroutine maklab(label,labsiz,nlines,pixbits,reclen)

       integer*1 label(2240)

       integer labsiz

       integer nlines

       integer pixbits, reclen

       integer ipos, labpos, labrecs

       character*48 labstr

c

c          print*,' dividing by reclen '

       labrecs = 2047/reclen + 1

       labsiz = labrecs * reclen

       labpos = 0

       write (labstr, 101) (labrecs + nlines)*reclen - 20

101   format ('NJPL1I00PDS1',i8.8,'            = SFDU_LABEL')

       call putlab (label, labpos, labstr)

       labstr ='RECORD_TYPE                     = FIXED_LENGTH'

       call putlab (label, labpos, labstr)

       write (labstr, 102) reclen

102   format ('RECORD_BYTES                    = ',i8)

       call putlab (label, labpos, labstr)

       write (labstr, 103) labrecs + nlines

103   format ('FILE_RECORDS                    = ',i8)

       call putlab (label, labpos, labstr)

       write (labstr, 104) labrecs

104   format ('LABEL_RECORDS                   = ',i8)

       call putlab (label, labpos, labstr)

       write (labstr, 105) labrecs + 1

105   format ('^IMAGE                          = ',i8)

       call putlab (label, labpos, labstr)

       labstr ='OBJECT                          = IMAGE'

       call putlab (label, labpos, labstr)

       write (labstr, 106) nlines

106   format ('  LINES                         = ',i8)

       call putlab (label, labpos, labstr)

       write (labstr, 107) reclen

107   format ('  LINE_SAMPLES                  = ',i8)

       call putlab (label, labpos, labstr)

        write (labstr, 108) pixbits

108   format ('  SAMPLE_BITS                   = ',i8)

        call putlab (label, labpos, labstr)

        call putlab (label, labpos, 'END_OBJECT')

        call putlab (label, labpos, 'END')

c         print*,' labpos, labsiz ',labpos,labsiz

c         print*,' print label '

c         print*,label

        do 10 ipos = labpos, labsiz

           label(ipos)=ichar(' ')

10      continue

	return

	end     

