c-------------------------------------------------------------------
c added by KWK 4/30/90 
c
c maklab - make PDS label
c
c input : nlines - number of lines in the output image
c         numpar - number of pixels per line in the output image
c         pixbits - number of bits per pixel in the output image
c         reclen - number of bytes in each record of the output image
c
c output: label - buffer containing PDS label
c         labsiz - number of bytes in PDS label
c                  = 2047 rounded up to a whole number of records
c
       subroutine maklab (label,labsiz,nlines,numpar,pixbits,reclen)
       integer*1 label(*)
       integer labsiz
       integer*2 nlines, numpar
       integer pixbits, reclen

       integer ipos, labpos, labrecs
       character*48 labstr

       labrecs = 2047/reclen + 1
       labsiz = labrecs * reclen
       labpos = 0

       write (labstr, 1010) (labrecs + nlines)*reclen - 20
1010   format ('NJPL1I00PDS1',i8.8,'            = SFDU_LABEL')
       call putlab (label, labpos, labstr)

       labstr ='RECORD_TYPE                     = FIXED_LENGTH'
       call putlab (label, labpos, labstr)

       write (labstr, 1020) reclen
1020   format ('RECORD_BYTES                    = ',i8)
       call putlab (label, labpos, labstr)

       write (labstr, 1030) labrecs + nlines
1030   format ('FILE_RECORDS                    = ',i8)
       call putlab (label, labpos, labstr)

       write (labstr, 1040) labrecs
1040   format ('LABEL_RECORDS                   = ',i8)
       call putlab (label, labpos, labstr)

       write (labstr, 1050) labrecs + 1
1050   format ('^IMAGE                          = ',i8)
       call putlab (label, labpos, labstr)

       labstr ='OBJECT                          = IMAGE'
       call putlab (label, labpos, labstr)

       write (labstr, 1060) nlines
1060   format ('  LINES                         = ',i8)
       call putlab (label, labpos, labstr)

       write (labstr, 1070) numpar
1070   format ('  LINE_SAMPLES                  = ',i8)
       call putlab (label, labpos, labstr)

       write (labstr, 1080) pixbits
1080   format ('  SAMPLE_BITS                   = ',i8)
       call putlab (label, labpos, labstr)

       call putlab (label, labpos, 'END_OBJECT')

       call putlab (label, labpos, 'END')

       do 10 ipos = labpos, labsiz
         label(ipos) = ichar(' ')
10     continue

       return
       end
