c-------------------------------------------------------------------
C      NSIDC routine
c putlab - add a string to the end of the current partial label
c
c input : label - buffer containing PDS label so far
c         labpos - position of last valid byte in buffer
c         string - text to add to end of label buffer
c
c output: label - previous label + string + <CR> + <LF>
c         labpos - updated position in buffer
c
       subroutine putlab (label, labpos, string)
       integer*1 label(*)
       integer labpos
       character*(*) string

       integer strpos

       do 10 strpos = 1, len(string)
         labpos = labpos + 1
         label(labpos) = ichar(string(strpos:strpos))
10     continue

       labpos = labpos + 1
       label(labpos) = 13
       labpos = labpos + 1
       label(labpos) = 10

       return
       end
