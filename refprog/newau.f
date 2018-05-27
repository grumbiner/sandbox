      SUBROUTINE newau(au1, au2, au3, title, jou, vol, 
     1                    page1, page2, year, note, nref,
     2                 tau1, tau2, tau3, ttitle, tjou, tvol,
     3                    tpage1, tpage2, tyear, tnote, tnref,
     4                 posit, n)
      
C     Subroutine to insert a new author into the current list.

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen =  24)
      PARAMETER (tlen   = 196)
      PARAMETER (notlen =  16)
      PARAMETER (maxsz  = 4000)
      
C     Declare the main data structures.
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
C     Local and temporary variables.
      CHARACTER*24 tau1, tau2, tau3, tjou
      CHARACTER*196 ttitle
      INTEGER tvol, tpage1, tpage2, tyear, tnref
      CHARACTER*16 tnote
 
      INTEGER i, n, posit
      LOGICAL yes
C***********************************************************----------!!

C       All entries >= posit must be bumped up one, and N increased by 1.
        DO 1000 i = n, posit, -1
          au1(i+1)   = au1(i)
          au2(i+1)   = au2(i)
          au3(i+1)   = au3(i)
          title(i+1) = title(i)
          jou(i+1)   = jou(i)
          vol(i+1)   = vol(i)
          page1(i+1) = page1(i)
          page2(i+1) = page2(i)
          year(i+1)  = year(i)
          note(i+1)  = note(i)
          nref(i+1)  = nref(i)
 1000   CONTINUE
        au1(posit)   = tau1
        au2(posit)   = tau2
        au3(posit)   = tau3
        title(posit) = ttitle
        jou(posit)   = tjou
        vol(posit)   = tvol
        page1(posit) = tpage1
        page2(posit) = tpage2
        year(posit)  = tyear
        note(posit)  = tnote
        nref(posit)  = tnref

        n = n + 1

C***********************************************************----------!!

      RETURN 
      END
