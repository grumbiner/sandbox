*
*  clrtty: clear the TTY window and postion the pen at 3,12 (v,h)
*
	SUBROUTINE clrtty

        INTEGER ERASERECT
        PARAMETER (ERASERECT=Z'8A330000')
        INTEGER MOVETO
        PARAMETER (MOVETO=Z'89309000')
	
	INTEGER*2 rect(4)
	
	data rect /1,1,479,639/
	
	call toolbx(ERASERECT,rect)
	call toolbx(MOVETO,3,12)
	
	end
*
* This routine force the compiler to expand the heap by 2k bytes so that
* the spool SUBROUTINE can be loaded by the print command.
* If this heap space is no allocated in this manner or with the linker
* the program will give an error 64 (out of memory) when the print option is
* selected from the menu.
*
	SUBROUTINE dum
	INTEGER*2 ary(1000)
	common ary
	return
	end
	
* Absoft random number generator
	function rnd()
	integer seed
	save seed
	data seed /1234/
	seed = seed*125
	seed = seed - (seed/2796203) * 2796203
	rnd = seed/2796203.0
	end
*
*  str255: converts a FORTRAN string to a Pascal LSTRING
*

	character*256 function str255(string)

	character*(*) string
	
	str255 = char(len(trim(string)))//string
	end