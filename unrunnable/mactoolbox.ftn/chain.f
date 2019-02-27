
*
*  chain: transfer control to specified application (the application had
*	  better be executable)
*

	SUBROUTINE chain
	
	IMPLICIT none

	character*64 filename		! name of file to execute
	
	
	call getfil('APPL', filename)
	if (filename = " ") return
	
 	execute trim(filename)
	 
	end
*
*  getfile: prompt user for file name and look it up
*

	SUBROUTINE getfil(ftype, filename)
	
	IMPLICIT none
	
	INCLUDE HD.I40:Macfortran020:demobg.inc
	
	INTEGER*4 toolbx
	
	INTEGER*1 reply(76)
	INTEGER*2 good
	character*4 ostype
	INTEGER*2 vrefnum
	INTEGER*2 version
	character*64 fname
	INTEGER flength
	character*64 filename

	equivalence (reply(1), good)
	equivalence (reply(3), ostype)
	equivalence (reply(7), vrefnum)
	equivalence (reply(9), version)
	equivalence (reply(11), fname)
	
	INTEGER*4 where
	INTEGER*2 wh(2)
	equivalence (wh(1), where)
	
	LOGICAL exist
	
* File parameter block.  Used to set the default volume in response to a
* Standard File call.
	INTEGER*1 params(80)		! Enough for all variants.
	INTEGER   iolink
	INTEGER*2 iotype
	INTEGER*2 iotrap
	INTEGER   iocmdaddr
	INTEGER   iocompletion
	INTEGER*2 ioresult
	INTEGER   ionameptr
	INTEGER*2 iovrefnum
	
	equivalence (params(1), iolink)
	equivalence (params(5), iotype)
	equivalence (params(7), iotrap)
	equivalence (params(9), iocmdaddr)
	equivalence (params(13), iocompletion)
	equivalence (params(17), ioresult)
	equivalence (params(19), ionameptr)
	equivalence (params(23), iovrefnum)
	
	INTEGER filerr
	
	wh(1) = 50
	wh(2) = 50

	call toolbx(SFGETFILE, where, 0, 0, 1, 
     +	  toolbx(PTR, ftype), 0, reply, 2)

	if (good = 0) then
	  filename = ' '
	  return
	endif
	flength = ichar(fname(1:1))
	filename = fname(2:flength+1)
	ionameptr = 0
	iovrefnum = vrefnum
	filerr = toolbx(PBSETVOL, 
     +	  toolbx(PTR, params))
	end
	
