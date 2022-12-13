       program getssmi                            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c                 
c GETSSMI - Extracts a 16-bit image file from the archive file                 
c           located on CD-ROM. The image can then be displayed                 
c           with the PDS Image Software, IMDISP.                 
c                 
c   Inputs -                 
c                 
c      USER-DEFINED ++++++++++++++++++++++++++++++++++++++++++++++++    
c   
c           indate  : Date in the format YYMMDD (eg., 870710)   
c                     From this variable the CD-ROM is searched for the   
c                     appropriate filename. The filename for the Tb grid   
c                     is derived from this variable.   
c              
c           drive   : CD reader drive specification. For example, the   
c                     drive specification default is L:. User may have    
c                     configured CD reader with a different drive    
c                     specification (eg., F:).   
c       
c           outfile : File path, name, and descriptor string                 
c                     of the 16-bit raster file to be created                 
c                     on magnetic disk (Floppy or HardDrive)                 
c                 
c           ipos    : Position in the cell of parameter to extract        
c                 
c           type    : User selection for either No Mask (0),          
c                     Landmask (1)  or Coastline (2).        
c        
c           mval    : User defined multiplier for masks. It is         
c                     recommended that the multiplier be applied to        
c                     the landmask if the raster file is to be         
c                     displayed with the PDS software (see PDS         
c                     documentation regarding the function    
c                     'SET DNHI').        
c        
c           answer  : User answer to "write PDS label?" query
c                     'Y' or 'y' = yes, write PDS label to output file
c                     anything else = no, do not write label 
c        
c      INTERNAL     ++++++++++++++++++++++++++++++++++++++++++++++++        
c
c           yr      : Two character descriptor for year derived from
c                     user input, indate.
c 
c           month   : Two character descriptor for the month derived from
c                     user input, indate.
c
c           day     : Two character descriptor for the day of the month
c                     derived from user input, indate.
c
c           mon     : Array of 12 elements. Each element is a three
c                     character descriptor representing the first three
c                     characters of a month. This data structure is ordered
c                     'JAN' ... 'DEC'.
c
c           infile  : File path, name, and descriptor string                 
c                     of the input archive file residing on CD-ROM                
c         
c                     For example, 'L:\N3B\1979\MAR\870710.N3B' is    
c                     a path, filename, and descriptor string for    
c                     an archive file residing on CD-ROM. This file    
c                     contains SSM/I Tb gridded data for the data-day    
c                     July 10, 1987.    
c
c           grdtyp  : File extension describing the type of grid
c                     N3A, N3B, S3A, S3B
c                 
c           indat   : 3160-byte buffer containing all parameters in       
c                     an SSM/I Tb gridline. There are 4 SSM/I grid types.
c                     The S3B gridline contains the maximum number of cells:
c                     316 cells * 5 parameters * 2 bytes ==> 3160 bytes.                  
c                     This buffer is defined as a 2-byte integer buffer.
c
c                     There are four buffers associated with indat. These
c                     buffers are IN_N3A, IN_N3B, IN_S3A, IN_S3B. Each 
c                     buffer is equivalence to indat to support block IO
c                     which makes the IO more efficient.
c
c           mskdat  : 632-byte buffer containing mask data for a        
c                     SSM/I N3A, S3A, N3B, or S3B masks (Landmask or 
c                     Coastline). A zero value represents open water 
c                     in the case of the Landmask. A one (1) represents 
c                     both land and water in the case of the Coastline.         
c        
c                     There are four buffers associated with mskdat. These
c                     buffers are MSKN3A, MSKN3B, MSKS3A, MSKS3B. Each
c                     buffer is equivalenced to indat to support block IO
c                     which makes the IO more efficient.
c
c            lrec   : The input record length in bytes. This constant 
c                     is dependent upon the grid type requested.
c
c            lblk   : The input block size in bytes. This constant is
c                     dependent upon the grid type requested and is used
c                     when opening the IO channel to the grid on CD-ROM.
c
c            nlines : The number of lines in the requested grid. This 
c                     constant is dependent upon the grid type requested.
c
c            npars  : The number of parameters contained in a grid. For the
c                     N3A and S3A grids npars is 2; for the N3B and S3B grids
c                     npars is 5.
c
c            numint : The number of two-byte integers in an input buffer.
c
c            numpar : The number of pixels in a grid line which refer to a
c                     single parameter. Numpar is equal to the number of
c                     pixels (bytes) per line in a mask or coastline image
c                     file. Numpar is the number of 2-byte pixels/line in the 
c                     extracted SSM/I image (grid of a single parameter).
c
c            label  : Buffer used to construct PDS label with proper
c                     image parameters.  A PDS label on the output file
c                     allows IMDISP to automatically initialize image
c                     parameters such as, number of lines, number of 
c                     samples and bits per pixel.
c
c            labsiz : Number of bytes in PDS label.
c
c
c   Outputs -                  
c                            
c      INTERNAL     ++++++++++++++++++++++++++++++++++++++++++++++++    
c                 
c            outdat : 1264-byte buffer containing only the parameter    
c                     requested by user (ipos). The S3A gridline contains
c                     the maximum number of pixels: 632 (times 2 bytes).
c                     This buffer is defined as a 2-byte integer buffer.
c                     The 16-bit representation requires a scaling factor 
c                     of (0.1).       
c         
c                     This process does not apply the scaling factor;        
c                     extracted data values will be retained as stored       
c                     in the archive files.                 
c                 
c                     For example, the extracted value of 2436 is the      
c                     stored representation of 243.6 degrees Kelvin.      
c      
c                     There are four buffers associated with outdat. These
c                     buffers are N3AOUT, N3BOUT, S3AOUT, S3BOUT. Each 
c                     buffer is equivalenced to outdat to support block IO
c                     which makes the IO more efficient.
c  Vince Troisi                     
c     
c  Modifications   
c  12/27/89           Version 1.0   Revise for SSM/I CD-ROMs
c   1/28/90           Version 1.1   Increase the size of the IO buffers
c                                   to enhance throughput from CD reader.
c                                   Blocksize has been increased by a 
c                                   factor of 40.
c                                   Create buffers for each grid type to
c                                   support block IO. 
c  Ken Knowles (KWK)
c  4/30/90            Version 1.2   Added option to write a PDS label 
c                                   on the output file.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       integer*2 indat(1580)
       integer*2 in_n3a(1216),in_n3b(1520),in_s3a(1264),in_s3b(1580)
       equivalence (indat,in_n3a)
       equivalence (indat,in_n3b)
       equivalence (indat,in_s3a)
       equivalence (indat,in_s3b)                          
       integer*2 outdat(632)     
       integer*2 outn3a(608),outn3b(304),outs3a(632),outs3b(316)
       equivalence (outdat,outn3a)
       equivalence (outdat,outn3b)
       equivalence (outdat,outs3a)
       equivalence (outdat,outs3b)                   
c   
       character * 2 drive
       character * 6 indate                 
       character * 3 grdtyp
       character * 50 infile                    
       character * 50 outfile                
       character * 2  year, month, day   
       character * 3 mon (12)  
       CHARACTER*1 dirchr  ! Added by BG 10-24-91.
   
c           
       integer * 1 mskdat(632)     
       integer * 1 mskn3a(608),mskn3b(304),msks3a(632),msks3b(316)
       equivalence (mskdat,mskn3a)
       equivalence (mskdat,mskn3b)
       equivalence (mskdat,msks3a)
       equivalence (mskdat,msks3b)       
       integer * 2 type, mval, in , iout, ihem
       integer * 2 lrec, lblk, nlines, npars, numint, numpar                       

c added by KWK 4/30/90   
       character*1 answer
       integer*1 label(2528)
       integer labsiz

       data mon   
     +   /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',   
     +    'OCT','NOV','DEC'/   
               
c            
c Begin Query            
c            
       print *,'Extract data from SSM/I archive file on CD-ROM to '              
       print *,'a 16-bit raster file which can be displayed with'              
       print *,'the NASA PDS Software Package, IMDISP'              
       print *,' '            
c   
       print *,' '   
       print *,'Enter CD reader drive specification (eg., L:) :'    
       read (*,'(a)') drive              
c
c Query user for parameter to extract
c           
       print *,               
     + 'Brightness temperature values are stored in SSM/I grid cells'    
c       
       print *,'as follows:'                 
       print *,' '                
       print *,              
     + '  1     2     3     4     5     6     7    '              
       print *,               
     + ' ---   ---   ---   ---   ---   ---   ---   '              
       print *,               
     + ' 19V , 19H , 22V , 37V , 37H , 85V , 85H '
       print *,' '                 
       print *,'Enter position of parameter to extract (1-7):'                   
     
       read *,ipos                 
c
c Query user to select one Hemisphere
c
       print *,' '            
       print *,'Select one of the polar regions:'
       print *,'1. North'
       print *,'2. South'
       print *,'Enter 1 or 2:'   
       read *,ihem
c
c Query user for date of data to extract
c
       print *,' '
       print *,'Enter date of SSM/I Tb archive file to open (YYMMDD):'
       read (*,'(a)') indate   
c   
c    Combine responses to form filename and search path   
c    and define IO constants
c          
       if (ihem.eq.1) then
          if (ipos.ge.6) then
             grdtyp = 'N3A'
             lrec   = 2432
             lblk   = 2432
             nlines =  896
             npars  =    2
c
c    Adjust position to 1 or 2 for N3A grids
c
             ipos = ipos - 5
          else
             grdtyp = 'N3B'
             lrec   = 3040
             lblk   = 3040
             nlines =  448
             npars  =    5
          endif
       else
          if (ipos.ge.6) then
             grdtyp = 'S3A'
             lrec   = 2528
             lblk   = 2528
             nlines =  664
             npars  =    2
c
c      Adjust position to 1 or 2 for S3A grids
c
             ipos = ipos - 5
          else
             grdtyp = 'S3B'
             lrec   = 3160
             lblk   = 3160
             nlines =  332
             npars  =    5
          endif
       endif
c
c    Define remainder of IO variable in terms of previously 
c    defined IO constants
c
       numint = lrec/2
       numpar = numint/npars
c
c  Create the filename and path
c
C     Dirchr added for system compatibility
       PRINT *,'What is the directory seperator / or \?'
       READ (*,9001) dirchr
 9001 FORMAT (A1)
       read (indate(3:4),'(i2)') mm   
       infile=drive//dirchr//grdtyp//dirchr//"19"//indate(1:2)
     +                  //dirchr//mon(mm)//   
     +         dirchr//indate(1:6)//"."//grdtyp   
c
c  Open primary input and output files
c
C      form changed from binary, to unformatted in open statements
C      ' changed to "
C      Blocksize statements removed,
C      Mode='Read' replaced with action="read" (both are non-standard)
C      BG 10-24-91
       open (unit=12,file=infile,form="binary",                            
CBG     +      blocksize=lblk*40,recl=lrec,mode="read")
     +      recl=lrec,action="read")
c
c
c                            
       print *, 'Enter image file to create:'                    
       read (*,'(a)') outfile                  
       open (unit=14,file=outfile,form="unformatted",                        
CBG     +      blocksize=numpar*40,recl=numpar,mode="write")                                
     +      recl=lrec,action="write")
          
c
c Query user for masking option
c
 888   continue
         print *,'Select a mask option:'
         print *,' 0) No Mask   '
         print *,' 1) Landmask  '          
         print *,' 2) Coastlines  '          
         print *,':'            
         read *,type        
    
       if(type.lt.0) go to 888            
       if(type.ge.3) go to 888            

       print *,' '            
       if (type.ne.0)then
         print *,'Enter value for mask (1-500 is suggested):'            
         read *,mval            
         print *,' '       
c            
c Open masking files
c                    
         if (type.eq.1) then            
           open(unit=10,file=drive//dirchr//"TOOLS"//dirchr//
     +                       grdtyp//"MASK.DAT",            
CBG     +      form="unformatted",blocksize=numpar*40,recl=numpar,
CBG     +      mode="read")            
     +      form="unformatted",recl=numpar,
     +      action="read")            
         else             
           if (type.eq.2) then            
             open(unit=10,file=drive//dirchr//"TOOLS"//dirchr//
     +                        grdtyp//"COAST.DAT",            
CBG     +        form="unformatted",blocksize=numpar*40,recl=numpar,
CBG     +        mode="read")            
     +      form="unformatted",recl=numpar,
     +      action="read")            
           endif            
         endif            
        endif             

c
c Query user for PDS label option  -  added by KWK 4/30/90
c
       print *, 'Write PDS label? Enter Y or N :'                    
       read (*,'(a)') answer
       if (answer.eq.'Y' .or. answer.eq.'y') then
         call maklab (label,labsiz,nlines,numpar,16,2*numpar)
         write (14) (label(iout), iout=1,labsiz)
       endif
c  
c            
c Get ssm/i grid for requested channel                      
c =============================================================          
c  
c            
         do 100 l=1,nlines   
           ip=0
           if(grdtyp.eq.'N3A') read(12)in_n3a
           if(grdtyp.eq.'N3B') read(12)in_n3b
           if(grdtyp.eq.'S3A') read(12)in_s3a
           if(grdtyp.eq.'S3B') read(12)in_s3b                           
           do 50 k=ipos,numint,npars           
             ip=ip+1                    
             outdat(ip)=indat(k)                      
 50        continue            
c           
c            
c Process masking option            
c            
           if (type.ne.0) then            
            if(grdtyp.eq.'N3A') read(10)mskn3a
            if(grdtyp.eq.'N3B') read(10)mskn3b
            if(grdtyp.eq.'S3A') read(10)msks3a
            if(grdtyp.eq.'S3B') read(10)msks3b
           
            do 60 mm=1,numpar
              if(mskdat(mm).ne.0) outdat(mm) = mskdat(mm)*mval            
 60         continue            
           endif                    
c            
c Write buffer to output            
c                         
           if(grdtyp.eq.'N3A') write(14)outn3a
           if(grdtyp.eq.'N3B') write(14)outn3b
           if(grdtyp.eq.'S3A') write(14)outs3a
           if(grdtyp.eq.'S3B') write(14)outs3b
c                                
100      continue                      
c            
c =============================================================          
c                  
         if (type.ne.0) close (10)            
         close (12)                        
         close (14)                            
         end

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

c-------------------------------------------------------------------
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
