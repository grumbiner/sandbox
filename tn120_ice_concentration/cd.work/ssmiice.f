      program ssmiice          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c      
c SSMIICE - This program computes ice concentrations from SSMI TB      
c           grids residing on CD-ROM.      
c      
c    Inputs      
c      
c      USER DEFINED +++++++++++++++++++++++++++++++++++++++++++++++++   
c
c
c        drive   :  2 character drive specification of CD reader      
c 
c        indate  :  Date in the format YYMMDD (eg., 870710)      
c                   From this variable the CD-ROM is searched for the      
c                   appropriate filename. The filename for the ice      
c                   concentration algorithm is derived from this       
c                   variable.      
c
c        ihem    :  Defines the appropriate hemisphere to apply this
c                   algorithm (1=North, 2=South)
c
c        icepar  :  Defines the type of ice concentrations to derive:
c                   1=total, 2=multi-year, 3=first-year.
c      
c      INTERNAL     +++++++++++++++++++++++++++++++++++++++++++++++++    
c      
c        infile  :  The path, name, and descriptor string describing      
c                   the data file from which the SSMI Tb data will be      
c                   extracted from the CD-ROM. Software assumes CD-ROM   
c                   is located on drive L:.      
c      
c        buff    :  Input buffer containing the SSMI Tb data read from      
c                   the CD-ROM. Two lines of SSMI TB grid data will      
c                   read with a single IO request.      
c       
c        mask    :  Landmask used in computing the 50km       
c                   ice concentration grid. This file (N3CMSK.DAT or 
c                   S3CMSK.DAT) should reside in a directory on C: 
c                   drive in a subdirectory \TOOLS.      
c
c        idim1   :  Number of parameters in an SSM/I Tb grid cell
c
c        idim2   :  Number of cells in the SSM/I Tb grid line
c
c        idim3   :  Number of grid lines in an SSM/I Tb grid
c      
c        grdtyp  : Three character descriptor designating the grid type
c                  to access from the CD-ROM (N3B or S3B)
c
c        icetyp  : Three character descriptor designating the extension
c                  for the resultant ice parameter image (N3C or S3C).
c
c        c       : Array of tiepoint coefficients. Returned from USETIE
c                  and used in GETICE. Refer to NODS Handbook Appendix
c                  CC pp 23-24. 
c
c    Outputs      
c      
c      INTERNAL  +++++++++++++++++++++++++++++++++++++++++++++++++++++  
c      
c        outfil  :  The name and descriptor string which reference       
c                   the data file which will be used to store the      
c                   ice concentration grid. This string is computed      
c                   internally from the variables, indate and grdtyp. The      
c                   file will be created in the default directory.      
c                   Program will abort if file currently exists!   
c   
c                   The file dimensions will be 152 1-byte cell values      
c                   with 224 grid lines for the grid type 'N3C' and
c                   158 1-byte cell values with 166 grid lines for the
c                   grid type 'S3C'.       
c      
c        ice     :  Output buffer containing the computed ice       
c                   concentrations for the 50km total ice concentration      
c                   grid. Totice is 158 bytes in length which corresponds 
c                   to the length of an S3C raster line.
c      
c  Vince Troisi     
c  5/3/1989     
c  
c  1/4/1989 - Modified for SSM/I CD-ROMs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
      integer idim1, idim2, idim3   
c
c There are five channels in the N3B or S3B grid cells
c     
      parameter (idim1 = 5)                
      parameter (maxlen = 316)
                       
      integer iunit, ounit, munit, olen        
      integer ymd(6), ier, recln        
                                     
      CHARACTER*1 dirchr
C       Character separator for directories - \ on IBM pc's, 
C                                             / on others
C     Added by BG 10-24-91
      character * 3  grdtyp, icetyp
      character * 2  drive  
      character * 2  year, month, day      
      character * 6  indate                                    
      character * 3  mon(12)                                     
      character *80  outfil,infile        
      integer   * 2  buff(maxlen*idim1,2)                                
      integer   * 1  ice(maxlen/2) , mask(maxlen/2)      
      real           c(12)
      data   mon       
     +  /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',         
     +   'OCT','NOV','DEC'/                                   
C        
C================================================================              
C       
c      
c Query user for search criteria      
c                               
c      
c
c  Query user for CD-Rom device attribute
c
       print *,'Enter CD reader drive specification (eg., L:) : ' 
       read (*,'(a)')drive                             
c
c  Query user for date
c
       print *,      
     + 'Enter date of ice concentration grid to compute(yymmdd):'              
       read (*,'(a)') indate                                     
       read (indate(3:4),'(i2)')mm                                     
c
c  Query user for appropriate polar region
c
       print *,' '
       print *,'Select one of the polar regions:'
       print *,' 1. North'
       print *,' 2. South'
       print *,'Enter 1 or 2:'
       read *,ihem
c
c  Query user for ice type to compute
c
       print *,' '
       print *,'Select type of ice characteristic to derive:'
       print *,' 1. Total Ice Concentration'
       print *,' 2. Multi-Year Ice Concentration'
       print *,'Enter 1 or 2:'
       read *, icepar
c
c Combine responses to form filenames and paths and to define
c IO constants
c
       if (ihem.eq.1) then
          grdtyp = 'n3b'
          icetyp = 'n3c'
          lrec   = 3040
          idim2  =  304
          idim3  =  448
          npar   = idim1*idim2
       else
          grdtyp = 's3b'
          icetyp = 's3c'
          lrec   = 3160
          idim2  =  316
          idim3  =  332
          npar   = idim1*idim2
       endif

C      Dirchr added by BG 10-24-91 for intersystem compatibility
       PRINT *,'What is the directory seperator \ or /?'
       READ (*,9001) dirchr
 9001  FORMAT (A1)
       infile=drive//dirchr//grdtyp//dirchr//'19'//
     +       indate(1:2)//dirchr//mon(mm)//        
     +       dirchr//indate(1:6)//'.'//grdtyp                                   

       print *,infile
c      
c Open brightness temperature file on CD      
c      
       iunit = 21           
       open (unit=iunit,file=infile,status='old',      
     +      form='binary',recl=lrec,iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!' 
                 stop                   
             endif                                
c                                   
c Construct output file name and open the file.        
c                                      
      outfil = indate(1:6)//'.'//icetyp        
      print *,outfil                     
      ounit= 22                                   
      open (unit=ounit,file=outfil(1:10),status='new',recl=idim2/2,        
     +   iostat=ier,form='binary')        
      if (ier.ne.0) then            
             print *,'Error opening file ',outfil(1:10)            
             stop            
      endif            
c                                   
c Open Landmask file                                   
c                                      
      munit = 18                                   
      open (unit=munit,file='c:\tools\'//icetyp//'msk.dat',
     +  form='binary',recl=idim2/2,status='old')                     
c
c Get ice algorithm coefficients derived from tiepoints
c
      call usetie (c,ihem)
              
c                                   
c-------- start ice concentration algorithm           
c                                   
      do 1000 lp=1,idim3,2        
        read(iunit)((buff(inb,jb),inb=1,npar),jb=1,2)                          
        read(munit)(mask(inm),inm=1,idim2/2)
c           
c Average TBs into 50 km cells and determine ice concentrations         
c               
        call getice(buff,c,maxlen,idim1,idim2,ice,mask,icepar)               
c                                      
c Write grid line of ice concentrations to file.      
c                                    
        write(ounit)(ice(iout),iout=1,idim2/2)    
                                   
1000  continue                                      
c                                   
c-------- end ice algorithm                                   
c                                      
      close (unit=munit)              
      close (unit=iunit)                                      
      close (unit=ounit)                                   
                                      
      end