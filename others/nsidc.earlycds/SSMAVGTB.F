      subroutine avgtb( tb,idim1,idim2,idim3,totice,mask)            

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       



c AVGTB - Average the four 25km TB cells to one 50km cell then compute   

c         ice concentrations. This subroutine only returns total ice.  

c         

c   Algorithm         

c         

c         The ice concentration algorithm is an adaptation of the    

c         algorithm designed by the NODS Software Group (refer to the    

c         NODS User Handbook, Appendix CC, pp 22-26). The algorithm    

c         uses the latest SMMR tiepoints which were determined by Don    

c         Cavalieri (NASA GFSC).    

c                  

c         Four SMMR TB 25km grid cells are averaged and the ice          

c         algorithm is applied to the averaged values. A weather  

c         filter (GR > .08) is applied to the averaged 50km TB 

c         cell to correct for false concentrations computed in regions  

c         of open water.    

c 

c         Computed concentrations greater than 120% and less than -20%    

c         are considered suspect and the corresponding cell is flagged    

c         as missing (-99).   

c         

c   Inputs          

c         

c          tb    : buffer of TB cell values. This buffer includes all        



c                  SMMR TB parameters.         

c         

c          idim1 : The number of parameters in a SMMR cell (ie., 10)         

c          idim2 : The number of cells in a SMMR grid row  (ie., 304)        



c          idim3 : The number of rows contained in buffer, tb (=2)         

c         

c          mask  : A line from the landmask for 50km grid. The mask         

c                  line corresponds to the totice grid line to be         

c                  computed. (1=land; 0=water)         

c         

c   Outputs         

c         

c          totice: Line of 50km ice concentration Grid. Values are         

c                  represented as a single byte percentage ranging         

c                  from 0 - 100%. Cells over land flagged with -88;          

c                  cells for which concentrations were suspect or          

c                  not computed are flagged with -99.         

c           

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      



c                                                                      

      integer    idim1,idim2,idim3          

      integer*2  tb(idim1*idim2,idim3)                  

      real       sum                     

      integer*2  count                                              

c                    

c Offset from start of cell to three parameters used in ice                  



c calculations for two lines of data                    

c                    

      integer*2  ioff(6)                    

c                    

      integer*2  fyr,myr,tot                               

      integer*1  fyrb(2),myrb(2)                                    

      integer*1  totice(idim2/2),mask(idim2/2),fyrice,myrice                 



      logical    GOOD         

c                    

c Four TB parameters for ice concentrations                    

c                                                                            



      real       BT18V,BT18H,BT37V                                       

c                                                   

      real       GR,PR,PRGR                                       

      real       DW(2),DF(2),DM(2),SW(2),SF(2),SM(2)          

      real       CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL          

      real       ANF,ANM,DD                                       

c                                       

      equivalence  (fyr, fyrb(1))                                       

      equivalence  (myr, myrb(1))                                       

c                               

c Tiepoints for open water, first-year ice and multi-year ice          

c                                      

      data TB18VW / 157.3/                                       

      data TB18HW / 102.1/                                       

      data TB18VF / 237.5/                                       

      data TB18HF / 227.1/                                       

      data TB18VM / 202.7/                                       

      data TB18HM / 189.7/                                       

      data TB37VW / 189.5/                                       

      data TB37VF / 233.4/                                       

      data TB37VM / 168.9/          



c

c  Ice image product to produce  1) total ice concentration

c				 2) multiyear ice concentrations

c				 3) first year ice concentrations

c

       data imgtyp / 2/



 

c          

c Offset pointers for Brightness temperature cells           

c                                

      data ioff /4,14,5,15,8,18/             

c          

c          

c      

      DW(1) = TB18VW - TB18HW                                        

      DF(1) = TB18VF - TB18HF                                        

      DM(1) = TB18VM - TB18HM                                        

      SW(1) = TB18VW + TB18HW                                        

      SF(1) = TB18VF + TB18HF                                        

      SM(1) = TB18VM + TB18HM                                        

      DW(2) = TB37VW - TB18VW                                        

      DF(2) = TB37VF - TB18VF                                        

      DM(2) = TB37VM - TB18VM                                        

      SW(2) = TB37VW + TB18VW                                        

      SF(2) = TB37VF + TB18VF                                        

      SM(2) = TB37VM + TB18VM                                        

c  

c Derive coefficients from tiepoints  

c                                   

      CA = DM(1)*DW(2) - DM(2)*DW(1)                                       

      CB = DM(2)*SW(1) - DW(2)*SM(1)                                       

      CC = DW(1)*SM(2) - DM(1)*SW(2)                                       

      CD = SM(1)*SW(2) - SM(2)*SW(1)          

      CE = DF(1)*(DM(2) - DW(2)) + DW(1)*(DF(2) - DM(2))           

     1+    DM(1)*(DW(2) - DF(2))                                       

      CF = DF(2)*(SM(1) - SW(1)) + DW(2)*(SF(1) - SM(1))          

     1+    DM(2)*(SW(1) - SF(1))                                       

      CG = DF(1)*(SW(2) - SM(2)) + DW(1)*(SM(2) - SF(2))          

     1+    DM(1)*(SF(2) - SW(2))                

      CH = SF(2)*(SW(1) - SM(1)) + SW(2)*(SM(1) - SF(1))              

     1+    SM(2)*(SF(1) - SW(1))                                       

C                                       

      CI = DF(2)*DW(1) - DF(1)*DW(2)                                       

      CJ = DW(2)*SF(1) - DF(2)*SW(1)                                       

      CK = SW(2)*DF(1) - DW(1)*SF(2)                                       

      CL = SF(2)*SW(1) - SF(1)*SW(2)    

c   

c Begin processing buffer   

c                              

      ii = 0                                         

      do 1000 i = 1,idim1*idim2,idim1*idim3           

c          

c Increment pointer for next Ice cell.          

c          

         ii=ii+1                                         

c         

c Average TBs for 18V, 18H, 37V         

c  

                  do 80 l=1,3    

                   k=2*(l-1)    

                   sum = 0.0         

                   count = 0         

                   do 70 m=1,2         

                    do 60 n=1,2          

c          

c Sum Tb Cell value if Tb cell value not missing (0)          

c    

                     if(tb(i+ioff(k+m),n) .ne. 0) then

c		      print*,'tb = ',float(tb(i+ioff(k+m),n))/10.

                      sum= sum + float(tb(i+ioff(k+m),n))/10.       

                      count=count+1         

                     endif    

 60                 continue         

 70                continue     

                   if (count.ne.0) sum=sum/count         

                   if(l.eq.1) BT18V = sum         

                   if(l.eq.2) BT18H = sum         

                   if(l.eq.3) BT37V = sum                    

 80              continue         

C                        

C Calculate Ice Concentration         

C           

          GOOD = .true.          

c          

c Set ice cell to land(101) if over land          

c          

          if(mask(ii).eq.1)then        

             totice(ii)=102        

          else        

c          

c Set ice cell to missing if any TB parameters are missing          

c                            

            if (BT18V.EQ.0.0 .or. BT18H.EQ.0.0 .or.          

     *          BT37V.EQ.0.0 ) then          

               fyrice     = 0                            

               myrice     = 0                            

               totice(ii) = 0                           

            else                            

c          

c Compute open water weather filter          

c                            

               GR = (BT37V - BT18V)/(BT37V + BT18V)           

c...........................................................        

c Set ice cell to 0% concentration if weather filter > .08          

c Filter value of 0.08 may be altered at a future date! 

c........................................................... 

              if (GR .gt. .08) then                            

                  fyrice = 0                            

                  myrice = 0                            

                  totice(ii) = 0                            

              else                                   

c          

c Apply noniterative ice algorithm          

c 

c Compute polarization ration PR                            

c 

                  PR = (BT18V - BT18H)/(BT18V + BT18H)              

                  PRGR = PR*GR                            

                  ANF  = CA + CB*PR + CC*GR + CD*PRGR          

                  ANM  = CI + CJ*PR + CK*GR + CL*PRGR    

c		  print*,'ci = ',ci

c		  print*,'cj = ',cj

c		  print*,'ck = ',ck

c		  print*,'cl = ',cl

                  DD   = CE + CF*PR + CG*GR + CH*PRGR 

c 

c Compute First-Year, Multi-Year, and Total Ice Concentrations 

c 

c                  print*,'fyr float',100.0*(ANF/DD)

                  fyr = ifix(100.0*(ANF/DD))                        

                  myr = ifix(100.0*(ANM/DD))                        

                  tot = fyr + myr   

c		  print*,'fyr, myr, tot',fyr,myr,tot

c..................................................... 

c  Filter out values outside the range of -20 to 120 

c  This filter may be altered at a future date 

c..................................................... 

                  if (fyr .lt. -20 .or.                             

     *                myr .lt. -20 .or.                             

     *                tot .lt. -20 .or.                            

     *                fyr .GT. 120 .or.                             

     *                myr .GT. 120 .or.                             

     *                tot .GT. 120)  then          

                        fyrice     = 0                            

                        myrice     = 0         

                        totice(ii) = 0          

                        GOOD = .false.                            

                   endif                            

c                                       

c Place concentration values into byte data representations 

c 

                   if (GOOD) then                           

                     fyrice = fyrb(2)                            

                     myrice = myrb(2)       

		     if (imgtyp.eq.1) totice(ii) = fyrice + myrice

		     if (imgtyp.eq.2) totice(ii) = myrice

		     if (imgtyp.eq.3) totice(ii) = fyrice

c		     print*,fyrice,myrice,totice(ii)

c          

c Set concentration to 0 if computed concentration between -20 and 0         



c Set concentration to 100 if computed concentration between 100 and 120     



c          

                     if (totice(ii) .lt. 0)   totice(ii) = 0 

                     if (totice(ii) .gt. 100) totice(ii) = 100

c		     print*,totice(ii)

                   endif

                   GOOD =.true.          

c                            

               endif                            

            endif                            

         endif                           

1000    continue           

c                              

      return                              

      end

