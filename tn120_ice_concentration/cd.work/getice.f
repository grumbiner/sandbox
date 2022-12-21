      subroutine getice ( tb,c,maxlen,idim1,idim2,ice,mask,icepar )             
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c GETICE - Average the four 25km TB cells to one 50km cell then compute    
c          ice concentrations.   
c          
c   Algorithm          
c          
c         The ice concentration algorithm is an adaptation of the     
c         algorithm designed by the NODS Software Group (refer to the     
c         NODS User Handbook, Appendix CC, pp 22-26). The algorithm     
c         uses the latest SSM/I tiepoints which were determined by Don     
c         Cavalieri and Koni Steffen.     
c                   
c         Four SSM/I TB 25km grid cells are averaged and the ice           
c         algorithm is applied to the averaged values. A weather   
c         filter (GR > .05) is applied to the averaged 50km TB  
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
c                  SSM/I TB parameters excluding the 85GHz channels.          
c          
c          idim1 : The number of parameters in a SSM/I cell (ie., 5)          
c
c          idim2 : The number of cells in a SSM/I Tb grid line.         
c
c          maxlen: The maximum length of tb          
c          
c          mask  : A line from the landmask for 50km grid. The mask          
c                  line corresponds to the ice grid line to be          
c                  computed. (1=land; 0=water)     
c
c          icepar: The ice concentration type to compute (1=total,
c                  2=multiyear)
c
c
c   Internals
c
c          c     : Array of coefficients used to compute ice concentrations.
c                  Refer to NODS Handbook Volue II. Appendix CC pp 23-24.
c
c          ioff  : Offset pointers used to point to Tb values used in
c                  this ice algorithm (19GHz V, 19GHz H, 37GHz V). 
c
c          ipos  : position in tb array which is to be summed
c
c          sum   : Real register used for summing four adjacent Tb values
c
c          count : The number of Tb values not flagged as missing. A Tb value
c                  of zero (0) is a missing Tb value.
c
c          fyr   : Integer register which stores the calculated first-year
c                  ice concentration
c
c          myr   : Integer register which stores the calculated multi-year
c                  ice concentration
c
c          tot   : Integer register which stores the calculated total ice
c                  concentration
c
c          fyrb  : Two - byte buffer equivalenced to fyr register. Only the
c                  first byte is the significant part of the register.
c
c          myrb  : Two - byte buffer equivalenced to myr register. Only the 
c                  first byte is the significant part of the register.
c
c          GR    : Gradient ratio
c
c          PR    : Polarization ration
c          
c   Outputs          
c          
c          ice   : Line of 50k m ice concentration grid. Values are          
c                  represented as a single byte percentage ranging          
c                  from 0 - 100%. Cells over land flagged with -88;           
c                  cells for which concentrations were suspect or           
c                  not computed are flagged with -99.          
c            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
 
c                                                                       
      integer    idim1,idim2
      integer    maxlen           
      integer*2  tb(maxlen*idim1,2)                   
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
      integer*1  ice(idim2/2),mask(idim2/2),fyrice,myrice                  
 
      logical    GOOD          
c                     
c Three TB parameters for ice concentrations                     
c                                                                             
 
      real       BT19V,BT19H,BT37V                                        
c
c 
c                                                    
      real       C (12) 
      real       GR,PR,PRGR                                        
      real       ANF,ANM,DD                                        
      INTEGER*2 tbval ! Added by BG 10-24-91
      IMPLICIT NONE   ! Added by BG 10-24-91
      INTEGER i, l, k, m, ipos, n, icepar, ii  ! Added by BG 10-24-91
c                                        
      equivalence  (fyr, fyrb(1))                                        
      equivalence  (myr, myrb(1))                                        
c           
c Offset pointers for Brightness temperature cells used in algorithm            
c                                 
      data ioff /0,5,1,6,3,8/         
c    
c Begin processing buffer    
c                               
      ii = 0                                          
      do 1000 i = 1,idim1*idim2,idim1*2            
c           
c Increment pointer for next Ice cell.           
c           
         ii=ii+1                                          
c          
c Average TBs for 19V, 19H, 37V          
c   
c                         
c Calculate Ice Concentration          
c            
          GOOD = .true.           
c           
c Set ice pixel to land(-88) if over land           
c           
          if (mask(ii).eq.1) then         
             ice(ii)=-88         
          else         
c
c Determine Ice Concentration of the ice pixel 
c 
              do 80 l=1,3     
                k=2*(l-1)     
                sum = 0.0          
                count = 0          
                do 70 m=1,2
                  ipos=i+ioff(k+m)          
                  do 60 n=1,2           
c           
c Sum Tb Cell value if Tb cell value not missing (0)           
c     
                    tbval = tb(ipos,n)
                    if (tbval .ne. 0) then          
                     sum= sum + float(tbval)/10.        
                     count=count+1          
                    endif     
 60                continue          
 70              continue      
                 if (count.ne.0) sum=sum/count          
                 if(l.eq.1) BT19V = sum          
                 if(l.eq.2) BT19H = sum          
                 if(l.eq.3) BT37V = sum                     
 80            continue          
c           
c Set ice cell to missing if any TB parameters are missing           
c                             
               if (BT19V.EQ.0.0 .or. BT19H.EQ.0.0 .or.           
     *             BT37V.EQ.0.0 ) then           
                   fyrice     = -99                             
                   myrice     = -99                             
                   ice(ii)    = -99                             
               else                             
c           
c Compute open water weather filter           
c                             
                   GR = (BT37V - BT19V)/(BT37V + BT19V)            
c...........................................................         
c Set ice cell to 0% concentration if weather filter > .05           
c Filter value of 0.05 may be altered at a future date!  
c...........................................................  
                   if (GR .gt. .05) then                             
                      fyrice  = 0                             
                      myrice  = 0                             
                      ice(ii) = 0                             
                   else                                    
c           
c Apply noniterative ice algorithm           
c  
c Compute polarization ration PR                             
c  
                      PR   = (BT19V - BT19H)/(BT19V + BT19H)               
                      PRGR = PR*GR                             
                      ANF  = C(1) + C(2)*PR + C(3)*GR + C(4)*PRGR           
                      ANM  = C(9) + C(10)*PR + C(11)*GR + C(12)*PRGR            
                      DD   = C(5) + C(6)*PR + C(7)*GR + C(8)*PRGR  
c  
c Compute First-Year, Multi-Year, and Total Ice Concentrations  
c  
                      fyr = ifix(100.0*(ANF/DD))                         
                      myr = ifix(100.0*(ANM/DD))                         
                      tot = fyr + myr    
c.....................................................  
c  Filter out values outside the range of -20 to 120  
c  This filter may be altered at a future date  
c.....................................................  
                      if (fyr .lt. -20 .or.                              
     *                    myr .lt. -20 .or.                              
     *                    tot .lt. -20 .or.                             
     *                    fyr .gt. 120 .or.                              
     *                    myr .gt. 120 .or.                              
     *                    tot .gt. 120)  then           
                            fyrice     = -99                             
                            myrice     = -99           
                            ice(ii)    = -99           
                            GOOD       = .false.                             
                      endif                             
c                                        
c Place concentration values into byte data representations  
c  
                      if (GOOD) then                            
                          fyrice  = fyrb(1)                             
                          myrice  = myrb(1)           
                          if (icepar.eq.1) ice(ii) = fyrice + myrice 
                          if (icepar.eq.2) ice(ii) = myrice          
c------------------------------------------------------------------------
c Set concentration to 0 if computed concentration between -20 and 0        
c Set concentration to 100 if computed concentration between 100 and 120 
c------------------------------------------------------------------------
                          if (ice(ii) .lt. 0)   ice(ii) = 0  
                          if (ice(ii) .gt. 100) ice(ii) = 100           
                      endif 
                      GOOD =.true.                          
                   endif                             
               endif                             
          endif                            
1000    continue            
c
c Return computed ice concentration array, ice
c                               
      return                               
      end