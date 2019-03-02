      subroutine usetie90( c , ihem )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c USETIE - This subroutine computes coefficients for the ice algorithm
c          using the NASA-approved tiepoints.
c
c   Inputs
c
c          c     : Real array of coefficients. Refer to NODS User 
c                  handbook for description of coefficients A-L.
c
c          ihem  : Integer denoting hemisphere (1=north, 2=south). Used
c                  in determining which set of coefficients to use.
c
c
c   Internals
c
c          TB...W: Open water tiepoints. One for each of the three channels
c                  used in the ice concentration algorithm (19V, 19H, 37V)
c
c          TB...F: Firstyear tiepoints. One for each of the three channels
c                  used in the ice concentration algorithm (19V, 19H, 37V)
c
c          TB...M: Multiyear tiepoints. One for each of the three channels
c                  used in the ice concentration algorithm (19V, 19H, 37V)
c
c          DW    : Difference between component tiepoints for open water.
c
c          SW    : Sum of the component tiepoints for open water.
c
c          DF    : Difference between component tiepoints for first year ice.
c
c          SF    : Sum of the component tiepoints for first year ice.
c
c          DM    : Difference between component tiepoints for multiyear ice.
c
c          SM    : Sum of the component tiepoints for multiyear ice.
c
c
c   Outputs
c
c          c     : Array of coefficients which have been computed from
c                  tiepoints.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT none 
c
c
      real c(12)
      integer ihem
c                
c               
c Tiepoints for open water, first-year ice and multi-year ice
c The first element of each tiepoint array represents the tiepoints
c for the North Polar region; the second element is applied to the
c South Polar region.
c                   
c   declare the two-element open water tiepoints
c      
      real TB19VW(2), TB19HW(2), TB37VW(2)
c
c   declare the two-element first-year tiepoints
c
      real TB19VF(2), TB19HF(2), TB37VF(2)
c
c   declare the two-element multi-year tiepoints
c
      real TB19VM(2), TB19HM(2), TB37VM(2)
c
      real DW(2) , DF(2) , DM(2)
      real SW(2) , SF(2) , SM(2)
c
c First component of each tiepoint pair is for Northern Hemisphere
c Second component of each tiepoint pair is of Southern Hemisphere
c These tiepoints approved for use by the NASA Sea Ice Algorithm
c Working Group (1/90)
c

      data TB19VW / 175.3, 175.3/   
      data TB19HW /  97.7,  97.7/   
      data TB19VF / 254.0, 251.2/   
      data TB19HF / 236.0, 241.7/  
      data TB19VM / 223.2, 223.2/ 
      data TB19HM / 203.9, 203.9/ 
      data TB37VW / 199.6, 199.6/ 
      data TB37VF / 250.0, 248.3/ 
      data TB37VM / 186.3, 186.3/
 

c
c
c
      DW(1) = TB19VW(ihem) - TB19HW(ihem)
      DF(1) = TB19VF(ihem) - TB19HF(ihem)
      DM(1) = TB19VM(ihem) - TB19HM(ihem)
      SW(1) = TB19VW(ihem) + TB19HW(ihem)
      SF(1) = TB19VF(ihem) + TB19HF(ihem)
      SM(1) = TB19VM(ihem) + TB19HM(ihem)
      DW(2) = TB37VW(ihem) - TB19VW(ihem)
      DF(2) = TB37VF(ihem) - TB19VF(ihem)
      DM(2) = TB37VM(ihem) - TB19VM(ihem)
      SW(2) = TB37VW(ihem) + TB19VW(ihem)
      SF(2) = TB37VF(ihem) + TB19VF(ihem)
      SM(2) = TB37VM(ihem) + TB19VM(ihem)
c   
c Derive coefficients from tiepoints
c
      C(1) = DM(1)*DW(2) - DM(2)*DW(1)       
      C(2) = DM(2)*SW(1) - DW(2)*SM(1)      
      C(3) = DW(1)*SM(2) - DM(1)*SW(2)     
      C(4) = SM(1)*SW(2) - SM(2)*SW(1)    
      C(5) = DF(1)*(DM(2) - DW(2)) + DW(1)*(DF(2) - DM(2))   
     1+    DM(1)*(DW(2) - DF(2))                            
      C(6) = DF(2)*(SM(1) - SW(1)) + DW(2)*(SF(1) - SM(1)) 
     1+    DM(2)*(SW(1) - SF(1))                          
      C(7) = DF(1)*(SW(2) - SM(2)) + DW(1)*(SM(2) - SF(2))
     1+    DM(1)*(SF(2) - SW(2))
      C(8) = SF(2)*(SW(1) - SM(1)) + SW(2)*(SM(1) - SF(1))
     1+    SM(2)*(SF(1) - SW(1))                         
      C(9) = DF(2)*DW(1) - DF(1)*DW(2)                  
      C(10) = DW(2)*SF(1) - DF(2)*SW(1)                
      C(11) = SW(2)*DF(1) - DW(1)*SF(2)               
      C(12) = SF(2)*SW(1) - SF(1)*SW(2)     
c
c
c
      return
      end
