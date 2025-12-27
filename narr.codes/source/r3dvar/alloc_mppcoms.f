       subroutine alloc_mppcoms(mype_in,npes_in)

! allocate "common" space for information on distribution of eta model amoung pe's

         include "PARMETA.comm"
         include "mpif.h"
          include "my_comm.h"
         include "mpp.h"

!   allocate space for common/mppcom/

         allocate(IS_LOC_TABLE(0:INPES*JNPES))
         allocate(JS_LOC_TABLE(0:INPES*JNPES))
         allocate(IE_LOC_TABLE(0:INPES*JNPES))
         allocate(JE_LOC_TABLE(0:INPES*JNPES))
         allocate(ICHUNKTAB(0:INPES*JNPES))

!         assign values for mype and npes in common/mppcom/

         mype=mype_in
         npes=npes_in

!   allocate space for common/glb_table/

         allocate(IS_GLB_TABLE(0:INPES*JNPES))
         allocate(IE_GLB_TABLE(0:INPES*JNPES))
         allocate(JS_GLB_TABLE(0:INPES*JNPES))
         allocate(JE_GLB_TABLE(0:INPES*JNPES))

!   allocate space for common/tempcom/

         allocate(TEMP1(IM,JM)) ; allocate(TEMP2(IM,JM))
         allocate(TEMP3(IM,JM)) ; allocate(TEMP4(IM,JM))
         allocate(TEMP5(IM,JM)) ; allocate(TEMP6(IM,JM))
         allocate(TEMP7(IM,JM)) ; allocate(TEMP8(IM,JM))
         allocate(TEMP9(IM,JM)) ; allocate(TEMP10(IM,JM))
         allocate(TEMP11(IM,JM)) ; allocate(TEMP12(IM,JM))
         allocate(TEMP13(IM,JM)) ; allocate(TEMP14(IM,JM))
         allocate(TEMP15(IM,JM)) ; allocate(TEMP16(IM,JM))
         allocate(ITEMP(IM,JM)) ; allocate(ITEMP2(IM,JM))

!   allocate space for common/topo/

         allocate(TEMP2X(0:IM+1,0:JM+1))
         allocate(TTVG(0:IM+1,0:JM+1))
         allocate(HTMG(0:IM+1,0:JM+1,LM))

!   allocate space for common/mappings/

         allocate(G2LI(0:IM+1)) ; allocate(L2GI(0:IM+1))
         allocate(G2LJ(0:JM+1)) ; allocate(L2GJ(0:JM+1))

       return
       end subroutine alloc_mppcoms

