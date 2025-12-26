      SUBROUTINE SETUP_SERVERS(NPES_MOD,
     *                         MYPE,
     *                         NPES,
     *                         IQUILT_GROUP,
     *                         INUMQ,
     *                         MPI_COMM_COMP,
     *                         MPI_COMM_INTER,
     *                         MPI_COMM_INTER_ARRAY )
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  SETUP_SERVERS   SETUP I/O SERVERS      
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-03-20
C
C ABSTRACT:  SETUP I/O SERVERS
C
C PROGRAM HISTORY LOG:
C   00-03-11  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL SETUP_SERVERS(NPES_MOD,
C    *                       MYPE,
C    *                       NPES,
C    *                       IQUILT_GROUP,
C    *                       INUMQ,
C    *                       MPI_COMM_COMP,
C    *                       MPI_COMM_INTER,
C    *                       MPI_COMM_INTER_ARRAY )

C
C   INPUT ARGUMENT LIST:
C     NPES_MOD - NUMBER OF MPI TASKS FOR MODEL INTEGRATION FROM INPES AND JNPES
C                THIS IS THE NUMBER OF MPI TASKS THE EXECUTABLE HAS BEEN BUILT FOR.
C                NPES, RETURNED FROM MPI_COMM_SIZE, MUST BE AT LEAST THIS SIZE
C                OTHERWISE THE INTEGRATION CANNOT PROCEED. THE DIFFERENCE BETWEEN
C                NPES_MOD AND NPES IS THE NUMBER OF MPI TASKS THAT ARE AVAILABLE
C                FOR I/O SERVING. THIS CAN BE ZERO, IN WHICH CASE CHKOUT WILL
C                WRITE A DIRECT ACCESS FILE THAT CAN BE SEPARTELY "QUILTED". 
C                IN ORDER TO SKIP THE SEPARATE QUILTING STEP, MAKE SURE THAT
C                THE NUMBER OF MPI TASKS THAT THE CODE IS INITIATED WITH IS AT
C                LEAST ONE GREATER THAN NPES_MOD.
C
C   OUTPUT ARGUMENT LIST:
C     NPES - NUMBER OF MPI TASKS FOR MODEL INTEGRATION
C     MYPE - MY RANK
C     IQUILT_GROUP - NUMBER OF I/O SERVER GROUPS
C     INUMQ - ARRAY THAT HOLDS THE NUMBER OF SERVERS IN EACH GROUP
C     NPES - NUMBER OF MPI TASKS FOR MODEL INTEGRATION
C     MPI_COMM_COMP - THE NEW INTRACOMMUNICATOR FOR ALL TASKS
C     MPI_COMM_INTER - THE INTERCOMMUNICATOR FOR THE I/O SERVERS
C     MPI_COMM_INTER_ARRAY - THE ARRAY OF INTERCOMMUNICATORS FOR THE INTEGRATION TASKS
C
C   INPUT FILES:  NONE
C
C   OUTPUT FILES:  
C         NONE BUT THE CODE DOES ATTEMPT TO READ THE ENVIRONMENT VARIABLE "SERVER_GROUPS".
C          THIS IS THE NUMBER OF INDEPENDENT GROUPS OF SERVER TASKS. THE DEFAULT IS ONE
C          AND SHOULD BE OK FOR MOST APPLICATIONS OF THE ETA CODE. IF ONE SET OF I/O
C          SERVERS CAN NOT COMPLETE BEFORE THE NEXT OUPUT TIME THEN ADDITIONAL I/O SERVER
C          GROUPS WOULD BE USEFUL.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C            PARA_RANGE
C            MPI_INIT
C            MPI_COMM_RANK
C            MPI_COMM_SIZE
C            MPI_COMM_DUP
C            MPI_COMM_SPLIT
C            MPI_COMM_GROUP
C            MPI_GROUP_EXCL
C            MPI_COMM_CREATE
C            MPI_GROUP_FREE
C            MPI_INTERCOMM_CREATE
C            MPI_BARRIER
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$

      include 'mpif.h'
      integer comdup
      integer, allocatable :: irank ( : )
      integer MPI_COMM_INTER_ARRAY(*), INUMQ(*)
      logical yes
      character*4 get
C-----------------------------------------------------------------------
C
C     INITIALIZE MPI
C     RETRIEVE THE NUMBER OF TOTAL MPI TASKS AND MY RANK
C
      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,mype,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,npes,ierr)
C
C     AT THIS POINT NPES IS THE TOTAL NUMBER OF MPI TASKS. WE WILL
C     RESET THIS AT THE END OF THE SUBROUTINE TO THE NUMBER OF MPI
C     TASKS THAT ARE WORKING ON THE MODEL INTGRATION.
C
C     FIRST, HOWEVER, WE NEED TO MAKE SURE THAT A SUFFICIENT NUMBER
C     OF MPI TASKS HAVE BEEN INITIATED. IF NOT, WE WILL STOP.
C
      IF ( NPES .LT. NPES_MOD ) THEN
         PRINT *, ' ***********************************************'
         PRINT *, ' ***********************************************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *
         PRINT *, ' THERE ARE INSUFFICIENT MPI TASKS TO CONTINUE'
         PRINT *, ' YOU MUST SPECIFY AT LEAST ',NPES_MOD,' TASKS'
         PRINT *, ' STOPPING NOW'
         PRINT *, ' HASTA LA VISTA BABY'
         PRINT *
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' ***********************************************'
         PRINT *, ' ***********************************************'
         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
      END IF
C
C     OK, WE HAVE A SUFFICIENT NUMBER OF MPI TASKS TO CONTINUE
C
C     HOW MANY GROUPS OF SERVERS ? THE DEFAULT IS 1 GROUP
C     THE ENVIRONMENT VARIABLE, SERVER_GROUPS, CAN BE USED TO
C     SPECIFY MORE SERVER GROUPS
C
      get = '1'
      call getenv('SERVER_GROUPS',get)
      read(get,fmt='(i4)') iquilt_group
      iquilt_group = max(iquilt_group,1)
C
C     ERROR CHECK NUMBER OF GROUPS - THE MAXIMUM IS 100 - THIS IS A LOT
C
      if ( iquilt_group .gt. 100 ) then
         print *, ' ***** IQUILT_GROUP IS GREATER THAN 100'
         print *, ' ***** DO YOU REALLY WANT THIS ?'
         print *, ' ***** IF SO THEN INCREASE SIZE IN mpp.h'
         print *, ' ***** ALSO, CHANGE IF CHECK IN SETUP_SERVERS'
         print *, ' ***** RESETTING THE NUMBER OF SERVER GROUPS TO 100'
         print *, ' ***** WE ARE CONTINUING ....   '
         iquilt_group = 100
      end if
      if ( mype .eq. 0 ) then
      print *, ' we will try to run with ',iquilt_group,' server groups'
      end if
C
C     COMPUTE THE NUMBER OF SERVERS PER GROUP
C     ALL MPI TASKS BEYOND NPES_MOD WILL BE SERVERS
C     IF THE NUMBER OF SERVERS IS NOT EQUALLY DIVISIBLE BY
C     THE NUMBER OF GROUPS OF SERVERS THEN SOME GROUPS MAY HAVE
C     MORE SERVERS THEN OTHERS - THIS IS FINE
C     NOTE THAT WE REQUIRE AT LEAST ONE SERVER PER GROUP
C     WE MAY NEED TO REDUCE THE NUMBER OF SERVER GROUPS IF
C     IT EXCEEDS THE NUMBER OF SERVERS
C
      iqserver = NPES - NPES_MOD
      if ( iqserver .eq. 0 ) then
         if ( mype .eq. 0 ) then
           print *, ' *** you specified 0 I/O servers '
           print *, ' CHKOUT will write a file'
         end if
         iquilt_group = 0
      end if
      if ( iquilt_group .gt. iqserver )  then
          iquilt_group = iqserver
          print *, ' ***** NOT ENOUGH SERVERS'
          print *, ' ***** WE NEED TO REDUCE THE NUMB OF SERVER GROUPS'
          print *, ' ***** NUMB OF SERVER GROUPS IS ', iquilt_group
      end if
      do i = 0, iquilt_group - 1
         call para_range(1,iqserver,iquilt_group,i,istaq,iendq)
         inumq(i+1) = iendq-istaq+1
      if ( mype .eq. 0 ) print *, ' i, inumq = ',i+1,inumq(i+1)
      end do
C
C     SETUP THE "COLOR" FOR MPI_COMM_SPLIT
C     THOSE TASKS WHICH WILL DO MODEL INTEGRATION WILL BE COLOR 0
C     THE SERVER TASKS WILL HAVE THE COLOR OF THE GROUP NUMBER THAT
C     THEY WILL BELONG
C
      if ( mype .lt. NPES_MOD ) then
         icolor = 0
      else 
         istaxx = NPES_MOD
         do i = 1, iquilt_group
            iendxx = istaxx + inumq(i) - 1
            if ( mype .ge. istaxx .and. mype .le. iendxx ) then
               icolor = i
            end if
            istaxx = iendxx + 1
         end do
      end if
C
C     SPLIT THE COMMUNICATOR - THE NEW INTRACOMMUNICATOR FOR ALL TASKS
C     IS MPI_COMM_COMP. MPI_COMM_WORLD IS STILL AVAILABLE BUT IT DOES
C     REFER TO ALL THE MPI TASKS ( MODEL INTEGRATION AND I/O SERVING )
C        
      call mpi_comm_dup(MPI_COMM_WORLD,comdup,ierr)
      call mpi_comm_split(comdup,icolor,mype,mpi_comm_comp,ierr)
C     
C     AT THIS POINT WE HAVE A NEW COMMUNICATOR, MPI_COMM_COMP,
C     THAT CAN BE USED BY THE FORECASTS TASKS AND THE I/O SERVER TASKS
C     FOR THEIR INTERNAL COMMUNICATIONS. ONTO THE INTERCOMMUNICATORS ...
C
C     NOW WE MUST CREATE THE INTERCOMMUNICATORS FOR USE BETWEEN THE MPI
C     TASKS DOING THE MODEL INTEGRATION AND THE MPI TASKS FOR EACH 
C     SERVER GROUP. THE FIRST STEP IS TO EXCLUDE THE TASKS THAT DONT
C     BELONG. WE WILL DO THIS FOR EACH SERVER GROUP BY EXCLUDING THE TASKS
C     FROM ALL OF THE OTHER SERVER GROUPS.
C
      allocate ( irank ( iqserver ) )
      ixx = NPES_MOD
      do i = 1, iquilt_group
         yes = .true.
         if ( mype .lt. NPES_MOD ) then
            irlr = ixx
         else
            irlr = 0
         end if
      icc = 0
      iss = NPES_MOD
C     THIS IS THE FIRST POSSIBLE TASK ID THAT COULD BE EXCLUDED
      do jj = 1, iquilt_group
         if ( jj .ne. i ) then
            issl = iss
            do kk = 1, inumq(jj)
               icc = icc + 1
               irank(icc)= issl
               if ( mype .eq. issl ) yes = .false.
               issl = issl + 1
            end do
         end if
         iss = iss + inumq(jj)
      end do
C
C     AT THIS POINT WE HAVE AN ARRAY, IRANK, WITH TASK IDS TO EXCLUDE
C     THERE ARE ICC OF THEM.
C     CREATE A NEW GROUP WITH THE TASKS FROM THE OTHER SERVER GROUPS
C     EXCLUDED AND THEN CREATE A NEW COMMUNICATOR ( IWORLD_MINUS ) THAT
C     CONTAINS ONLY THE MPI TASKS DOING THE MODEL INTEGRATION AND THE
C     TASKS THAT BLONG TO THE SERVER GROUP WE ARE CONSIDERING.
C   
      iworld = MPI_COMM_WORLD
      call mpi_comm_group(iworld,igroup,ierr)
      call mpi_group_excl(igroup,icc,irank,igroup_x,ierr)
      call mpi_comm_create(iworld,igroup_x,iworld_minus,ierr)
      call mpi_group_free(igroup,ierr)
      call mpi_group_free(igroup_x,ierr)
C
C     AT THIS POINT WE HAVE A COMMUNICATOR THAT EXCLUDES THE TASKS WE DONT WANT.
C     CREATE AN INTERCOMMUNICATOR FOR USE BETWEEN THE MPI TASKS DOING THE MODEL
C     INTEGRATION AND THE I/O SERVER GROUP WE ARE CONSIDERING. THIS PROCESS IS
C     A COLLECTIVE ROUTINE SO IT CAN ONLY BE DONE BY THE TASKS THAT HAVE NOT 
C     BEEN EXCLUDED. SAVE THIS NEW COMMUNICATOR IN MPI_COMM_INTER FOR USE BY
C     THE TASKS THAT BELONG TO THE SERVER GROUP THAT WE ARE CONSIDERING. THE
C     TASKS THAT ARE PERFORMING THE MODEL INTEGRATION WILL REFERENCE
C     MPI_COMM_INTER_ARRAY() SINCE WE WILL NEED TO SELECT WHICH SERVER
C     GROUP WE WISH TO COMMUNICATE WITH.
c
      if ( yes ) then
      call mpi_intercomm_create(mpi_comm_comp,0,iworld_minus,irlr,0,
     *   mpi_comm_inter_array(i),ierr)
      mpi_comm_inter = mpi_comm_inter_array(i)
      end if
C
      call mpi_barrier(MPI_COMM_WORLD,ierr)
C
      end do     ! end do for loop over the number of server groups
C
C***
C***  NPES IS REALLY THE NUMBER OF TASKS WORKING ON THE MODEL INTEGRATION
C***
      NPES = NPES  - IQSERVER
C
      IF(MYPE.EQ.0) THEN
         print *, ' The Model integration is using ',npes,' MPI task'
         print *, ' There are ',iqserver,' I/O servers'
      END IF
C***
      deallocate ( irank )
C
      END
