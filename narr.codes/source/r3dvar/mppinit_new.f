subroutine mppinit_new(inpes,jnpes,imglb,jmglb, &
         ids,    ide,    jds,    jde, &
         ips_tbl,ipe_tbl,jps_tbl,jpe_tbl, &
         pe_of_injn,in_of_i,jn_of_j)

!     this subroutine mimics MPPINIT, except that everything is kept
!        in global coordinate units, which makes everything simpler.
!       this is used by subroutine out_restart_pieces_newijpe, which
!         efficiently reorganizes and outputs a restart-in-pieces file with
!           a new specification of inpes and jnpes.

!    the motivation for doing the above is to avoid using the monolithic restart file, at least
!      during the data-assimilation phase, since it is very expensive to run quilt jobs that 
!         reassemble pieces into monolithic restart files.

  implicit none

  integer(4),intent(in)::inpes,jnpes       !  desired partition

  integer(4),intent(in)::imglb,jmglb       !  x and y dimensions of full eta grid

  integer(4),intent(out)::ids,ide,jds,jde  !   global definition of eta domain

  integer(4),dimension(inpes,jnpes),intent(out):: &
                 ips_tbl,ipe_tbl,jps_tbl,jpe_tbl      !  boundaries of each subdomain (in global units)

  integer(4),intent(out)::pe_of_injn(inpes,jnpes),in_of_i(imglb),jn_of_j(jmglb)

  integer(4) i,ichunk,ichunk_calc,itail,j,jchunk,jchunk_calc,jnchunks,jtail
  integer(4) mpe,my_ie_calc,my_is_calc,my_je_calc,my_js_calc,nchunks

  ids=1
  ide=imglb
  jds=1
  jde=jmglb

  ichunk=imglb/inpes
  jchunk=jmglb/jnpes
  itail=imglb-(inpes*(imglb/inpes))
  jtail=jmglb-(jnpes*(jmglb/jnpes))

  my_js_calc=1
  jnchunks=0

  do j=1,jnpes
   jchunk_calc=jchunk
   if(j.le.jtail) jchunk_calc=jchunk+1
   jnchunks=jnchunks+jchunk_calc
   my_je_calc=jnchunks
   my_is_calc=1
   nchunks=0
   do i=1,inpes
    ichunk_calc=ichunk
    if(i.le.itail) ichunk_calc=ichunk+1
    nchunks=nchunks+ichunk_calc
    my_ie_calc=nchunks
    ips_tbl(i,j)=my_is_calc
    ipe_tbl(i,j)=my_ie_calc
    jps_tbl(i,j)=my_js_calc
    jpe_tbl(i,j)=my_je_calc
    my_is_calc=my_ie_calc+1
   end do
   my_js_calc=my_je_calc+1
  end do

  mpe=0
  do j=1,jnpes
   do i=1,inpes
    in_of_i(ips_tbl(i,j):ipe_tbl(i,j))=i
    jn_of_j(jps_tbl(i,j):jpe_tbl(i,j))=j
    pe_of_injn(i,j)=mpe
    mpe=mpe+1
   end do
  end do

return
end subroutine mppinit_new
