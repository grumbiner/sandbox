subroutine gradnl(grad,pg,cg,yo,xbarb,xbar,mdata,xfw,doqc)

!   compute nonlinear or linear gradient of Jo

  real(4) grad(max(1,mdata)),yo(max(1,mdata)),xbarb(max(1,mdata)),xbar(max(1,mdata))
  logical doqc
  logical xfw(max(1,mdata))
  real(8) wgross,wnotgross,residual,arg1,arg2,arg

  wnotgross=1._8-pg
  wgross=1._8*pg*cg
  if(mdata.gt.0) then
   do i=1,mdata
    residual=1._8*yo(i)-1._8*(xbarb(i)+xbar(i))
    if(.not.xfw(i) .and. doqc) then
      arg1 = wnotgross * exp(-0.5_8*residual**2)
            arg2 = wgross
            arg = arg1 + arg2
            grad(i)=residual*arg1/arg
    else
            grad(i)=residual
    endif
   enddo
  endif

return
end subroutine gradnl
