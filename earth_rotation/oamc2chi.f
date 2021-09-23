	program oamc2chi

c	This routine converts the oceanic angular momentum
c	due to currents (kg-m**2/s) to Earth orientation 
c	excitation functions (mas and ms).

	integer*4 nin, nout
	real*8 mjd
	real*8 xin,  yin,  zin
	real*8 xout, yout, zout
	real*8 pi, omega, cmina, cmoi
	character*255 inf, outf

        pi    = 3.14159265358979323846d0
        omega = 7.292115d-5     ! mean sidereal rotation rate (radians/sec)
        cmina = 0.0261d37       ! C-A in kg-m**2 for 1066A (from Smith & Dahlen)
        cmoi  = 7.1242d37       ! principle moment of inertia C of mantle in kg-m**2

	write (6,*)
	write (6,'(1x,a,$)') 'Enter input  current OAM file name: '
	read  (5,'(a)') inf
	open  (1,file=inf,form='formatted',status='old')
	rewind 1

	write (6,'(1x,a,$)') 'Enter output current chi file name: '
	read  (5,'(a)') outf
	open  (2,file=outf,form='formatted',status='new',
     +	    carriagecontrol='list',recl=132)
	rewind 2

	nin  = 0
	nout = 0
  10	continue
	read (1,*,err=888,end=20) mjd, xin, yin, zin
	nin = nin + 1

	xout = xin * 1.61d0 / omega / cmina			! radians
	yout = yin * 1.61d0 / omega / cmina			! radians
	zout = zin / omega / cmoi				! days
	xout = xout * 180.0d0 * 3600.0d0 * 1000.0d0 / pi	! mas
	yout = yout * 180.0d0 * 3600.0d0 * 1000.0d0 / pi	! mas
	zout = zout * 86400.0d0 * 1000.0d0			! ms

	write (2,*,err=888) mjd, xout, yout, zout
	nout = nout + 1
	goto 10

  20	continue

	write (6,*)
	write (6,*) nin,  ' records  read  from input file'
	write (6,*) nout, ' records written to output file'

	stop

 888	stop ' ERR encountered in I/O'

	end
