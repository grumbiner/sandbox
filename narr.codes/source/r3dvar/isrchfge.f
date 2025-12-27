	function isrchflt(num,a,inca,target)

!	---------------------------------------------
!
!	Searches a real vector for the first element 
!	that is less than a real target
!
!	ouput :		i*4	array index of result
!	input :	num	i*4	number of array elements
!		a	r*4	real input array (vector)
!		inca 	i*4	increment between data 
!				values in a (not used)
!		target	r*4	target value
!
!	Steve Nieman	- May 1997
!
!	----------------------------------------------
 
  real(4) a(*)


	ianswer = 0

	if(num.le.0)then
	  ianswer=0
	  goto 200
	endif

	do i=1,num
	  if(a(1+(i-1)*inca).lt.target)goto 100
	enddo 

	ianswer = num + 1
	goto 200

100	continue

	ianswer = i

200	continue

	isrchflt = ianswer

	return
	end

        function isrchfle(num,a,inca,target)

!       ---------------------------------------------
!
!       Searches a real vector for the first element 
!       that is less than or equal to a real target
!
!       ouput :         i*4     array index of result
!       input : num     i*4     number of array elements
!               a       r*4     real input array (vector)
!               inca    i*4     increment between data 
!                               values in a (not used)
!               target  r*4     target value
!
!       Steve Nieman    - May 1997
!
!       ----------------------------------------------
 

  real(4) a(*)


	ianswer = 0

        if(num.le.0)then
	  ianswer=0
	  goto 200
	endif

        do i=1,num
          if(a(1+(i-1)*inca).le.target)goto 100
        enddo 

        ianswer = num + 1
	goto 200

100     continue

        ianswer = i

200	continue

	isrchfle = ianswer

        return
        end

        function isrchfgt(num,a,inca,target)

!       ---------------------------------------------
!
!       Searches a real vector for the first element 
!       that is greater than a real target
!
!       ouput :         i*4     array index of result
!       input : num     i*4     number of array elements
!               a       r*4     real input array (vector)
!               inca    i*4     increment between data 
!                               values in a (not used)
!               target  r*4     target value
!
!       Steve Nieman    - May 1997
!
!       ----------------------------------------------
 

  real(4) a(*)


	ianswer = 0

        if(num.le.0)then
	  ianswer=0
	  goto 200
	endif

        do i=1,num
          if(a(1+(i-1)*inca).gt.target)goto 100
        enddo 

        ianswer = num + 1
        goto 200

100     continue

        ianswer = i

200	continue

	isrchfgt = ianswer

        return
        end

        function isrchfge(num,a,inca,target)

!       ---------------------------------------------
!
!       Searches a real vector for the first element 
!       that is greater than or equal to a real target
!
!       ouput :         i*4     array index of result
!       input : num     i*4     number of array elements
!               a       r*4     real input array (vector)
!               inca    i*4     increment between data 
!                               values in a (not used)
!               target  r*4     target value
!
!       Steve Nieman    - May 1997
!
!       ----------------------------------------------
 

  real(4) a(*)


	ianswer = 0

        if(num.le.0)then
	  ianswer=0
	  goto 200
	endif

        do i=1,num
          if(a(1+(i-1)*inca).ge.target) go to 100
        enddo 

        ianswer = num + 1
        goto 200

100     continue

        ianswer = i

200	continue

	isrchfge = ianswer

        return
        end





