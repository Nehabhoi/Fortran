Program sortMinMax
	implicit none
	real mean
	real median
	real result
    real maximum
    real minimum
	logical l1
    integer Len
	real :: numbers(20)
    
	numbers = (/7.80,8.90,2.20,5.50,6.70,9.70,1.50,1.80,8.80,8.20,10.10,0.50,11.14,4.50,5.60,6.70,2.30,3.40,9.00,1.20/)
    Len = size(numbers)

	l1 = 1 .eq. 2
	call printfun(numbers,l1,Len)

	call sortarr(numbers,Len)

	result = mean(numbers,Len)
	write(*,"(A5,3f8.6)") 'mean=',result 

	result = median(numbers,Len)
	write(*,"(A7,3f8.2)") 'median=',result

	result = minimum(numbers,Len)
    write(*,"(A8,3f8.2)") 'minimum:',result

    result = maximum(numbers,Len)
    write(*,"(A8,3f8.2)") 'maximum:',result
    
    l1 = 1 .eq. 1
	call printfun(numbers,l1,Len)
End Program sortMinMax

real function mean(a,Len)
	real, dimension (Len), intent (in) :: a
    integer Len
	real total,cnt
	total = 0.0
	cnt = 0.0
	mean = 0.0
	do i  =1,Len
	   total = total + a(i)
	   cnt = cnt + 1
	end do

	mean = total/cnt
end function mean

real function median(a,Len)
	real, dimension (Len), intent (in) :: a
	integer Len, midpoint
	midpoint = Len /2
	if (mod(Len,2) .eq. 0 ) then
		median = (a(midpoint) + a(midpoint+1) ) / 2.0
	else
		median = a(midpoint+1)
	end if
end function median

subroutine printfun(a,b,Len)
	real, dimension (Len), intent (in) :: a
	logical ,intent(in) :: b
	integer Len
	do i =1,Len
		if (b) then
			write (*,"(3f8.2)",advance="no") a(i)
		else
			write (*,"(3f8.2)") a(i)
		end if
	end do
	write (*,'(A)')
end subroutine printfun


real function maximum(a,Len)
    real, dimension (Len), intent (in) :: a
    maximum = 0.0
    do i =1,Len
        if(a(i) .gt. maximum) then
            maximum = a(i)
        end if
    end do
end function maximum

real function minimum(a,Len)
    real, dimension (Len), intent (in) :: a
    real min
    minimum = 99999.0
    do i =1,Len
        if(a(i) .lt. minimum) then
            minimum = a(i)
        end if
    end do
end function minimum


subroutine sortarr(a,Len)
    real, dimension (Len) :: a
    real key
    integer j
    do i =2,Len
        key = a(i)
        j = i-1
        do while ((j .ge. 0) .and. (a(j) .gt. key))
            a(j+1) = a(j)
            j = j - 1
        end do
        a(j+1) = key
    end do
end subroutine sortarr