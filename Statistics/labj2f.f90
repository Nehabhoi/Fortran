Program meanMedian
	implicit none
	real mean
	real median
	real result
	logical l1
	real :: numbers(9)
	numbers = (/1.2,2.3,3.4,4.5,5.6,6.7,7.8,8.9,9.0 /)

	l1 = 1 .eq. 1
	call printfun(numbers,l1)

	result = mean(numbers)
	write(*,"(A5,3f8.6)") 'mean=',result 

	result = median(numbers)
	write(*,"(A7,3f8.2)") 'median=',result

	l1 = 1 .eq. 2
	call printfun(numbers,l1)
End Program meanMedian

real function mean(a)
	real, dimension (9), intent (in) :: a
	real total,cnt
	integer arraySize
	arraySize = size(a)
	total = 0.0
	cnt = 0.0
	do i  =1,arraySize
	   total = total + a(i)
	   cnt = cnt + 1
	end do

	mean = total/cnt
end function mean

real function median(a)
	real, dimension (9), intent (in) :: a
	integer arraySize, midpoint
	arraySize = size(a)
	midpoint = arraySize /2
	if (mod(arraySize,2) .eq. 0 ) then
		median = (a(midpoint) + a(midpoint+1) ) / 2.0
	else
		median = a(midpoint+1)
	end if
end function median

subroutine printfun(a,b)
	real, dimension (9), intent (in) :: a
	logical ,intent(in) :: b
	integer arraySize
	arraySize = size(a)
	do i =1,arraySize
		if (b) then
			write (*,"(3f8.2)",advance="no") a(i)
		else
			write (*,"(3f8.2)") a(i)
		end if
	end do
	write (*,'(A)')
end subroutine printfun