program conpay

	integer year
	real owed, paymnt, rate, x, round2, next
	data owed/10000.0/, paymnt /1000.0/,  rate /8.0/
	round2(x) = real(NINT(x*100.0))/ 100.0
	next(x) = x* (1.0 + rate /100.0)

	print *, '        year   owed before              payment     owed after'
	print *, '                payment                               payment'
	print *,  '---------------------------------------------------------------'

	year = 1
11	if(paymnt .le. next(owed)) then
	owed = next(owed)
	owed = round2(owed)
	print *, year, owed, paymnt, owed - paymnt

	year = year + 1
	owed = owed - paymnt
	go to 11
	end if

	owed = next(owed)
	owed = round2(owed)

	print *, year, owed, owed, 0.0
	print *,  '---------------------------------------------------------------'
	end


