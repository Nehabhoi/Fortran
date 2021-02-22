program Powers

integer x


open(4, file='powers.out', status='new')

write (4,90) 'x', 'x**2',  'x**3', 'x**4'
90 format(10x,A1,7x,A4,6x,A4,6x,A4)

write (4,80) Repeat ('=',33)
80 format(10x,A33)

do 11 x = 1, 15

write (4, 20) x, x**2, x**3, x**4

20 format (1x, 2i10, i10, i10)

11 continue

close(4)

end
