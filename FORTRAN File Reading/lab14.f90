program outputdata

integer i
integer a,b,c,d
open(4, file='powers.out', status='old')
print *,'Contents of file powers.out'   
read(4,*)
read(4,*)
i = 0
30 read(4,*,END = 10) a,b,c,d
      print *, a,b,c,d
      i = i + 1
      goto 30
10 print *,'The file contains ',i,' records'   
close(4)

end