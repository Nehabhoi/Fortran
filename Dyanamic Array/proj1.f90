module dyanmic_array_module
    implicit none 
    contains
    !Procedures that Require Explicit Interfaces
    subroutine reallocate_2d_array(a,ni_new,nj_new)

	    integer,dimension(:,:),allocatable,intent(inout) :: a
	    integer,dimension(:,:),allocatable :: temp
	    integer,intent(in) :: ni_new,nj_new
	    integer :: ni_old,nj_old

	    ni_old = ubound(a,1)
	    nj_old = ubound(a,2)

	    allocate(temp(ni_new,nj_new))

	    temp(1:ni_old,1:nj_old) = a

	    call move_alloc(temp,a)

    end subroutine reallocate_2d_array
end module dyanmic_array_module


Program grade

    use dyanmic_array_module
    implicit none 

    !define ans_key array to store answer key
    integer, dimension(:), allocatable :: ans_key

    ! deffine student_data array to store student id and answer
    integer, dimension (:,:), allocatable :: student_data

    integer :: Len, Len1, count, i,result,worth,frequency_index, total, Int1
    
    ! define filename variable for input
    character(len = 50) :: filename   

    !define score function
    integer score

    ! define frequency array and default value to zero to store frequency of each score
    integer,dimension(101) :: frequency
    frequency = (/ (0, I = 1,101) /)

    ! take file name as input  
    write(*,*) "Enter file name:"
    read(*,*) filename

    ! open file and read total quetions and store in variable Len
    open (unit=4, file=filename, status='old', action='read')
    read(4, *) Len
    worth = 100/Len

    ! allocatable memory to ans_key array and read data from file and stored value in array
    allocate(ans_key(Len))
    call readData(ans_key,Len)

    

    count = 0  
    write(*,*) "Student ID    Score"
    write(*,*) "==================="

    ! read data for each student and store it into student_data
    i = 1  
    Len1 = Len + 1
    30  call reallocate_2d_array(student_data,i,Len1)
        call readData(student_data(i,:),Len1)
        if (student_data(i,1) .ne. 0) then
            result = score(ans_key,student_data,i,Len,worth)
            frequency_index = result + 1
            frequency(frequency_index) = frequency(frequency_index) + 1
            write(*,"(1x,i5,9x,i3)") student_data(i,1),result
            i = i+1
            count = count + 1
            go to 30
        else
            go to 40
        end if

    40  write(*,*) "==================="
        write(*,"(1x,A14,i3)") "Tests graded = ",count
        write(*,*) "==================="
        write(*,*) "Score     Frequency"
        write(*,*) "==================="

        ! calculate frequency of score and Average score
        i = 101
        total = 0
    20  Int1 = i - 1
        if(frequency(i) .gt. 0) then
            write(*,"(1x,i3,11x,i3)") Int1,frequency(i)
            total = total + (Int1 * frequency(i))
        end if
        i = i-1
        if ( i .gt. 0) then
            goto 20
        end if
    write(*,*) "==================="
    total = total / count
    write(*,"(1x,A14,i3)") "Class Average =",total

    ! deallocate memory assigned to ans_key and student_data array
    deallocate(ans_key)
    deallocate(student_data)

    ! close file
    close(4)

End Program grade

!function to read one line of file and store data in array
subroutine readData(a,L)
    integer L,io,type,i
    integer, dimension (L) :: a
    read(4,*, IOSTAT = io) a
    if(io < 0) then
        a = 0
    end if
end subroutine readData

! function to calculate total marks of a student
integer function score(ans_key,student_data,i,Len,worth)
    integer Len,worth,i,x
    integer, dimension (Len) :: ans_key
    integer, dimension (i,Len + 1) :: student_data
    score = 0
    
    do j =1,Len     
        x = j + 1
        if (ans_key(j) .eq. student_data(i,x)) then
            score = score + worth
        end if
    end do
end function score

