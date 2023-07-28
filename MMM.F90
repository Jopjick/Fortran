Program main
implicit none
integer i
real(8):: arr(10)
integer n

read(*,*) n

do i= 1, n
read(*,*) arr(i)
enddo

do i= 1, n
write(*,*) arr(i)
enddo

end program main

