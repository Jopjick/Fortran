program who
implicit none
real(8) x, a, y, e
real(8):: sum= 0
integer(2):: n=1

write(*,*) ' Enter value of x for sin(x/2):'
write(*,*) ' (x must be a float number between -2 and 2)'
read(*,*) x
do while ((x.GT.2).or.(x.LT.-2))
	write(*,*) ' x does not get into the gap:'
	write(*,*) ' Enter value of x for sin(x/2):'
	read(*,*) x
enddo

write(*,*) '__________________________'
write(*,*) 'Enter accuracy:'
read(*,*) e
write(*,*) '__________________________'

write(*,*) 'i       x(i)     n(i)     y(i)    d(i)' 
a = x/2
sum= sum+a
y = sin(x/2)
write(*,'(i2 a f10.8)') n, ' ', a
do while (abs(sum - y).gt.e)
	n = n+1
	a= (-1)*a*(x**2)/(4*(2*n-1)*(2*n-2))
	sum = sum + a
	write(*,'(i2 a f10.8 )') n, ' ', a
enddo

end program who
