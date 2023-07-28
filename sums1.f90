program Sin_sum
	implicit none
	real(8) x, character_s, y, d
	real(8):: sin_sum= 0.0, e= 0.00001
	integer(4) n, co, i

	write(*,*) ' Enter value of x for sin(x/2):'
	write(*,*) ' (x must be a float number between -2 and 2)'
	read(*,*) x
	write(*,*) 'i x(i)       n(i)       y(i)    d(i)' 
	n= 1
	co= 1
	do while x.le.2
		
		x = x + 0.1
	end program	
	
