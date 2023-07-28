program Sin_sum
implicit none
real(8) x, y, char_s, ssum, d, next_cha, e, x1
integer(4) n, i
next_cha(char_s,x,n)=(-1)*char_s*x*x/(8*n*(2*n+1))

write(*,*) 'Enter accuracy for approximation:'
read(*,*) e
write(*,*) '--------------------------------------------'
write(*,*) 'Enter step for x:'
read(*,*) x1
write(*,*) '--------------------------------------------'
write(*,*) 'Sin(x/2) for x between 0 and 2:'
write(*,*) '--------------------------------------------'
write(*,*) '|  i |  x(i)  | n(i) |   y(i)  |    d(i)   |'
write(*,*) '--------------------------------------------'

x=0
i=1
do while (x.le.2.001)
y= sin(x/2)
char_s= x/2
ssum= char_s
n=1
do while (abs(ssum-y).ge.e)
char_s=next_cha(char_s,x,n)
ssum = ssum + char_s
n = n + 1
enddo
d = abs(y-ssum)
write(*,'(a i2 a f5.2 a i2 a f7.4 a e8.2 a)') ' | ', i, ' | ', x, '  |  ', n, '  | ', ssum, ' |  ', d, ' |'
write(*,*) '--------------------------------------------'
i = i + 1 
x = x + x1
enddo

end program Sin_sum
