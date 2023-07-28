real(8) function tan_next(x, n, count1)
implicit none
integer:: n, count1
integer i
real(8) sin_cur, cos_cur, sin_prev, cos_prev, sin_f, cos_f, temp1, temp2, tan_end 
real(8):: x

sin_f(x, sin_cur, sin_prev)= 2*sin_cur*cos(x)-sin_prev
cos_f(x, cos_cur, cos_prev)= 2*cos_cur*cos(x)-cos_prev

sin_prev = 0
cos_prev = 1
sin_cur = sin(x)
cos_cur = cos(x)

do i=1, n
count1 = count1 + 1
temp1= sin_cur
temp2= cos_cur
sin_cur= sin_f(x, sin_cur, sin_prev)
cos_cur = cos_f(x, cos_cur, cos_prev)
sin_prev= temp1
cos_prev= temp2
enddo

tan_end = sin_cur/cos_cur
tan_next = tan_end
end function tan_next

recursive function sin_next(x, n, coun) result(sin_end)
implicit none
integer:: n
integer coun
real(8):: x, sin_end(2), t(2)

coun = coun + 1

if (n.gt.1) then
t = sin_next(x, n, coun)
sin_end(2)= sin_end(1)
sin_end(1)= 2*t(1)*cos(x)-t(2)

elseif (n.eq.1) then
sin_end(2)=1
sin_end(1)= sin(x)
else
sin_end(1)= 0
endif
end function sin_next

real(8) recursive function cos_next(x,n, coun) result(cos_end)
implicit none
integer:: n
integer coun
real(8):: x
real(8) cos_prev
coun = coun + 1
cos_prev= 1
if (n.gt.1) then
cos_prev= 2*cos_next(x,n-1, coun)*cos(x)-cos_prev
cos_end = cos_prev
elseif (n.eq.1) then
cos_prev = cos(x)
cos_end = cos_prev
else
cos_end= cos_prev
endif
end function cos_next

program main
implicit none
integer n, coun, count1
real(8) x, tan_next, tg_it, tg_rec, cos_next
real(8):: t(2)
interface
function sin_next(x, n, coun) result(sin_end)
real(8):: x, sin_end(2)
integer n, coun
end function sin_next
end interface

coun = 0
count1 = 0

write(*,*) 'Enter x'
read(*,*)  x
write(*,*) 'Enter n'
read(*,*)  n

tg_it= tan_next(x,n, count1)
t = sin_next(x, n, coun)
tg_rec= t(1)


write(*,'(a f7.2)') ' iterative tan(n+1)= ', tg_it 
write(*,'(a i3)') ' deapth of iteration:', count1
write(*,'(a f7.2)') ' recursive tan(n+1)= ', tg_rec 
write(*,'(a i5)') ' deapth of recursion:', coun	
end program main
