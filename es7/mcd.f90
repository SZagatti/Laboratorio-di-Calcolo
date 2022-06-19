module pippo
implicit none
contains 
recursive function euclide(a,b) result(c)
integer, intent(in) :: a,b
integer :: c
if(mod(a,b)==0) then
c=a/b
else
c=euclide(mod(a,b),b)
end if
end function euclide
end module

program MCD
use pippo
integer :: x,y,z
print*, "primo valore:"
read*, x
print*, "secondo valore:"
read*, y

if (x==0) z=y
if (y==0) z=x

z=euclide(x,y)
print*,"L'MCD e':", z
end program MCD
