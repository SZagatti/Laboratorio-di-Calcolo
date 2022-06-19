program array
 implicit none
real, dimension(10) :: x
integer :: i
read*,x
do i=1,10
print*,"elemento numero:",i
 read*,x(i)
end do
print*,x
do i=1,10
 print*,x(i)
end do
end program array
