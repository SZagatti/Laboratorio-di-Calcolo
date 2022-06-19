program istogramma
 implicit none
 integer, dimension(1:10) :: A
integer :: i,k,j
j=0
 do i=1,10
 print*,"elemento numero",i,"di A:"
 read*,A(i)
 end do

 do k=-5,5
 do i=1,10
 if( A(i)==k) then
j=j+1
end if
 end do
print*,k,j
j=0
 end do
end program istogramma
