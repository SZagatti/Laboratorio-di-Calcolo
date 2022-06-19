program ordinecrescente
 implicit none
 real, dimension(:),allocatable :: a
 real :: temp
 integer :: N,i,j
print*,"dimensione dell'array a:"
read*, N
allocate (a(N))
do i=1,N
 print*,"elemento numero",i,"di a:"
 read*,a(i)
end do
do j=1,N
do i=1,N-1
if (a(i) < a(i+1)) cycle
if (a(i) > a(i+1)) temp = a(i)
a(i)=a(i+1)
a(i+1) = temp
end do
end do
print*,a
end program ordinecrescente
