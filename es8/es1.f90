Module A
implicit none
contains
function raddoppia(x) result(dop)
real,dimension(:),intent(in) ::x
real,dimension(size(x,1)) ::dop
dop= 2*x
end function raddoppia
end module A

program es1
use A
implicit none
real, dimension(:), allocatable :: x,risultato
integer :: m
print*, "Dimesione dell'array:"
read*, m
allocate (x(m))
print*, "Immettere i valori dell'array:"
read*, x
risultato=raddoppia(x)
print*, "Il risultato è:",risultato
end program es1
