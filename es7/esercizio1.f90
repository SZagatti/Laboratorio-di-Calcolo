module primo
contains
function mediapesata(a,b,n) result(mediap)
implicit none
integer, intent(in) :: n
real,dimension(n),intent(in) :: a,b
real,dimension(n) :: r
real :: mediap
r=a*b
mediap=sum(r)/sum(b)
end function
end module primo

program esercizio1
use primo
real,dimension(:),allocatable :: matrice1,matrice2
real :: risultato 
integer :: m
print*, "Dimesione delle due matrici:"
read*, m
allocate (matrice1(m),matrice2(m))
print*, "Immettere i valori della prima matrice:"
read*, matrice1
print*, "Immettere i valori della seconda matrice:"
read*, matrice2
risultato = mediapesata(matrice1,matrice2,m)
print*,"La media pesata degli elementi della prima matrice, pesata con gli elementi della seconda sara':",risultato
end program esercizio1
