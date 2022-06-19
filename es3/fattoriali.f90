program fattoriali
implicit none
real :: k
integer :: i,q,N
print*,"Inserisci il numero intero fino al quale vuoi avere i fattoriali:"
read*, N
do i=1,N
k = i
   do q=1,i-1
   k = k*(i-q)
   end do
print*, i, "fattoriale =",k
end do
end program fattoriali
