program mediarandom
 implicit none
 integer :: M,i
 real :: x,somma
 print*,"numero di numeri casuali che si vuole generare:"
 read*, M
 somma = 0
 do i=1,M
 CALL random_number(x)
 somma = x + somma
 end do
print*,somma
 Print*,"LA MEDIA E':", somma/M
 end program mediarandom
