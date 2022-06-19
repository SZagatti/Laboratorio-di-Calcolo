program prodottoscalare
 implicit none
 real, dimension(2) :: a,b
 integer :: i
 real :: c 
 do i=1,2
 print*,"elemento numero",i,"di a:"
 read*,a(i)
end do
 do i=1,2
 print*,"elemento numero",i,"di b:"
 read*,b(i)
end do
print*,"vettore a:",a
print*,"vettore b:",b
print*,"---------- PRODOTTI SCALARI ----------"
print*,"METODO 1:"
print*,"prodotto scalare a.a:", sum(a*a)
print*,"prodotto scalare b.b:", sum(b*b)
print*,"prodotto scalare a.b:", sum(a*b)
print*,"METODO 2:"
print*,"prodotto scalare a.a:", dot_product(a,a)
print*,"prodotto scalare b.b:", dot_product(b,b)
print*,"prodotto scalare a.b:", dot_product(a,b)
print*,"---------- MODULI ----------"
print*,"modulo di a:", sqrt(dot_product(a,a))
print*,"modulo di b:", sqrt(dot_product(b,b))
print*,"---------- ANGOLI ----------"
 c=(dot_product(a,b))/((sqrt(dot_product(a,a)))*sqrt(dot_product(b,b)))
print*,"angolo tra a e b:", acos(c)
end program prodottoscalare
