module geom
implicit none

type :: punto
    real ::x,y
end type punto

type :: triangolo
    type(punto) :: vertice_A
    type(punto) :: vertice_B
    type(punto) :: vertice_C
end type triangolo
 
contains

function sum_points(p,q) result(r)
type(punto), intent(in) :: p,q
type(punto)             :: r
r%x = p%x + q%x
r%y = p%y + q%y
end function sum_points

function invert(p) result(r)
type(punto), intent(in) :: p
type(punto) :: r
r%x = -p%x
r%y = -p%y
end function invert

function scala(p,k) result(q)
type(punto), intent(in) :: p
real,intent(in) :: k
type(punto) :: q
q%x = k*p%x
q%y = k*p%y
end function scala
end module geom


program piano
use geom
implicit none
type(punto) :: A,B,C
type(triangolo) :: T1, T2 , T3

print*,'coordinate 2D primo vertice'
read*,A
print*,'coordinate 2D secondo vertice'
read*,B
print*,'coordinate 2D terzo vertice'
read*,C
T1 = triangolo(A,B,C)  ! il nome del tipo dati "costruisce" una variabile 
                       ! dello stesso tipo a partire dai 3 campi  
print*,' vertice A: ',T1%vertice_A
print*,' vertice B: ',T1%vertice_B
print*,' vertice C: ',T1%vertice_C


T2%vertice_A = invert(T1%vertice_A)
T2%vertice_B = invert(T1%vertice_B)
T2%vertice_C = invert(T1%vertice_C)

T3%vertice_A = scala(sum_points(T2%vertice_A,T2%vertice_B),0.5)
T3%vertice_B = scala(sum_points(T2%vertice_B,T2%vertice_C),0.5)
T3%vertice_C = scala(sum_points(T2%vertice_C,T2%vertice_A),0.5)
print*,'coppie di coordinate dei vertici del triangolo T2', T2
print*,'coppie di coordinate dei vertici del triangolo T3', T3

open(unit=1,file="triangolo.dat")
write(unit=1,fmt=*)T2%vertice_A%x,T2%vertice_A%y
write(unit=1,fmt=*)T2%vertice_B%x,T2%vertice_B%y
write(unit=1,fmt=*)T2%vertice_C%x,T2%vertice_C%y
write(unit=1,fmt=*)T2%vertice_A%x,T2%vertice_A%y
write(unit=1,fmt=*)
write(unit=1,fmt=*)
write(unit=1,fmt=*)T3%vertice_A%x,T3%vertice_A%y
write(unit=1,fmt=*)T3%vertice_B%x,T3%vertice_B%y
write(unit=1,fmt=*)T3%vertice_C%x,T3%vertice_C%y
write(unit=1,fmt=*)T3%vertice_A%x,T3%vertice_A%y
end program piano

