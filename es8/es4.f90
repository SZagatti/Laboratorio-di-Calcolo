module geom2
implicit none
private     ! rende privati tutti i nomi non esplicitamente dichiarati pubblici

public :: punto,triangolo,operator(+),operator(*)

type :: punto
    real ::x,y
  contains
    procedure :: inv => invert
end type punto

type :: triangolo
    type(punto) :: vertice_A
    type(punto) :: vertice_B
    type(punto) :: vertice_C
end type triangolo

interface operator (+)
    module procedure sum_points
end interface operator (+)

interface operator (*)
    module procedure scalasx
    module procedure scaladx
end interface operator (*)
contains

function sum_points(p,q) result(r)
type(punto), intent(in) :: p,q
type(punto)             :: r
r%x = p%x + q%x
r%y = p%y + q%y
end function sum_points

function invert(this) result(r)
class(punto), intent(in) :: this
type(punto) :: r
r%x = -this%x
r%y = -this%y
end function invert

function scalasx(p,k) result(q)
type(punto), intent(in) :: p
real,intent(in) :: k
type(punto) :: q
q%x = k*p%x
q%y = k*p%y
end function scalasx

function scaladx(k,p) result(q)
type(punto), intent(in) :: p
real,intent(in) :: k
type(punto) :: q
q%x = (p%x)*k
q%y = (p%y)*k
end function scaladx

end module geom2

program piano
use geom2
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


T2%vertice_A = T1%vertice_A%inv()
T2%vertice_B = T1%vertice_B%inv()
T2%vertice_C = T1%vertice_C%inv()

T3%vertice_A = ((T2%vertice_A)+(T2%vertice_B))*0.5
T3%vertice_B = ((T2%vertice_B)+(T2%vertice_C))*0.5
T3%vertice_C = ((T2%vertice_C)+(T2%vertice_A))*0.5
print*,'coppie di coordinate dei vertici del triangolo T2', T2
print*,'coppie di coordinate dei vertici del triangolo T3', T3

open(unit=1,file="triangolo2.dat")
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

