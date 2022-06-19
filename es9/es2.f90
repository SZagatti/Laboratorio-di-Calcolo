module prec
integer, parameter :: rk = selected_real_kind(6)
end module prec

module integrazione
use prec
implicit none
contains

function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
y=exp(-x**2)
end function fun

function trapezi(fun,a,b,n) result(r)
real(kind=rk), intent(in) :: a,b
integer, intent(in) :: n
integer :: i
real(kind=rk) :: r,h

interface
function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
end function fun
end interface

h=(b-a)/n
do i=1,(n-1)
r=r+fun(a+h*i)
end do
r=h*(r+(fun(a)/2)+(fun(b)/2))
end function trapezi
end module integrazione

program integrali
use integrazione
implicit none
real(kind=rk) :: q,s,a,b,int1,int2,err1,err2
integer :: n
a=-5
b=5
print*, "Inserire il valore n nel quale si vuole dividere l'intervallo:"
read*, n
int1=trapezi(fun,a,b,n)
print*, "Il valore dell'integrale calcolato con il metodo dei trapezi e':"
print*, int1
print*, "Il valore dell'integrale calcolato analiticamanete e':"
print*, sqrt(acos(-1.0))
err1=int1-sqrt(acos(-1.0))
print*, "Errore:", err1
end program integrali
