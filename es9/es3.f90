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
y=x**(3.0/2.0)
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

function cavalieri(fun,a,b,n) result(r)
real(kind=rk), intent(in) :: a,b
integer, intent(in) :: n
integer :: i
real(kind=rk) :: r,h

interface
function fun (x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
end function fun
end interface

h=(b-a)/n
r=0
do i=1,(n-1),2
r=r+4*(fun(a+h*i))
end do

do i=2,(n-2),2
r=r+2*(fun(a+h*i))
end do
r=(r+(fun(a))+(fun(b)))*h/3
end function cavalieri
end module

program integrali
use integrazione
implicit none
real(kind=rk) :: q,a,b,int1,int2,err1,err2
integer :: n
a=0
b=1
open(unit=1,file="es3.dat")
do n=2,10000,2
int1=trapezi(fun,a,b,n)
int2=cavalieri(fun,a,b,n)
q=n
write(unit=1,fmt=*)log(q),log(abs(int1-0.4)),log(abs(int2-0.4))
end do
end program integrali
