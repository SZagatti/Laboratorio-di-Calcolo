module prec
integer, parameter :: rk = selected_real_kind(6)
end module prec

module b
use prec
implicit none
contains

function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
y=sin(x)
end function fun

function derivfun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
y=cos(x)
end function derivfun

subroutine newton (fun,derivfun,xo,eps,x1)
real(kind=rk), intent(in) :: eps
real(kind=rk) :: xo
real(kind=rk), intent(out) :: x1
integer :: i

interface
function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
end function fun

function derivfun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
end function derivfun
end interface

do i=1,10000
x1=xo - fun(xo)/derivfun(xo)
if(x1==0 .or. abs(x1-xo)<eps) exit
xo=x1
end do
end subroutine
end module

program es5
use prec
use B
real(kind=rk) :: xo,eps
print*,"Inserire il valore di xo:"
read*, xo
print*,"Inserire il valore della precisione epsilon:"
read*, eps
call newton (fun,derivfun,xo,eps,x1)
print*, "La funzione ha valore zero nel punto x=",x1
print*, "La funzione ha valore analitico zero nel punto x=", sqrt(4.0_rk)
end program
