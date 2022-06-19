module prec
integer, parameter :: rk = selected_real_kind(6)
end module prec

module exam
use prec
implicit none
contains

function posx (xo,velx,t) result(rx)
real (kind=rk), intent(in) :: xo,velx,t
real (kind=rk) :: rx
rx=xo+velx*t
end function

function posy (yo,vely,t) result(ry)
real (kind=rk) :: yo,vely,t
real (kind=rk) :: ry
ry=yo+vely*t
end function

end module


program esame
use prec
use exam
implicit none
real, parameter :: pi=acos(-1.0)
real (kind=rk),dimension(2) :: vel,n
real (kind=rk) :: a,b,xo,yo,controllo,alpha,rad,primo,secondo,terzo,t,t1,t2
integer :: i,K

print*,"Inserire i valori a e b dei semiassi dell'ellisse"
print*,"a:"
read*, a
print*,"b:"
read*, b
print*,"Inserire le coordinete x(0) e y(0) della posizione iniziale della particella all'interno della scatola ellittica"
print*,"x(0):" 
read*, xo
print*,"y(0):" 
read*, yo
controllo=((xo/a)**2)+((yo/b)**2)
if (controllo >= 1.0) STOP "ERRORE: i valori di posizione della particella si trovano all' esterno dell' ellisse o sul suo bordo"
print*,"Inserire il valore dell'angolo alpha tra il vettore velocità e l'asse delle x"
print*,"Alpha (in gradi):"
read*,alpha
print*,"Inserire il numero di collisioni che si vuole considerare"
print*,"N:"
read*,K
rad=(alpha*pi)/(180.0_rk)
vel(1)=cos(rad)
vel(2)=sin(rad)
do i=1,K
primo=((b**2)*(vel(1)**2)+(a**2)*(vel(2)**2))
secondo=(2.0_rk)*((b**2)*xo*vel(1)+(a**2)*yo*vel(2))
terzo=-(((a**2)*(b**2))-((a**2)*(yo**2))-((b**2)*(xo**2)))
t1=(-(secondo)+sqrt((secondo**2)-(4.0_rk)*primo*terzo))/(2*primo)
t2=(-(secondo)-sqrt((secondo**2)-(4.0_rk)*primo*terzo))/(2*primo)
if (t2<=t1) t=t1
if (t2>t1) t=t2
xo=posx (xo,vel(1),t)
yo=posy (yo,vel(2),t)
n(1)=xo/(a*a)/sqrt((xo/(a*a))**2+(yo/(b*b))**2)
n(2)=yo/(b*b)/sqrt((xo/(a*a))**2+(yo/(b*b))**2)
vel=vel-(2_rk)*(dot_product(vel,n))*n
write(unit=1,fmt=*)xo,yo
end do
end program
