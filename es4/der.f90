program der
           implicit none
	   integer, parameter :: rk=selected_real_kind(33)
           real(kind=rk) :: x,xp,xm, f,fp, fm, deriv1,deriv3, deltax, deltax0
           integer ::i
           x = 1.0
           deltax0 = 0.9
           do i = 201,1,-1
               deltax = deltax0**i
               xp = x + deltax   
               xm = x - deltax
               f  = exp(x)
               fp = exp(xp)
               fm = exp(xm)
               deriv1 = ( fp - f ) / deltax
               deriv3 = ( fp - fm ) / (2*deltax)
               if ((fp-f)==0)cycle   ! l' istruzione cycle fa saltare al blocco di controllo del ciclo do
               write(unit=1,fmt=*) i,deltax,deriv1,deriv3
           end do
           end program der
