program secondo_grado

        
      implicit none               
! l' istruzione precedente obbliga a definire esplicitamente il tipo di ogni variabile

      integer, parameter :: rk=selected_real_kind(6)
      real(kind=rk)    :: a,b,c,discr,xp,xm,xd      ! discriminante e soluzioni corrispondenti
                                  ! alle due scelte del segno davanti la
                                  ! radice quadrata

      read*, a,b,c                ! i coefficienti vengono letti

      discr = b**2 - 4*a*c
      xp = ( -b + sqrt(discr) )/(2*a)
      xm = ( -b - sqrt(discr) )/(2*a)
      xd = (-2*c)/(b + sqrt(discr))
      print*,xp,xm,xd
end program secondo_grado
