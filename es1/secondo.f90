program secondo_grado

        
      implicit none               
! l' istruzione precedente obbliga a definire esplicitamente il tipo di ogni variabile

      
      real    :: a=3,b=4,c=5            ! i coefficienti reali dell' equazione
                                  ! a x**2 + b x + c = 0

      complex :: discr,xp,xm      ! discriminante e soluzioni corrispondenti
                                  ! alle due scelte del segno davanti la
                                  ! radice quadrata

      read*, a,b,c                ! i coefficienti vengono letti

      discr = b**2 - 4*a*c
      xp = ( -b + sqrt(discr) )/(2*a)
      xm = ( -b - sqrt(discr) )/(2*a)

      print*,xp,xm
end program secondo_grado
