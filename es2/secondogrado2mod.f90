program secondo_grado

        
      implicit none               
! l' istruzione precedente obbliga a definire esplicitamente il tipo di ogni variabile

      
      real    :: a,b,c            ! i coefficienti reali dell' equazione
                                  ! a x**2 + b x + c = 0

      real :: discr,xp,xm      ! discriminante e soluzioni corrispondenti
                                  ! alle due scelte del segno davanti la
                                  ! radice quadrata

      read*, a,b,c                ! i coefficienti vengono letti

      discr = b**2 - 4*a*c
      xp = ( -b + sqrt(discr) )/(2*a)
      xm = ( -b - sqrt(discr) )/(2*a)
	IF (discr<0) THEN
	print*, 'Errore: discriminante minore di 0'
	ELSE 
	print*,xp,xm
        END IF
end program secondo_grado


