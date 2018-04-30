

program main
implicit none


  integer (kind = 4) , parameter :: n = 6
  real (kind = 4) :: A(n-1,n-1)
  real (kind = 4) :: x(n-1),u(0:n)
  integer (kind = 4) :: i,j
  real (kind = 4) :: c

  WRITE(*,*) 'testa'

  Do i = 1,n-1
    x(i) = 0
  end DO



  Do i = 2,n-2
    A(i,:) = x
    A(i,i-1) = 1
    A(i,i+1) = 1
    A(i,i) = -2;
  end DO
  A(1,:) = x
  A(1,1) = -2
  A(1,2) = 1
  A(n-1,:) = x
  A(n-1,n-2) = 1
  A(n-1,n-1) = -2

  x(n-1) = -1

  WRITE(*,*) x
  WRITE(*,*) "A"
  Do i = 1,n-1

    WRITE(*,*) A(i,:)
  end DO

  Do i = 2,n-1
    Do j = 1,i-1
        c = A(i,j)/A(j,j)
        A(i,:) = A(i,:)-c*A(j,:)
        x(i)=x(i)-c*x(i-1)
    end do
  end do


        WRITE(*,*) 'after gauss'

        WRITE(*,*) x
        WRITE(*,*) "A"
        Do i = 1,n-1

          WRITE(*,*) A(i,:)
        end DO


  Do i = 1,n-1
    if (A(i,i) .NE. 0) then
      c = A(i,i)
      Do j = 1,n-1
        A(i,j) = A(i,j)/c
      end do
      x(i) = x(i) / c
    end if
  end do


      WRITE(*,*) 'after gauss2'

      WRITE(*,*) x
      WRITE(*,*) "A"
      Do i = 1,n-1

        WRITE(*,*) A(i,:)
      end DO
    u(0) = 0
    u(n) = 1

    DO i = 1,n-1
        u(n-i) = x(n-i)

        DO j = 1,i-1
            u(n-i) = u(n-i) - A(n-i,n-i+j)*u(n-i+j)
        end do


    WRITE(*,*) 'after gauss reslove'

    WRITE(*,*) u


end
