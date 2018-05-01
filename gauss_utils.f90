module gauss_utils
  implicit none
  integer,parameter :: g_prec = 4
contains

  subroutine gaussAX (n,A,x)
    implicit none
    integer (kind = 4) :: i,j
    integer (kind = 4),intent(in) :: n
    real (kind = g_prec), intent(inout) :: A(n-1,n-1)
    real (kind = g_prec),intent(inout)  :: x(n-1)

    Do i = 2,n-1
      Do j = 1,i-1
          associate (c => A(i,j)/A(j,j))
            A(i,:) = A(i,:)-c*A(j,:)
            x(i)=x(i)-c*x(i-1)
          end associate
      end do
    end do

  end subroutine

  subroutine gaussAX_normalise (n,A,x)
    implicit none
    integer (kind = 4) :: i,j
    integer (kind = 4),intent(in) :: n
    real (kind = g_prec), intent(inout) :: A(n-1,n-1)
    real (kind = g_prec),intent(inout)  :: x(n-1)
    real (kind = g_prec) :: c

    Do i = 1,n-1
      if (A(i,i) .NE. 0) then
          c = A(i,i)
          Do j = 1,n-1
            A(i,j) = A(i,j)/c
          end do
          x(i) = x(i) / c
      end if
    end do

  end subroutine
  subroutine gaussAX_normalised_resolve (n,A,x,u)
    implicit none
    integer (kind = 4) :: i,j
    integer (kind = 4),intent(in) :: n
    real (kind = g_prec), intent(in) :: A(n-1,n-1)
    real (kind = g_prec),intent(in)  :: x(n-1)
    real (kind = g_prec),intent(inout)  :: u(0:n)
    u(0) = 0
    u(n) = 1

    DO i = 1,n-1
        u(n-i) = x(n-i)

        DO j = 1,i-1
            u(n-i) = u(n-i) - A(n-i,n-i+j)*u(n-i+j)
        end do
    end do

  end subroutine

end module gauss_utils
