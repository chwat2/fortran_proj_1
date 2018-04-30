
subroutine declareX (n,x)
  implicit none
  integer,parameter :: prec = 4
  integer (kind = 4),intent(in) :: n
  real (kind = prec) , intent(inout) :: x(n-1)
  integer (kind = 4) :: i

  Do i = 1,n-2
    x(i) = 0
  end DO
  x(n-1) = -1
end subroutine

subroutine declareA (n,A)
  implicit none
  integer,parameter :: prec = 4
  integer (kind = 4) :: i
  integer (kind = 4),intent(in) :: n
  real (kind = prec), intent(inout) :: A(n-1,n-1)
  real (kind = prec),allocatable :: x(:)
  allocate (x(n-1))
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

  if (allocated(x)) deallocate(x)
end subroutine

subroutine gaussAX (n,A,x)
  implicit none
  integer,parameter :: prec = 4
  integer (kind = 4) :: i,j
  integer (kind = 4),intent(in) :: n
  real (kind = prec), intent(inout) :: A(n-1,n-1)
  real (kind = prec),intent(inout)  :: x(n-1)

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
  integer,parameter :: prec = 4
  integer (kind = 4) :: i,j
  integer (kind = 4),intent(in) :: n
  real (kind = prec), intent(inout) :: A(n-1,n-1)
  real (kind = prec),intent(inout)  :: x(n-1)
  real (kind = prec) :: c

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
  integer,parameter :: prec = 4
  integer (kind = 4) :: i,j
  integer (kind = 4),intent(in) :: n
  real (kind = prec), intent(in) :: A(n-1,n-1)
  real (kind = prec),intent(in)  :: x(n-1)
  real (kind = prec),intent(inout)  :: u(0:n)
  u(0) = 0
  u(n) = 1

  DO i = 1,n-1
      u(n-i) = x(n-i)

      DO j = 1,i-1
          u(n-i) = u(n-i) - A(n-i,n-i+j)*u(n-i+j)
      end do
  end do

end subroutine

subroutine writeAx(n,A,x)
  implicit none
  integer,parameter :: prec = 4
  integer (kind = 4) :: i
  integer (kind = 4),intent(in) :: n
  real (kind = prec), intent(in) :: A(n-1,n-1)
  real (kind = prec),intent(in)  :: x(n-1)

  WRITE(*,*) x
  WRITE(*,*) "A"
  Do i = 1,n-1
    WRITE(*,*) A(i,:)
  end DO
end subroutine

program main
implicit none
  integer :: args_count,first_argument,length,status
  integer,parameter :: prec = 4
  integer :: prec2
  character(100) value
  integer (kind = 4) :: n = 6
  real (kind = prec), allocatable :: A(:,:)
  real (kind = prec), allocatable :: x(:),u(:)
  integer (kind = 4) :: i,j
  real (kind = prec) :: err

  call get_command_argument(1,value,length,status)
  read(value,*) prec2
  WRITE(*,*) prec2

  allocate (A(n-1,n-1))
  allocate (x(n-1))
  allocate (u(0:n))

  call declareX (n,x)
  call declareA (n,A)




call writeAx(n,A,x)

call  gaussAX (n,A,x)


  call    writeAx(n,A,x)
call  gaussAX_normalise (n,A,x)




      WRITE(*,*) 'after gauss2'

      WRITE(*,*) x
      WRITE(*,*) "A"
      Do i = 1,n-1

        WRITE(*,*) A(i,:)
      end DO
  call gaussAX_normalised_resolve(n,A,x,u)

    WRITE(*,*) 'after gauss reslove'

    WRITE(*,*) u
    err = 0

    DO i = 1,n-1
      WRITE(*,*) (i/n)
      WRITE(*,*) u(i)
      err = err + ABS( u(i)-( real (i, kind = prec) / real (n, kind = prec) ) )
    end do
    WRITE(*,*) err

end
