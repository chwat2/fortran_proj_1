
subroutine declareX (n,x)
  implicit none
  integer,parameter :: prec = 16
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
  integer,parameter :: prec = 16
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
  integer,parameter :: prec = 16
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
  integer,parameter :: prec = 16
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
  integer,parameter :: prec = 16
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
  integer,parameter :: prec = 16
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

function makeSimulation (n) result (errSum)
  implicit none
  integer,parameter :: prec = 16
  integer (kind = 4),intent(in) :: n
  real (kind = prec) :: errSum
  real (kind = prec), allocatable :: A(:,:)
  real (kind = prec), allocatable :: x(:),u(:)
  integer (kind = 4) :: i,j

  allocate (A(n-1,n-1))
  allocate (x(n-1))
  allocate (u(0:n))
  WRITE(*,*) n
  call declareX (n,x)
  !call    writeAx(n,A,x)
  call declareA (n,A)
  !call    writeAx(n,A,x)
  call  gaussAX (n,A,x)
  !call    writeAx(n,A,x)
  call  gaussAX_normalise (n,A,x)
  !call    writeAx(n,A,x)
  call gaussAX_normalised_resolve(n,A,x,u)
  !call    writeAx(n,A,x)
  !WRITE(*,*) u
    errSum = 0
    DO i = 1,n-1
      errSum = errSum + ABS( u(i)-( real (i, kind = prec) / real (n, kind = prec) ) ) !explicit shadowing
    end do
    if (allocated(x)) deallocate(x)
    if (allocated(A)) deallocate(A)
    if (allocated(u)) deallocate(u)
end function

program main
implicit none

  integer,parameter :: prec = 16
  integer,parameter :: max_n = 1000
  real(kind=prec),external :: makeSimulation
  integer :: args_count,first_argument,length,status

  integer :: prec2,i
  character(100) value

  integer (kind = 4) :: n
  real (kind = prec) :: err(100)

  call get_command_argument(1,value,length,status)
  read(value,*) prec2
  !WRITE(*,*) prec2
  i = 0
  open(unit = 7, file = "../stats/stats_16.dat")
  DO n = 5,max_n,10
    i = i + 1
    err(i) =  makeSimulation(n)
    WRITE(7,*) n, err(i)
  end do
  close(7)

end
