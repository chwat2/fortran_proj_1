module heat_transport_test
  implicit none
  private
    integer,parameter,public :: h_prec = 4
    PUBLIC :: makeSimulation
  contains
  subroutine declareX (n,x)
    implicit none
    integer (kind = 4),intent(in) :: n
    real (kind = h_prec) , intent(inout) :: x(n-1)
    integer (kind = 4) :: i

    Do i = 1,n-2
      x(i) = 0
    end DO
    x(n-1) = -1
  end subroutine

  subroutine declareA (n,A)
    implicit none
    integer (kind = 4) :: i
    integer (kind = 4),intent(in) :: n
    real (kind = h_prec), intent(inout) :: A(n-1,n-1)
    real (kind = h_prec),allocatable :: x(:)
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

  subroutine writeAx(n,A,x)
    implicit none
    integer (kind = 4) :: i
    integer (kind = 4),intent(in) :: n
    real (kind = h_prec), intent(in) :: A(n-1,n-1)
    real (kind = h_prec),intent(in)  :: x(n-1)

    WRITE(*,*) x
    WRITE(*,*) "A"
    Do i = 1,n-1
      WRITE(*,*) A(i,:)
    end DO
  end subroutine

  function makeSimulation (n) result (errSum)
    use gauss_utils
    !g_prec = h_prec
    implicit none

    integer (kind = 4),intent(in) :: n
    real (kind = h_prec) :: errSum
    real (kind = h_prec), allocatable :: A(:,:)
    real (kind = h_prec), allocatable :: x(:),u(:)
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
        errSum = errSum + ABS( u(i)-( real (i, kind = h_prec) / real (n, kind = h_prec) ) ) !explicit shadowing
      end do
      if (allocated(x)) deallocate(x)
      if (allocated(A)) deallocate(A)
      if (allocated(u)) deallocate(u)
  end function
end module
