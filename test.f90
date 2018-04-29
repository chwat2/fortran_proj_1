

program main
implicit none


  integer (kind = 4) , parameter :: n = 6
  integer (kind = 4) :: A(n,n)
  integer (kind = 4) :: x(n)
  integer (kind = 4) :: i

  WRITE(*,*) 'test'

  Do i = 1,n
    x(i) = 0
  end DO
    x(n) = -1
  WRITE(*,*) x

  Do i = 2,n-1
    A(i,:) = x
    A(i,i-1) = 1
    A(i,i+1) = 1
    A(i,i) = -2;
  end DO
  A(1,1) = -2
  A(n,n) = -2
  Do i = 1,n
    WRITE(*,*) A(i,:)
  end DO

end
