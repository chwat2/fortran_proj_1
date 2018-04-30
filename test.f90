

program main
implicit none


  integer (kind = 4) , parameter :: n = 6
  integer (kind = 4) :: A(n-1,n-1)
  integer (kind = 4) :: x(n-1)
  integer (kind = 4) :: i

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

end
