

program main
  use heat_transport_test
  implicit none

  integer,parameter :: prec = 4
  integer,parameter :: max_n = 1000
  integer :: args_count,first_argument,length,status

  integer :: prec2,i
  character(100) value

  integer (kind = 4) :: n
  real (kind = prec) :: err(100)

  call get_command_argument(1,value,length,status)
  read(value,*) prec2
  !WRITE(*,*) prec2
  i = 0

  select case(prec)
    case(8)
      open(unit = 7, file = "../stats/stats_8.dat")
    case(16)
      open(unit = 7, file = "../stats/stats_16.dat")
    case default
      open(unit = 7, file = "../stats/stats_4.dat")
  end select

  DO n = 5,max_n,10
    i = i + 1
    err(i) =  makeSimulation(n)
    WRITE(7,*) n, err(i)
  end do
  
  close(7)

end
