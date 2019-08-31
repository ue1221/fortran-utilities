module mod_test_jag_array_list
  implicit none

  type jagarray
    private
    integer :: num = 0
    integer, allocatable :: arr(:)
  contains
    procedure :: add => addj
    procedure :: clear => clearj
  end type

  type arraylist
    private
    integer :: num = 0
    integer, pointer :: arr(:) => null()
  contains
    procedure :: add => addl
    procedure :: clear => clearl
  end type

contains

  subroutine addj(jag,n)
    class(jagarray), intent(inout) :: jag
    integer, intent(in) :: n
    if (jag%num == 0) jag%arr = [integer ::]
    jag%arr = [jag%arr,n]
    jag%num = jag%num+1
  end

  subroutine clearj(jag)
    class(jagarray), intent(inout) :: jag
    jag%num = 0
  end

  subroutine append(a)
    integer, pointer, intent(inout) :: a(:)
    integer :: n
    integer, allocatable :: tmp(:)
    n = size(a)
    allocate(tmp(n))
    tmp = a
    deallocate(a)
    allocate(a(2*n))
    a(1:n) = tmp
    deallocate(tmp)
  end

  subroutine addl(list,n)
    class(arraylist), intent(inout) :: list
    integer, intent(in) :: n
    if (.not.associated(list%arr)) allocate(list%arr(1))
    if (list%num == size(list%arr)) call append(list%arr)
    list%num = list%num+1
    list%arr(list%num) = n
  end

  subroutine clearl(list)
    class(arraylist), intent(inout) :: list
    list%num = 0
    if (associated(list%arr)) deallocate(list%arr)
  end

end module mod_test_jag_array_list
program test_jag_array_list
  use mod_test_jag_array_list
  implicit none

  integer, parameter :: n = 18
  integer :: m, i, j, k, x
  real(8) :: t1, t2, dt, dtj, dtl
  type(jagarray) :: jag
  type(arraylist) :: list
  write(*,'(3x,a,3x)',advance='no') "length"
  write(*,'(a,3x)',advance='no') "jagarray"
  write(*,'(a,3x)',advance='no') "arraylist"
  write(*,*)
  m = 1
  do i = 0, n
    call cpu_time(t1)
    do j = 1, m
      call jag%add(j)
    end do
    call jag%clear()
    call cpu_time(t2)
    dt = t2-t1
    x = f(dt)
    dt = 0.d0
    do k = 1, x
      call cpu_time(t1)
      do j = 1, m
        call jag%add(j)
      end do
      call jag%clear()
      call cpu_time(t2)
      dt = dt+(t2-t1)
    end do
    dtj = dt/real(x,8)

    call cpu_time(t1)
    do j = 1, m
      call list%add(j)
    end do
    call list%clear()
    call cpu_time(t2)
    dt = t2-t1
    x = f(dt)
    dt = 0.d0
    do k = 1, x
      call cpu_time(t1)
      do j = 1, m
        call list%add(j)
      end do
      call list%clear()
      call cpu_time(t2)
      dt = dt+(t2-t1)
    end do
    dtl = dt/real(x,8)

    write(*,'(i10,x)',advance='no') m
    write(*,'(e12.3,x)',advance='no') dtj
    write(*,'(e12.3,x)',advance='no') dtl
    write(*,*)
    m = 2*m
  end do

contains

  integer function f(t)
    real(8), intent(in) :: t
    if (t < 1.e-8) then
      f = 20000
    else if (t < 1.e-3) then
      f = 1000
    else if (t < 5.d0) then
      f = ceiling(5.d0/t)
    else
      f = 1
    end if
  end

end program test_jag_array_list
