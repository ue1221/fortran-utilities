program sort_check
  use mod_sort
  implicit none
  integer, parameter :: trial = 15
  integer :: i, n
  real(8) :: time
  call random_seed_clock()
  write(*,'(a)',advance='no') "checking quick_sort..."
  call check_sort(quick_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking merge_sort..."
  call check_sort(merge_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking heap_sort..."
  call check_sort(heap_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking comb_sort..."
  call check_sort(comb_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking shell_sort..."
  call check_sort(shell_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking insertion_sort..."
  call check_sort(insertion_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking gnome_sort..."
  call check_sort(gnome_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking shaker_sort..."
  call check_sort(shaker_sort,1000,1000,10)
  write(*,'(a)') "done"
  write(*,'(a)',advance='no') "checking bitonic_sort..."
  call check_sort(bitonic_sort,1000,1000,10)
  write(*,'(a)') "done"

  write(*,'(3x,a,3x)',advance='no') "length"
  write(*,'(a,3x)',advance='no') "quick_sort"
  write(*,'(a,3x)',advance='no') "merge_sort"
  write(*,'(a,3x)',advance='no') "heap_sort"
  write(*,'(a,3x)',advance='no') "comb_sort"
  write(*,'(a,3x)',advance='no') "shell_sort"
  write(*,'(a,3x)',advance='no') "insertion_sort"
  write(*,'(a,3x)',advance='no') "gnome_sort"
  write(*,'(a,3x)',advance='no') "shaker_sort"
  write(*,'(a,3x)',advance='no') "bitonic_sort"
  write(*,*)

  n = 1
  do i = 1, trial
    n = 2*n+1
    write(*,'(i10,x)',advance='no') n
    call measure(quick_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(merge_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(heap_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(comb_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(shell_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(insertion_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(gnome_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(shaker_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    call measure(bitonic_sort,n,time)
    write(*,'(e12.3,x)',advance='no') time
    write(*,*)
  end do
contains
  subroutine random_seed_clock()
    implicit none
    integer :: nseed, clock
    integer, allocatable :: seed(:)
    call system_clock(clock)
    call random_seed(size=nseed)
    allocate(seed(nseed))
    seed = clock
    call random_seed(put=seed)
    deallocate(seed)
    return
  end subroutine random_seed_clock
  subroutine randint(x,maxint)
    implicit none
    integer, intent(out) :: x(:)
    integer, intent(in) :: maxint
    real(8) :: r(size(x))
    call random_number(r)
    x = int(maxint*r)
    return
  end subroutine randint
  subroutine measure(f,n,cputime)
    implicit none
    integer, intent(in) :: n
    real(8), intent(out) :: cputime
    interface
      subroutine f(x)
        integer, intent(inout) :: x(:)
      end subroutine f
    end interface
    integer :: x(n)
    integer :: i, m
    real(8) :: t1, t2, dt
    call randint(x,100*n)
    m = 1
    call cpu_time(t1)
    call f(x)
    call cpu_time(t2)
    dt = t2-t1
    if (dt.lt.5.d0) then
      if (dt.lt.1.e-8) then
        m = 20000
      else if(dt.lt.1.e-3) then
        m = 1000
      else
        m = int(5/dt)
      end if
      dt = 0.d0
      do i = 1, m
        call randint(x,100*n)
        call cpu_time(t1)
        call f(x)
        call cpu_time(t2)
        dt = dt+(t2-t1)/real(m,8)
      end do
    end if
    cputime = dt
    return
  end subroutine measure
  subroutine check_sort(f,n,maxint,count)
    implicit none
    integer, intent(in) :: n, maxint, count
    interface
      subroutine f(x)
        integer, intent(inout) :: x(:)
      end subroutine f
    end interface
    integer :: i, j
    integer :: x(n)
    do j = 1, count
      call randint(x, maxint)
      call f(x)
      do i = 1, n-1
        if (x(i).gt.x(i+1)) then
          write(*,'(a)') 'sort failed!'
          stop
        end if
      end do
    end do
    return
  end subroutine check_sort
end program sort_check
