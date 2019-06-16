module mod_binary_indexed_tree
  implicit none
  integer :: n
  integer, allocatable :: bit(:)
  private
  public :: initialize, finalize, add, summate, update, maximum
contains
  subroutine initialize(m)
    implicit none
    integer, intent(in) :: m
    n = m
    if (allocated(bit)) deallocate(bit)
    allocate(bit(n))
    bit = 0
    return
  end subroutine initialize
  subroutine finalize()
    implicit none
    if (allocated(bit)) deallocate(bit)
    return
  end subroutine finalize
  subroutine add(i,v)
    implicit none
    integer, intent(in) :: i, v
    integer :: x
    x = i
    do while (x.le.n)
      bit(x) = bit(x)+v
      x = x+and(x,-x)
    end do
    return
  end subroutine add
  function summate(i) result(s)
    implicit none
    integer, intent(in) :: i
    integer :: x, s
    x = i
    s = 0
    do while (x.gt.0)
      s = s+bit(x)
      x = x-and(x,-x)
    end do
    return
  end function summate
  subroutine update(i,v)
    implicit none
    integer, intent(in) :: i, v
    integer :: x
    x = i
    do while (x.le.n)
      if (bit(x).lt.v) bit(x) = v
      x = x+and(x,-x)
    end do
    return
  end subroutine update
  function maximum(i) result(m)
    implicit none
    integer, intent(in) :: i
    integer :: x, m
    x = i
    m = 0
    do while (x.gt.0)
      m = max(m,bit(x))
      x = x-and(x,-x)
    end do
    return
  end function maximum
end module mod_binary_indexed_tree
