module mod_binary_indexed_tree
  implicit none
  type fenwick_tree
    integer :: n
    integer, pointer :: arr(:)
  end type fenwick_tree
  private
  public :: fenwick_tree, init_bit, release_bit
  public :: add, summate, update, maximum
contains
  subroutine init_bit(bit,n)
    implicit none
    type(fenwick_tree) :: bit
    integer, intent(in) :: n
    bit%n = n
    allocate(bit%arr(n))
    bit%arr = 0
    return
  end subroutine init_bit
  subroutine release_bit(bit)
    implicit none
    type(fenwick_tree) :: bit
    if (associated(bit%arr)) deallocate(bit%arr)
    return
  end subroutine release_bit
  subroutine add(bit,i,v)
    implicit none
    type(fenwick_tree) :: bit
    integer, intent(in) :: i, v
    integer :: x
    x = i
    do while (x.le.bit%n)
      bit%arr(x) = bit%arr(x)+v
      x = x+and(x,-x)
    end do
    return
  end subroutine add
  function summate(bit,i) result(s)
    implicit none
    type(fenwick_tree) :: bit
    integer, intent(in) :: i
    integer :: x, s
    x = i
    s = 0
    do while (x.gt.0)
      s = s+bit%arr(x)
      x = x-and(x,-x)
    end do
    return
  end function summate
  subroutine update(bit,i,v)
    implicit none
    type(fenwick_tree) :: bit
    integer, intent(in) :: i, v
    integer :: x
    x = i
    do while (x.le.bit%n)
      if (bit%arr(x).lt.v) bit%arr(x) = v
      x = x+and(x,-x)
    end do
    return
  end subroutine update
  function maximum(bit,i) result(m)
    implicit none
    type(fenwick_tree) :: bit
    integer, intent(in) :: i
    integer :: x, m
    x = i
    m = 0
    do while (x.gt.0)
      m = max(m,bit%arr(x))
      x = x-and(x,-x)
    end do
    return
  end function maximum
end module mod_binary_indexed_tree
