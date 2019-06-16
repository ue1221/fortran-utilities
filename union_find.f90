module mod_union_find
  implicit none
  integer :: n
  integer, allocatable :: par(:), rnk(:), siz(:)
  private
  public :: initialize, finalize, find, same, unite, size_of
contains
  subroutine initialize(m)
    implicit none
    integer, intent(in) :: m
    integer :: i
    n = m
    if (allocated(par)) deallocate(par)
    if (allocated(rnk)) deallocate(rnk)
    if (allocated(siz)) deallocate(siz)
    allocate(par(n),rnk(n),siz(n))
    rnk = 0
    siz = 1
    do i = 1, n
      par(i) = i
    end do
    return
  end subroutine initialize
  subroutine finalize()
    implicit none
    if (allocated(par)) deallocate(par)
    if (allocated(rnk)) deallocate(rnk)
    if (allocated(siz)) deallocate(siz)
    return
  end subroutine finalize
  recursive function find(i) result(j)
    implicit none
    integer, intent(in) :: i
    integer :: j
    if (par(i).ne.i) then
      par(i) = find(par(i))
    end if
    j = par(i)
    return
  end function find
  function same(i,j) result(y)
    implicit none
    integer, intent(in) :: i, j
    logical :: y
    y = find(i).eq.find(j)
  end function same
  subroutine unite(i,j)
    implicit none
    integer, intent(in) :: i, j
    integer :: x, y
    x = find(i)
    y = find(j)
    if (x.eq.y) return
    if (rnk(x).lt.rnk(y)) then
      par(x) = y
      siz(y) = siz(y)+siz(x)
    else
      par(y) = x
      siz(x) = siz(x)+siz(y)
      if (rnk(x).eq.rnk(y)) rnk(x) = rnk(x)+1
    end if
    return
  end subroutine unite
  function size_of(i) result(s)
    implicit none
    integer, intent(in) :: i
    integer :: s
    s = siz(find(i))
  end function size_of
end module mod_union_find
