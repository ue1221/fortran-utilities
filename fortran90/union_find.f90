module mod_union_find
  implicit none
  type union_find
    integer :: n
    integer, pointer :: par(:), rnk(:), siz(:)
  end type union_find
  private
  public :: union_find
  public :: init_union_find, release_union_find, find, same, unite, size_of
contains
  subroutine init_union_find(uf,n)
    implicit none
    type(union_find), intent(inout) :: uf
    integer, intent(in) :: n
    integer :: i
    uf%n = n
    allocate(uf%par(n),uf%rnk(n),uf%siz(n))
    uf%rnk = 0
    uf%siz = 1
    do i = 1, n
      uf%par(i) = i
    end do
    return
  end subroutine init_union_find
  subroutine release_union_find(uf)
    implicit none
    type(union_find), intent(inout) :: uf
    if (associated(uf%par)) deallocate(uf%par)
    if (associated(uf%rnk)) deallocate(uf%rnk)
    if (associated(uf%siz)) deallocate(uf%siz)
    return
  end subroutine release_union_find
  recursive function find(uf,i) result(j)
    implicit none
    type(union_find), intent(inout) :: uf
    integer, intent(in) :: i
    integer :: j
    if (uf%par(i).ne.i) then
      uf%par(i) = find(uf,uf%par(i))
    end if
    j = uf%par(i)
    return
  end function find
  function same(uf,i,j) result(y)
    implicit none
    type(union_find), intent(inout) :: uf
    integer, intent(in) :: i, j
    logical :: y
    y = find(uf,i).eq.find(uf,j)
  end function same
  subroutine unite(uf,i,j)
    implicit none
    type(union_find), intent(inout) :: uf
    integer, intent(in) :: i, j
    integer :: x, y
    x = find(uf,i)
    y = find(uf,j)
    if (x.eq.y) return
    if (uf%rnk(x).lt.uf%rnk(y)) then
      uf%par(x) = y
      uf%siz(y) = uf%siz(y)+uf%siz(x)
    else
      uf%par(y) = x
      uf%siz(x) = uf%siz(x)+uf%siz(y)
      if (uf%rnk(x).eq.uf%rnk(y)) uf%rnk(x) = uf%rnk(x)+1
    end if
    return
  end subroutine unite
  function size_of(uf,i) result(s)
    implicit none
    type(union_find), intent(inout) :: uf
    integer, intent(in) :: i
    integer :: s
    s = uf%siz(find(uf,i))
  end function size_of
end module mod_union_find
