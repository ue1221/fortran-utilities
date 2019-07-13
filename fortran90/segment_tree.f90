module mod_segment_tree
  implicit none
  integer, parameter :: DEFAULT = 1000000000
  type segment_tree
    integer :: n, p
    integer, pointer :: arr(:)
  end type segment_tree
  private
  public :: segment_tree, init_segtree, release_segtree
  public :: update, query
contains
  integer function op(x,y)
    implicit none
    integer :: x, y
    op = min(x,y)
    return
  end function op
  subroutine init_segtree(st,n)
    implicit none
    type(segment_tree) :: st
    integer, intent(in) :: n
    integer :: p
    p = 1
    do while (p.lt.n)
      p = 2*p
    end do
    st%n = n
    st%p = p
    allocate(st%arr(2*p-1))
    st%arr = DEFAULT
    return
  end subroutine init_segtree
  subroutine release_segtree(st)
    implicit none
    type(segment_tree) :: st
    if (associated(st%arr)) deallocate(st%arr)
    return
  end subroutine release_segtree
  subroutine update(st,i,v)
    implicit none
    type(segment_tree) :: st
    integer, intent(in) :: i, v
    integer :: x
    x = i+st%p-1
    st%arr(x) = v
    do while (x.gt.1)
      x = x/2
      st%arr(x) = op(st%arr(2*x),st%arr(2*x+1))
    end do
    return
  end subroutine update
  integer function query(st,a,b)
    type(segment_tree) :: st
    integer, intent(in) :: a, b
    integer :: l, r
    query = DEFAULT
    l = a+st%p-1
    r = b+st%p-1
    do while (l.le.r)
      if (mod(l,2).eq.1) then
        query = op(query,st%arr(l))
        l = l+1
      end if
      if (mod(r,2).eq.0) then
        query = op(query,st%arr(r))
        r = r-1
      end if
      l = l/2
      r = r/2
    end do
    return
  end function query
end module mod_segment_tree
