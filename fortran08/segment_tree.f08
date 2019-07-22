module mod_segment_tree

  type t_segment_tree
    integer :: deflt
    integer :: n, p
    integer, pointer :: arr(:) => null()
  end type t_segment_tree

contains

  integer function op(x,y)
    implicit none
    integer :: x, y

    op = min(x,y)
    return
  end function op

  subroutine init_segtree(st,n,deflt)
    implicit none
    type(t_segment_tree), intent(inout) :: st
    integer, intent(in) :: n
    integer, intent(in) :: deflt
    integer :: p = 1

    do while (p < n)
      p = 2*p
    end do
    st%deflt = deflt
    st%n = n
    st%p = p
    allocate(st%arr(2*p-1))
    st%arr = deflt
    return
  end subroutine init_segtree

  subroutine set_default(st,i)
    implicit none
    type(t_segment_tree), intent(inout) :: st
    integer, intent(in) :: i
    integer :: x

    x = i+st%p-1
    st%arr(x) = st%deflt
    do while (x > 1)
      x = x/2
      st%arr(x) = op(st%arr(2*x),st%arr(2*x+1))
    end do
    return
  end subroutine set_default

  subroutine update(st,i,v)
    implicit none
    type(t_segment_tree), intent(inout) :: st
    integer, intent(in) :: i
    integer, intent(in) :: v
    integer :: x

    x = i+st%p-1
    st%arr(x) = v
    do while (x > 1)
      x = x/2
      st%arr(x) = op(st%arr(2*x),st%arr(2*x+1))
    end do
    return
  end subroutine update

  integer function query(st,a,b)
    implicit none
    type(t_segment_tree), intent(inout) :: st
    integer, intent(in) :: a, b
    integer :: l, r

    query = st%deflt
    l = a+st%p-1
    r = b+st%p-1
    do while (l <= r)
      if (mod(l,2) == 1) then
        query = op(query,st%arr(l))
        l = l+1
      end if
      if (mod(r,2) == 0) then
        query = op(query,st%arr(r))
        r = r-1
      end if
      l = l/2
      r = r/2
    end do
    return
  end function query

end module mod_segment_tree
