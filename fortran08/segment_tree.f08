module mod_segment_tree
  implicit none

  type t_segment_tree
    private
    integer :: deflt
    integer :: n, p
    integer, pointer :: arr(:) => null()
  contains
    procedure :: init => init_segtree
    procedure :: release => release_segtree
    procedure :: update => update
    procedure :: query => query
    final :: finalize
  end type t_segment_tree

contains

  integer function op(x,y)
    integer, intent(in) :: x, y

    op = min(x,y)
  end

  subroutine init_segtree(this,n,deflt)
    class(t_segment_tree), intent(inout) :: this
    integer, intent(in) :: n
    integer, intent(in) :: deflt
    integer :: p = 1

    do while (p < n)
      p = 2*p
    end do
    this%deflt = deflt
    this%n = n
    this%p = p
    allocate(this%arr(2*p-1))
    this%arr = deflt
  end

  subroutine release_segtree(this)
    class(t_segment_tree), intent(inout) :: this

    if (associated(this%arr)) deallocate(this%arr)
  end

  subroutine finalize(this)
    type(t_segment_tree), intent(inout) :: this

    if (associated(this%arr)) deallocate(this%arr)
  end

  subroutine set_default(this,i)
    class(t_segment_tree), intent(inout) :: this
    integer, intent(in) :: i
    integer :: x

    x = i+this%p-1
    this%arr(x) = this%deflt
    do while (x > 1)
      x = x/2
      this%arr(x) = op(this%arr(2*x),this%arr(2*x+1))
    end do
  end

  subroutine update(this,i,v)
    class(t_segment_tree), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: v
    integer :: x

    x = i+this%p-1
    this%arr(x) = v
    do while (x > 1)
      x = x/2
      this%arr(x) = op(this%arr(2*x),this%arr(2*x+1))
    end do
  end

  integer function query(this,a,b)
    class(t_segment_tree), intent(inout) :: this
    integer, intent(in) :: a, b
    integer :: l, r

    query = this%deflt
    l = a+this%p-1
    r = b+this%p-1
    do while (l <= r)
      if (mod(l,2) == 1) then
        query = op(query,this%arr(l))
        l = l+1
      end if
      if (mod(r,2) == 0) then
        query = op(query,this%arr(r))
        r = r-1
      end if
      l = l/2
      r = r/2
    end do
  end

end module mod_segment_tree
