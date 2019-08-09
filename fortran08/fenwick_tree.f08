module mod_fenwick_tree

  type t_fenwick_tree
    private
    integer :: n, p
    integer, pointer :: arr(:) => null()
  contains
    procedure :: init => init_bit
    procedure :: release => release_bit
    procedure :: add => get_addition
    procedure :: sum => get_summation
    procedure :: range => get_partial_summation
    procedure :: val => get_value
    procedure :: lower_bound => get_lower_bound
    procedure :: update => get_updated
    procedure :: max => get_maximum
  end type t_fenwick_tree

contains

  subroutine init_bit(this,n)
    implicit none
    class(t_fenwick_tree), intent(inout) :: this
    integer, intent(in) :: n
    integer :: p = 1

    this%n = n
    allocate(this%arr(n))
    this%arr = 0
    do while (lshift(p,1) <= n)
      p = lshift(p,1)
    end do
    this%p = p
    return
  end subroutine init_bit

  subroutine release_bit(this)
    implicit none
    class(t_fenwick_tree), intent(inout) :: this

    if (associated(this%arr)) deallocate(this%arr)
    return
  end subroutine release_bit

  subroutine get_addition(this,i,v)
    implicit none
    class(t_fenwick_tree), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: v
    integer :: x

    x = i
    do while (x <= this%n)
      this%arr(x) = this%arr(x)+v
      x = x+and(x,-x)
    end do
    return
  end subroutine get_addition

  function get_summation(this,i) result(s)
    implicit none
    class(t_fenwick_tree), intent(in) :: this
    integer, intent(in) :: i
    integer :: x
    integer :: s

    s = -1
    if (i > this%n) return
    s = 0
    x = i
    do while (x > 0)
      s = s+this%arr(x)
      x = x-and(x,-x)
    end do
    return
  end function get_summation

  function get_partial_summation(this,l,r) result(s)
    implicit none
    class(t_fenwick_tree), intent(in) :: this
    integer, intent(in) :: l, r
    integer :: i, j
    integer :: s

    s = -1
    if (l > r) return
    s = 0
    i = l-1
    j = r
    do while (j > i)
      s = s+this%arr(j)
      j = j-and(j,-j)
    end do
    do while (i > j)
      s = s-this%arr(i)
      i = i-and(i,-i)
    end do
    return
  end function get_partial_summation

  function get_value(this,i) result(v)
    implicit none
    class(t_fenwick_tree), intent(in) :: this
    integer, intent(in) :: i
    integer :: x
    integer :: v

    v = get_partial_summation(this,i,i)
    return
  end function get_value

  function get_lower_bound(this,v) result(x)
    implicit none
    class(t_fenwick_tree), intent(in) :: this
    integer, intent(in) :: v
    integer :: x, k
    integer :: w

    x = 0
    if (v <= 0) return
    k = this%p
    w = v
    do while (k > 0)
      if (x+k <= this%n .and. this%arr(x+k) < w) then
        w = w-this%arr(x+k)
        x = x+k
      end if
      k = rshift(k,1)
    end do
    x = x+1
    return
  end function get_lower_bound

  subroutine get_updated(this,i,v)
    implicit none
    class(t_fenwick_tree), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: v
    integer :: x

    x = i
    do while (x <= this%n)
      if (this%arr(x) < v) this%arr(x) = v
      x = x+and(x,-x)
    end do
    return
  end subroutine get_updated

  function get_maximum(this,i) result(m)
    implicit none
    class(t_fenwick_tree), intent(in) :: this
    integer, intent(in) :: i
    integer :: x, m

    x = i
    m = 0
    do while (x > 0)
      m = max(m,this%arr(x))
      x = x-and(x,-x)
    end do
    return
  end function get_maximum

  function len_of_lis(a) result(m)
    implicit none
    integer, intent(in) :: a(:)
    type(t_fenwick_tree) :: bit
    integer :: m, n, i, t

    n = size(a)
    call init_bit(bit,n)
    m = 0
    do i = 1, n
      t = get_maximum(bit,a(i)-1)+1
      m = max(m,t)
      call get_updated(bit,a(i),t)
    end do
    return
  end function len_of_lis

  function inv_num(a) result(m)
    implicit none
    integer, intent(in) :: a(:)
    type(t_fenwick_tree) :: bit
    integer :: m, n, i, t

    n = size(a)
    call init_bit(bit,n)
    m = 0
    do i = 1, n
      call get_addition(bit,a(i),1)
      m = m+i-get_summation(bit,a(i))
    end do
    return
  end function inv_num

end module mod_fenwick_tree
