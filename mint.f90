module mod_mint
  implicit none
  integer(8) :: modul
  type :: mint
    integer(8) :: n
  end type mint
  interface assignment(=)
    module procedure assign1, assign2, assign3_1, assign3_2, assign4_1, assign4_2
  end interface assignment(=)
  interface operator(+)
    module procedure add0, add1_1, add1_2, add2_1, add2_2
  end interface operator(+)
  interface operator(-)
    module procedure sub0, sub1_1, sub1_2, sub2_1, sub2_2
  end interface operator(-)
  interface operator(*)
    module procedure mul0, mul1_1, mul1_2, mul2_1, mul2_2
  end interface operator(*)
  interface operator(/)
    module procedure div0, div1_1, div1_2, div2_1, div2_2
  end interface operator(/)
  interface operator(**)
    module procedure pow1_1, pow1_2
  end interface operator(**)
  private
  public :: mint
  public :: init, show, inv
  public :: assignment(=), operator(+), operator(-)
  public :: operator(*), operator(/), operator(**)
contains
  subroutine init(m)
    implicit none
    integer(8), intent(in) :: m
    modul = m
    return
  end subroutine init
  subroutine show(a)
    implicit none
    type(mint), intent(in) :: a
    write(*,'(i0)') a%n
    return
  end subroutine show
  subroutine assign1(a,b)
    implicit none
    type(mint), intent(inout) :: a
    integer(8), intent(in) :: b
    integer(8) :: c
    c = b
    do while (c.lt.0_8)
      c = c + modul
    end do
    a%n = mod(c,modul)
    return
  end subroutine assign1
  subroutine assign2(a,b)
    implicit none
    type(mint), intent(inout) :: a
    integer, intent(in) :: b
    call assign1(a,int(b,8))
    return
  end subroutine assign2
  subroutine assign3_1(a,b)
    implicit none
    type(mint), intent(inout) :: a(:)
    integer(8), intent(in) :: b
    integer :: n, i
    n = size(a)
    do i = 1, n
      call assign1(a(i),b)
    end do
    return
  end subroutine assign3_1
  subroutine assign3_2(a,b)
    implicit none
    type(mint), intent(inout) :: a(:)
    integer, intent(in) :: b
    call assign3_1(a,int(b,8))
    return
  end subroutine assign3_2
  subroutine assign4_1(a,b)
    implicit none
    type(mint), intent(inout) :: a(:,:)
    integer(8), intent(in) :: b
    integer :: n, i
    n = size(a(:,1))
    do i = 1, n
      call assign3_1(a(i,:),b)
    end do
    return
  end subroutine assign4_1
  subroutine assign4_2(a,b)
    implicit none
    type(mint), intent(inout) :: a(:,:)
    integer, intent(in) :: b
    call assign4_1(a,int(b,8))
    return
  end subroutine assign4_2
  function add0(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a, b
    type(mint) :: r
    r%n = mod(a%n+b%n,modul)
    return
  end function add0
  function add1_1(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer(8), intent(in) :: b
    type(mint) :: r
    call assign1(r,b)
    r = add0(a,r)
    return
  end function add1_1
  function add1_2(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer, intent(in) :: b
    type(mint) :: r
    call assign2(r,b)
    r = add0(a,r)
    return
  end function add1_2
  function add2_1(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign1(r,a)
    r = add0(r,b)
    return
  end function add2_1
  function add2_2(a,b) result(r)
    implicit none
    integer, intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign2(r,a)
    r = add0(r,b)
    return
  end function add2_2
  function sub0(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a, b
    type(mint) :: r
    r%n = mod(modul+a%n-b%n,modul)
    return
  end function sub0
  function sub1_1(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer(8), intent(in) :: b
    type(mint) :: r
    call assign1(r,b)
    r = sub0(a,r)
    return
  end function sub1_1
  function sub1_2(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer, intent(in) :: b
    type(mint) :: r
    call assign2(r,b)
    r = sub0(a,r)
    return
  end function sub1_2
  function sub2_1(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign1(r,a)
    r = sub0(r,b)
    return
  end function sub2_1
  function sub2_2(a,b) result(r)
    implicit none
    integer, intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign2(r,a)
    r = sub0(r,b)
    return
  end function sub2_2
  function mul0(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a, b
    type(mint) :: r
    r%n = mod(a%n*b%n,modul)
    return
  end function mul0
  function mul1_1(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer(8), intent(in) :: b
    type(mint) :: r
    call assign1(r,b)
    r = mul0(a,r)
    return
  end function mul1_1
  function mul1_2(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer, intent(in) :: b
    type(mint) :: r
    call assign2(r,b)
    r = mul0(a,r)
    return
  end function mul1_2
  function mul2_1(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign1(r,a)
    r = mul0(r,b)
    return
  end function mul2_1
  function mul2_2(a,b) result(r)
    implicit none
    integer, intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign2(r,a)
    r = mul0(r,b)
    return
  end function mul2_2
  function div0(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a, b
    type(mint) :: r
    r = mul0(a,inv(b))
    return
  end function div0
  function div1_1(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer(8), intent(in) :: b
    type(mint) :: r
    call assign1(r,b)
    r = div0(a,r)
    return
  end function div1_1
  function div1_2(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer, intent(in) :: b
    type(mint) :: r
    call assign2(r,b)
    r = div0(a,r)
    return
  end function div1_2
  function div2_1(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign1(r,a)
    r = div0(r,b)
    return
  end function div2_1
  function div2_2(a,b) result(r)
    implicit none
    integer, intent(in) :: a
    type(mint), intent(in) :: b
    type(mint) :: r
    call assign2(r,a)
    r = div0(r,b)
    return
  end function div2_2
  function inv(n) result(m)
    implicit none
    type(mint), intent(in) :: n
    type(mint) :: m
    integer(8) :: a, b, x, y, t, q
    a = n%n
    b = modul
    x = 0
    y = 1
    do while(b.ne.0_8)
      q = a/b
      t = b
      b = mod(a,b)
      a = t
      t = y
      y = x
      x = t-q*x
    end do
    call assign1(m,y)
    return
  end function inv
  recursive function pow1_1(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer(8), intent(in) :: b
    type(mint) :: r
    if (b.eq.0_8) then
      call assign2(r,1)
      return
    end if
    if (mod(b,2_8).eq.0_8) then
      r = pow1_1(a,b/2_8)
      r = mul0(r,r)
      return
    end if
    r = mul0(a,pow1_1(a,b-1_8))
    return
  end function pow1_1
  function pow1_2(a,b) result(r)
    implicit none
    type(mint), intent(in) :: a
    integer, intent(in) :: b
    type(mint) :: r
    r = pow1_1(a,int(b,8))
    return
  end function pow1_2
end module mod_mint
