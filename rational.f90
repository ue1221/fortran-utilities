module mod_rational
  implicit none
  type rational
    integer(8) :: n, d
  end type rational
  interface assignment(=)
    module procedure assign0, assign1
  end interface assignment(=)
  interface operator(+)
    module procedure add0, add1, add2
  end interface operator(+)
  interface operator(-)
    module procedure sub0, sub1, sub2
  end interface operator(-)
  interface operator(*)
    module procedure mul0, mul1, mul2
  end interface operator(*)
  interface operator(/)
    module procedure div0, div1, div2
  end interface operator(/)
  interface operator(**)
    module procedure pow1
  end interface operator(**)
  interface operator(.gt.)
    module procedure gt0, gt1, gt2
  end interface operator(.gt.)
  interface operator(.ge.)
    module procedure ge0, ge1, ge2
  end interface operator(.ge.)
  interface operator(.eq.)
    module procedure eq0, eq1, eq2
  end interface operator(.eq.)
  interface operator(.le.)
    module procedure le0, le1, le2
  end interface operator(.le.)
  interface operator(.lt.)
    module procedure lt0, lt1, lt2
  end interface operator(.lt.)
  interface operator(.ne.)
    module procedure ne0, ne1, ne2
  end interface operator(.ne.)
  private
  public :: rational, show, gcd, lcm, ratio_of
  public :: trunc, ceil, round, is_int, int_of
  public :: assignment(=), operator(+), operator(-)
  public :: operator(*), operator(/), operator(**)
  public :: operator(.gt.), operator(.ge.), operator(.eq.)
  public :: operator(.le.), operator(.lt.), operator(.ne.)
contains
  subroutine show(a)
    implicit none
    type(rational), intent(in) :: a
    write(*,'(i0,"/",i0)') a%n, a%d
    return
  end subroutine show
  function trunc(a) result(n)
    implicit none
    type(rational), intent(in) :: a
    integer(8) :: n
    n = a%n/a%d
    if (mod(abs(a%n),a%d).eq.0_8) return
    if (a%n.lt.0_8) n = n-1_8
    return
  end function trunc
  function ceil(a) result(n)
    implicit none
    type(rational), intent(in) :: a
    integer(8) :: n
    n = a%n/a%d
    if (mod(abs(a%n),a%d).eq.0_8) return
    if (a%n.gt.0_8) n = n+1_8
    return
  end function ceil
  function round(a) result(n)
    implicit none
    type(rational), intent(in) :: a
    type(rational) :: b
    integer :: n
    call assign0(b,"1/2")
    n = trunc(a+b)
    return
  end function round
  function is_int(a) result(x)
    implicit none
    type(rational), intent(in) :: a
    logical :: x
    x = a%d.eq.1_8
    return
  end function is_int
  function int_of(a) result(n)
    implicit none
    type(rational), intent(in) :: a
    integer(8) :: n
    n = a%n
    return
  end function int_of
  subroutine assign0(r,s)
    implicit none
    type(rational), intent(inout) :: r
    character(*), intent(in) :: s
    integer :: i, l
    integer(8) :: n, d, g
    l = len_trim(s)
    do i = 2, l-1
      if (s(i:i).eq."/") then
        read(s(1:i-1),*) n
        read(s(i+1:l),*) d
        exit
      end if
    end do
    g = gcd(n,d)
    r%n = n/g
    r%d = d/g
    return
  end subroutine assign0
  subroutine assign1(r,a)
    implicit none
    type(rational), intent(inout) :: r
    integer(8), intent(in) :: a
    r%n = a
    r%d = 1_8
    return
  end subroutine assign1
  function ratio_of(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a, b
    type(rational) :: r
    r%n = a
    r%d = b
    return
  end function ratio_of
  function add0(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    integer(8) :: l, g
    l = lcm(a%d,b%d)
    r%n = (a%n*l)/a%d+(b%n*l)/b%d
    r%d = l
    g = gcd(abs(r%n),abs(r%d))
    r%n = r%n/g
    r%d = r%d/g
    return
  end function add0
  function add1(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    call assign1(r,b)
    r = add0(a,r)
    return
  end function add1
  function add2(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    call assign1(r,a)
    r = add0(r,b)
    return
  end function add2
  function sub0(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    integer(8) :: l, g
    l = lcm(a%d,b%d)
    r%n = (a%n*l)/a%d-(b%n*l)/b%d
    r%d = l
    g = gcd(abs(r%n),abs(r%d))
    r%n = r%n/g
    r%d = r%d/g
    return
  end function sub0
  function sub1(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    call assign1(r,b)
    r = sub0(a,r)
    return
  end function sub1
  function sub2(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    call assign1(r,a)
    r = sub0(r,b)
    return
  end function sub2
  function mul0(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    integer(8) :: g
    r%n = a%n*b%n
    r%d = a%d*b%d
    g = gcd(abs(r%n),abs(r%d))
    r%n = r%n/g
    r%d = r%d/g
    return
  end function mul0
  function mul1(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    call assign1(r,b)
    r = mul0(a,r)
    return
  end function mul1
  function mul2(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    call assign1(r,a)
    r = mul0(r,b)
    return
  end function mul2
  function div0(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    r = mul0(a,inv(b))
    return
  end function div0
  function div1(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    call assign1(r,b)
    r = div0(a,r)
    return
  end function div1
  function div2(a,b) result(r)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    call assign1(r,a)
    r = div0(r,b)
    return
  end function div2
  function inv(n) result(m)
    implicit none
    type(rational), intent(in) :: n
    type(rational) :: m
    m%n = n%d
    m%d = n%n
    return
  end function inv
  recursive function pow1(a,b) result(r)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    if (b.eq.0_8) then
      call assign1(r,1_8)
      return
    end if
    if (mod(b,2_8).eq.0_8) then
      r = pow1(a,b/2_8)
      r = mul0(r,r)
      return
    end if
    r = mul0(a,pow1(a,b-1_8))
    return
  end function pow1
  function lcm(x,y) result(z)
    implicit none
    integer(8), intent(in) :: x, y
    integer(8) :: z
    z = (x/gcd(x,y))*y
    return
  end function lcm
  recursive function gcd(x,y) result(z)
    implicit none
    integer(8), intent(in) :: x, y
    integer(8) :: z
    if (x.lt.y) then
      z = gcd(y,x)
      return
    end if
    z = x
    if (y.eq.0_8) return
    z = gcd(y,mod(x,y))
    return
  end function gcd
  function gt0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.gt.0_8
    return
  end function gt0
  function gt1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = gt0(a,r)
    return
  end function gt1
  function gt2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = gt0(r,b)
    return
  end function gt2
  function ge0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.ge.0_8
    return
  end function ge0
  function ge1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = ge0(a,r)
    return
  end function ge1
  function ge2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = ge0(r,b)
    return
  end function ge2
  function eq0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.eq.0_8
    return
  end function eq0
  function eq1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = eq0(a,r)
    return
  end function eq1
  function eq2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = eq0(r,b)
    return
  end function eq2
  function le0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.le.0_8
    return
  end function le0
  function le1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = le0(a,r)
    return
  end function le1
  function le2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = le0(r,b)
    return
  end function le2
  function lt0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.lt.0_8
    return
  end function lt0
  function lt1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = lt0(a,r)
    return
  end function lt1
  function lt2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = lt0(r,b)
    return
  end function lt2
  function ne0(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a, b
    type(rational) :: r
    logical :: x
    r = sub0(a,b)
    x = r%n.ne.0_8
    return
  end function ne0
  function ne1(a,b) result(x)
    implicit none
    type(rational), intent(in) :: a
    integer(8), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,b)
    x = ne0(a,r)
    return
  end function ne1
  function ne2(a,b) result(x)
    implicit none
    integer(8), intent(in) :: a
    type(rational), intent(in) :: b
    type(rational) :: r
    logical :: x
    call assign1(r,a)
    x = ne0(r,b)
    return
  end function ne2
end module mod_rational
