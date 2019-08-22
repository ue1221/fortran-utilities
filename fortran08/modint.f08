module mod_modint
  implicit none
  integer(8) :: modulus = 1000000007_8
  ! integer(8) :: modulus = 998244353_8
  ! integer(8) :: modulus = 1000000009_8

  type modint
    private
    integer(8) :: num
  contains
    procedure :: get => getnum
  end type

  interface change_modulus
    module procedure :: change_modulus64, change_modulus32
  end interface change_modulus

  interface modint
    module procedure :: newm, newi
  end interface modint

  interface assignment(=)
    module procedure :: setm, seti64, seti32
  end interface assignment(=)

  interface operator(+)
    module procedure :: posm, addmm, addmi64, addmi32, addi64m, addi32m
  end interface operator(+)

  interface operator(-)
    module procedure :: negm, submm, submi64, submi32, subi64m, subi32m
  end interface operator(-)

  interface operator(*)
    module procedure :: mulmm, mulmi64, mulmi32, muli64m, muli32m
  end interface operator(*)

  interface operator(/)
    module procedure :: divmm, divmi64, divmi32, divi64m, divi32m
  end interface operator(/)

  interface operator(**)
    module procedure :: powmi64, powmi32
  end interface operator(**)

  interface inv
    module procedure :: invm
  end interface inv

contains

  subroutine change_modulus64(newmod)
    integer(8), intent(in) :: newmod
    if (newmod == 0_8) then
      write(*,'(a)') "Error: Invalid value (newmod == 0). (modint, change_modulus64)"
      stop
    end if
    modulus = newmod
  end

  subroutine change_modulus32(newmod)
    integer, intent(in) :: newmod
    if (newmod == 0) then
      write(*,'(a)') "Error: Invalid value (newmod == 0). (modint, change_modulus32)"
      stop
    end if
    modulus = int(newmod,8)
  end

  integer(8) function getnum(this)
    class(modint), intent(in) :: this
    getnum = this%num
  end

  pure elemental type(modint) function newm()
    newm%num = 0_8
  end

  pure elemental subroutine setm(x,y)
    type(modint), intent(inout) :: x
    type(modint), intent(in) :: y
    x%num = y%num
  end

  pure elemental function posm(x) result(n)
    type(modint), intent(in) :: x
    type(modint) :: n
    n%num = x%num
  end

  pure elemental function negm(x) result(n)
    type(modint), intent(in) :: x
    type(modint) :: n
    n%num = modulus-x%num
  end

  pure elemental function addmm(x,y) result(n)
    type(modint), intent(in) :: x, y
    type(modint) :: n
    n%num = x%num+y%num
    if (n%num >= modulus) n%num = n%num-modulus
  end

  pure elemental function submm(x,y) result(n)
    type(modint), intent(in) :: x, y
    type(modint) :: n
    n%num = x%num-y%num
    if (n%num < 0_8) n%num = n%num+modulus
  end

  pure elemental function mulmm(x,y) result(n)
    type(modint), intent(in) :: x, y
    type(modint) :: n
    n%num = mod(x%num*y%num,modulus)
  end

  impure elemental function invm(x) result(n)
    type(modint), intent(in) :: x
    type(modint) :: n
    integer(8) :: a, b, c, q, r, v
    a = x%num
    if (a == 0_8) then
      write(*,'(a)') "Error: Division by zero (x == 0). (modint, invm)"
      stop
    end if
    b = modulus
    c = 0_8
    v = 1_8
    do while (b /= 0_8)
      q = a/b
      r = mod(a,b)
      a = b
      b = r
      r = c
      c = v-c*q
      v = r
    end do
    n%num = mod(v,modulus)
    if (n%num < 0_8) n%num = n%num+modulus
  end

  impure elemental function divmm(x,y) result(n)
    type(modint), intent(in) :: x, y
    type(modint) :: n
    n = mulmm(x,invm(y))
  end

  !##########################################################################
  !##################### overload with (normal) integer #####################
  !##########################################################################

  impure elemental type(modint) function newi(i)
    class(*), intent(in) :: i
    select type(i)
    type is (integer(8))
      newi%num = i
    type is (integer)
      newi%num = int(i,8)
    type is (integer(2))
      newi%num = int(i,8)
    type is (integer(1))
      newi%num = int(i,8)
    class default
      write(*,'(a)') "Error: Invalid value (i is not integer). (modint, newi)"
      stop
    end select
    newi%num = mod(newi%num,modulus)
    if (newi%num < 0_8) newi%num = newi%num+modulus
  end

  impure elemental subroutine seti64(x,i)
    type(modint), intent(inout) :: x
    integer(8), intent(in) :: i
    call setm(x,newi(i))
  end

  impure elemental subroutine seti32(x,i)
    type(modint), intent(inout) :: x
    integer, intent(in) :: i
    call setm(x,newi(i))
  end

  impure elemental function addmi64(x,i) result(n)
    type(modint), intent(in) :: x
    integer(8), intent(in) :: i
    type(modint) :: n
    n = addmm(x,newi(i))
  end

  impure elemental function addmi32(x,i) result(n)
    type(modint), intent(in) :: x
    integer, intent(in) :: i
    type(modint) :: n
    n = addmm(x,newi(i))
  end

  impure elemental function addi64m(i,y) result(n)
    integer(8), intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = addmm(newi(i),y)
  end

  impure elemental function addi32m(i,y) result(n)
    integer, intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = addmm(newi(i),y)
  end

  impure elemental function submi64(x,i) result(n)
    type(modint), intent(in) :: x
    integer(8), intent(in) :: i
    type(modint) :: n
    n = submm(x,newi(i))
  end

  impure elemental function submi32(x,i) result(n)
    type(modint), intent(in) :: x
    integer, intent(in) :: i
    type(modint) :: n
    n = submm(x,newi(i))
  end

  impure elemental function subi64m(i,y) result(n)
    integer(8), intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = submm(newi(i),y)
  end

  impure elemental function subi32m(i,y) result(n)
    integer, intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = submm(newi(i),y)
  end

  impure elemental function mulmi64(x,i) result(n)
    type(modint), intent(in) :: x
    integer(8), intent(in) :: i
    type(modint) :: n
    n = mulmm(x,newi(i))
  end

  impure elemental function mulmi32(x,i) result(n)
    type(modint), intent(in) :: x
    integer, intent(in) :: i
    type(modint) :: n
    n = mulmm(x,newi(i))
  end

  impure elemental function muli64m(i,y) result(n)
    integer(8), intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = mulmm(newi(i),y)
  end

  impure elemental function muli32m(i,y) result(n)
    integer, intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = mulmm(newi(i),y)
  end

  impure elemental function divmi64(x,i) result(n)
    type(modint), intent(in) :: x
    integer(8), intent(in) :: i
    type(modint) :: n
    n = divmm(x,newi(i))
  end

  impure elemental function divmi32(x,i) result(n)
    type(modint), intent(in) :: x
    integer, intent(in) :: i
    type(modint) :: n
    n = divmm(x,newi(i))
  end

  impure elemental function divi64m(i,y) result(n)
    integer(8), intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = divmm(newi(i),y)
  end

  impure elemental function divi32m(i,y) result(n)
    integer, intent(in) :: i
    type(modint), intent(in) :: y
    type(modint) :: n
    n = divmm(newi(i),y)
  end

  impure elemental function powmi64(x,i) result(n)
    type(modint), intent(in) :: x
    integer(8), intent(in) :: i
    type(modint) :: n, p
    integer(8) :: m
    n = newi(1_8)
    p = x
    m = i
    if (i < 0_8) then
      p = invm(x)
      m = abs(i)
    end if
    do while (m > 0_8)
      if (btest(m,0)) n = mulmm(p,n)
      p = mulmm(p,p)
      m = rshift(m,1)
    end do
  end

  impure elemental function powmi32(x,i) result(n)
    type(modint), intent(in) :: x
    integer, intent(in) :: i
    type(modint) :: n, p
    integer :: m
    n = newi(1_8)
    p = x
    m = i
    if (i < 0) then
      p = invm(x)
      m = abs(i)
    end if
    do while (m > 0)
      if (btest(m,0)) n = mulmm(p,n)
      p = mulmm(p,p)
      m = rshift(m,1)
    end do
  end

end module mod_modint
