module module_modint
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(int64) :: modu = 1000000007_8
  type modint
    integer(int64), private :: val = 0_8
  contains
    procedure, private :: asgn => asgn
    procedure, private :: add => add
    procedure, private :: sub => sub
    procedure, private :: mul => mul
    procedure, private :: div => div
    procedure, private :: pow => pow
    procedure, public :: inv => inv
    procedure, public :: get => get
    generic, public :: assignment(=) => asgn
    generic, public :: operator(+) => add
    generic, public :: operator(-) => sub
    generic, public :: operator(*) => mul
    generic, public :: operator(/) => div
    generic, public :: operator(**) => pow
  end type
  interface modint
    module procedure :: new_modint, new_modint_with_num
  end interface modint
  interface operator(+)
    module procedure :: add_r
  end interface
  interface operator(-)
    module procedure :: sub_r
  end interface
  interface operator(*)
    module procedure :: mul_r
  end interface
  interface operator(/)
    module procedure :: div_r
  end interface
  interface dot_product
    module procedure :: dot_prod
  end interface dot_product
  interface matmul
    module procedure :: matmul1, matmul2, matmul3
  end interface matmul
contains
  pure elemental integer(int64) function to_int64(num) result(ret)
    class(*), intent(in) :: num
    select type (num)
    type is (integer(int64))
      ret = num
    type is (integer(int32))
      ret = int(num, int64)
    type is (integer(int16))
      ret = int(num, int64)
    type is (integer(int8))
      ret = int(num, int64)
    class default
      ret = 0_8
      ret = 1_8/ret
    end select
  end
  pure elemental type(modint) function new_modint()
  end
  pure elemental type(modint) function new_modint_with_num(num) result(ret)
    class(*), intent(in) :: num
    ret%val = to_int64(num)
  end
  pure elemental subroutine asgn(this,other)
    class(modint), intent(inout) :: this
    class(*), intent(in) :: other
    integer(int64) :: val
    select type (other)
    type is (modint)
      this%val = other%val
    class default
      val = to_int64(other)
      if (0_8 <= val .and. val < modu) then
        this%val = val
      else
        this%val = modulo(val,modu)
      end if
    end select
  end
  pure elemental type(modint) function add(this,other) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    integer(int64) :: val
    select type (other)
    type is (modint)
      if (0_8 <= this%val+other%val .and. this%val+other%val < modu) then
        ret%val = this%val+other%val
      else
        ret%val = this%val+other%val-modu
      end if
    class default
      val = to_int64(other)
      if (0_8 <= this%val+val .and. this%val+val < modu) then
        ret%val = this%val+val
      else
        ret%val = modulo(this%val+val,modu)
      end if
    end select
  end
  pure elemental type(modint) function sub(this,other) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    integer(int64) :: val
    select type (other)
    type is (modint)
      if (0_8 <= this%val-other%val .and. this%val-other%val < modu) then
        ret%val = this%val-other%val
      else
        ret%val = this%val-other%val+modu
      end if
    class default
      val = to_int64(other)
      if (0_8 <= this%val-val .and. this%val-val < modu) then
        ret%val = this%val-val
      else
        ret%val = modulo(this%val-val,modu)
      end if
    end select
  end
  pure elemental type(modint) function mul(this,other) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    integer(int64) :: val
    select type (other)
    type is (modint)
      val = other%val
    class default
      val = to_int64(other)
    end select
    if (0_8 <= this%val*val .and. this%val*val < modu) then
      ret%val = this%val*val
    else
      ret%val = modulo(this%val*val,modu)
    end if
  end
  pure elemental type(modint) function div(this,other) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    type(modint) :: tmp
    call asgn(tmp,other)
    call asgn(ret,mul(this,inv(tmp)))
  end
  pure elemental integer(int64) function get(this)
    class(modint), intent(in) :: this
    get = this%val
  end
  pure elemental type(modint) function inv(this)
    class(modint), intent(in) :: this
    integer(int64) :: a, b, c, n
    integer(int64) :: x, y, z, m
    a = this%val
    b = modu
    c = 0_8
    n = 1_8
    do while (b /= 0_8)
      x = b
      y = mod(a,b)
      z = n-a/b*c
      m = c
      a = x
      b = y
      c = z
      n = m
    end do
    call asgn(inv,n)
  end
  pure elemental type(modint) function pow(this,other)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    integer(int64) :: n
    type(modint) :: i
    pow%val = 1_8
    i%val = this%val
    n = to_int64(other)
    do while (n > 0_8)
      if (btest(n,0)) call asgn(pow,mul(pow,i))
      call asgn(i,mul(i,i))
      n = rshift(n,1)
    end do
  end
  pure elemental type(modint) function add_r(other,this) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    call asgn(ret,add(this,other))
  end
  pure elemental type(modint) function sub_r(other,this) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    call asgn(ret,sub(this,other))
  end
  pure elemental type(modint) function mul_r(other,this) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    call asgn(ret,mul(this,other))
  end
  pure elemental type(modint) function div_r(other,this) result(ret)
    class(modint), intent(in) :: this
    class(*), intent(in) :: other
    call asgn(ret,div(this,other))
  end
  pure type(modint) function dot_prod(x,y) result(ret)
    type(modint), intent(in) :: x(:), y(:)
    integer :: i
    if (size(x,1) /= size(y,1)) i = to_int64("")
    do i = 1, size(x,1)
      call asgn(ret,add(ret,mul(x(i),y(i))))
    end do
  end
  pure function matmul1(x,y) result(ret)
    type(modint), intent(in) :: x(:,:), y(:)
    type(modint) :: ret(size(x,1))
    integer :: i
    if (size(x,2) /= size(y,1)) i = to_int64("")
    do i = 1, size(x,1)
      call asgn(ret(i),dot_prod(x(i,:),y))
    end do
  end
  pure function matmul2(x,y) result(ret)
    type(modint), intent(in) :: x(:), y(:,:)
    type(modint) :: ret(size(y,2))
    integer :: i
    if (size(x,1) /= size(y,1)) i = to_int64("")
    do i = 1, size(y,2)
      call asgn(ret(i),dot_prod(x,y(:,i)))
    end do
  end
  pure function matmul3(x,y) result(ret)
    type(modint), intent(in) :: x(:,:), y(:,:)
    type(modint) :: ret(size(x,1),size(y,2))
    integer :: i
    if (size(x,2) /= size(y,1)) i = to_int64("")
    do i = 1, size(x,1)
      call asgn(ret(i,:),matmul2(x(i,:),y))
    end do
  end
end module module_modint
