module mod_tree_set
  implicit none

  type t_node
    private
    type(t_node), pointer :: left => null(), right => null()
    integer :: level = 1
    integer :: e = 0
  end type

  type t_tree_set
    private
    type(t_node), pointer :: root => null()
    integer :: deflt = -1
  contains
    procedure :: size => size_of
    procedure :: clear => clear
    procedure :: add => add
    procedure :: remove => remove
    procedure :: contain => contain
    procedure :: first => firste
    procedure :: poll_first => poll_first
    procedure :: last => laste
    procedure :: poll_last => poll_last
    procedure :: floor => floore
    procedure :: lower => lowere
    procedure :: ceiling => ceilinge
    procedure :: higher => highere
    procedure :: element_list => element_list
  end type

contains

  function new_node(e) result(n)
    integer, intent(in) :: e
    type(t_node), pointer :: n
    n => null()
    allocate(n)
    n%e = e
  end

  integer function level(n)
    type(t_node), pointer, intent(in) :: n
    level = 0
    if (.not.associated(n)) return
    level = n%level
  end

  logical function is_leaf(n)
    type(t_node), pointer, intent(in) :: n
    is_leaf = associated(n) .and. .not.associated(n%left) .and. &
    & .not.associated(n%right)
  end

  recursive function tree_size(n) result(s)
    type(t_node), pointer, intent(in) :: n
    integer :: s
    s = 0
    if (.not.associated(n)) return
    s = 1+tree_size(n%left)+tree_size(n%right)
  end

  function skew(n) result(l)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: l
    l => n
    if (.not.(associated(n) .and. associated(n%left) .and. &
    &   n%left%level == n%level)) return
    l => n%left
    n%left => l%right
    l%right => n
  end

  function split(n) result(r)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: r
    r => n
    if (.not.(associated(n) .and. associated(n%right) .and. &
    &   associated(n%right%right) .and. n%right%right%level == n%level)) return
    r => n%right
    n%right => r%left
    r%left => n
    r%level = r%level+1
  end

  function predecessor(n) result(p)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: p
    p => null()
    if (.not.associated(n%left)) return
    p => n%left
    do while (associated(p%right))
      p => p%right
    end do
  end

  function successor(n) result(s)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: s
    s => null()
    if (.not.associated(n%right)) return
    s => n%right
    do while (associated(s%left))
      s => s%left
    end do
  end

  recursive function insert(n,e) result(t)
    type(t_node), pointer, intent(in) :: n
    integer, intent(in) :: e
    type(t_node), pointer :: t
    t => new_node(e)
    if (.not.associated(n)) return
    t => n
    if (e < t%e) then
      t%left => insert(t%left,e)
    else if (e > t%e) then
      t%right => insert(t%right,e)
    end if
    t => skew(t)
    t => split(t)
  end

  recursive function delete(n,e) result(t)
    type(t_node), pointer, intent(in) :: n
    integer, intent(in) :: e
    type(t_node), pointer :: t, l
    t => n
    if (.not.associated(n)) return
    if (e < t%e) then
      t%left => delete(t%left,e)
    else if (e > t%e) then
      t%right => delete(t%right,e)
    else
      if (is_leaf(t)) return
      if (.not.associated(t%left)) then
        l => successor(t)
        t%right => delete(t%right,l%e)
        t%e = l%e
      else
        l => predecessor(t)
        t%left => delete(t%left,l%e)
        t%e = l%e
      end if
    end if
    t => decrease_level(t)
    t => skew(t)
    t%right => skew(t%right)
    if (associated(t%right)) t%right%right => skew(t%right%right)
    t => split(t)
    t%right => split(t%right)
  end

  function decrease_level(n) result(t)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: t
    integer :: should_be
    t => n
    should_be = min(level(t%left),level(t%right))+1
    if (t%level > should_be) then
      t%level = should_be
      if (level(t%right) > should_be) t%right%level = should_be
    end if
  end

  recursive subroutine release_tree(t)
    type(t_node), pointer, intent(inout) :: t
    if (.not.associated(t)) return
    call release_tree(t%left)
    call release_tree(t%right)
    deallocate(t)
  end

  recursive subroutine get_element(t,es,num)
    type(t_node), pointer, intent(in) :: t
    integer, intent(inout) :: es(:)
    integer, intent(inout) :: num
    if (.not.associated(t)) return
    call get_element(t%left,es,num)
    num = num+1
    es(num) = t%e
    call get_element(t%right,es,num)
  end

  integer function size_of(this)
    class(t_tree_set), intent(in) :: this
    size_of = tree_size(this%root)
  end

  subroutine clear(this)
    class(t_tree_set), intent(inout) :: this
    call release_tree(this%root)
    this%root => null()
  end

  subroutine set_default(this,deflt)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: deflt
    this%deflt = deflt
  end

  subroutine add(this,e)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    this%root => insert(this%root,e)
  end

  subroutine remove(this,e)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    this%root => delete(this%root,e)
  end

  function contain(this,e) result(ret)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    type(t_node), pointer :: n
    logical :: ret
    ret = .false.
    n => this%root
    do while (associated(n))
      if (e < n%e) then
        n => n%left
      else if (e > n%e) then
        n => n%right
      else
        ret = .true.
        return
      end if
    end do
  end

  function firste(this) result(ret)
    class(t_tree_set), intent(inout) :: this
    type(t_node), pointer :: n
    integer :: ret
    ret = this%deflt
    n => this%root
    if (.not.associated(n)) return
    do while (associated(n%left))
      n => n%left
    end do
    ret = n%e
  end

  function poll_first(this) result(ret)
    class(t_tree_set), intent(inout) :: this
    type(t_node), pointer :: n
    integer :: ret
    ret = firste(this)
    if (ret /= this%deflt) this%root => delete(this%root,ret)
  end

  function laste(this) result(ret)
    class(t_tree_set), intent(inout) :: this
    type(t_node), pointer :: n
    integer :: ret
    ret = this%deflt
    n => this%root
    if (.not.associated(n)) return
    do while (associated(n%right))
      n => n%right
    end do
    ret = n%e
  end

  function poll_last(this) result(ret)
    class(t_tree_set), intent(inout) :: this
    type(t_node), pointer :: n
    integer :: ret
    ret = laste(this)
    if (ret /= this%deflt) this%root => delete(this%root,ret)
  end

  function floore(this,e) result(ret)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    type(t_node), pointer :: n
    integer :: ret
    ret = this%deflt
    n => this%root
    do while (associated(n))
      if (e < n%e) then
        n => n%left
      else if (e > n%e) then
        if (ret == this%deflt) ret = n%e
        if (e-ret > e-n%e) ret = n%e
        n => n%right
      else
        ret = n%e
        return
      end if
    end do
  end

  function lowere(this,e) result(ret)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    integer :: ret
    ret = floore(this,e-1)
  end

  function ceilinge(this,e) result(ret)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    type(t_node), pointer :: n
    integer :: ret
    ret = this%deflt
    n => this%root
    do while (associated(n))
      if (e < n%e) then
        if (ret == this%deflt) ret = n%e
        if (e-ret < e-n%e) ret = n%e
        n => n%left
      else if (e > n%e) then
        n => n%right
      else
        ret = n%e
        return
      end if
    end do
  end

  function highere(this,e) result(ret)
    class(t_tree_set), intent(inout) :: this
    integer, intent(in) :: e
    integer :: ret
    ret = ceilinge(this,e+1)
  end

  subroutine element_list(this,es,num)
    class(t_tree_set), intent(inout) :: this
    integer, intent(out) :: es(:)
    integer, intent(out) :: num
    num = 0
    call get_element(this%root,es,num)
  end

end module mod_tree_set
