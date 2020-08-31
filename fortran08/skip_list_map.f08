module mod_skip_list_map
  implicit none
  integer, private, parameter :: key_kind = 4
  integer, private, parameter :: val_kind = 4
  real(8), private, parameter :: threshold = 0.5d0
  integer(key_kind), private, parameter :: infty = lshift(1_key_kind, 8 * key_kind - 2)
  type t_node
    private
    integer(key_kind) :: key
    integer(val_kind) :: val
    integer :: length = 0
    type(t_node), pointer :: prev => null(), next => null()
    type(t_node), pointer :: above => null(), below => null()
  end type
  type t_skip_list_map
    integer :: size = 0
    integer :: level = 0
    integer(val_kind) :: default = -1
    type(t_node), private, pointer :: head => null()
    type(t_node), private, pointer :: tail => null()
  contains
    procedure :: search => search
    procedure :: contains => contain
    procedure :: insert => insert
    procedure :: index_of => index_of
    procedure :: get_key_at => get_key_at
    procedure :: remove => remove
    procedure :: remove_key_at => remove_key_at
    procedure :: first_key => first_key
    procedure :: poll_first => poll_first
    procedure :: last_key => last_key
    procedure :: poll_last => poll_last
    procedure :: floor_key => floor_key
    procedure :: lower_key => lower_key
    procedure :: ceiling_key => ceiling_key
    procedure :: higher_key => higher_key
    final :: finalize
  end type
  interface skip_list_map
    module procedure :: newslm0, newslm1
  end interface
  private :: t_node, random_seed_clock, new_node, less, finalize, increase_level, search_node
contains
  subroutine random_seed_clock()
    integer :: nseed, clock
    integer, allocatable :: seed(:)
    call system_clock(clock)
    call random_seed(size = nseed)
    allocate(seed(nseed))
    seed = clock
    call random_seed(put = seed)
    deallocate(seed)
  end
  logical function flip_coin() result(res)
    real(8) :: rand
    call random_number(rand)
    res = rand < threshold
  end
  function new_node(key, val, length) result(res)
    integer(key_kind), intent(in) :: key
    integer(val_kind), intent(in) :: val
    integer, intent(in) :: length
    type(t_node), pointer :: res
    allocate(res)
    res%key = key
    res%val = val
    res%length = length
  end
  logical function less(key1, key2) result(res)
    integer(key_kind), intent(in) :: key1, key2
    res = key1 < key2
  end
  subroutine finalize(this)
    type(t_skip_list_map), intent(inout) :: this
    call clear(this)
  end
  subroutine clear(this)
    class(t_skip_list_map), intent(inout) :: this
    type(t_node), pointer :: node, next, above
    if (.not.associated(this%head)) return
    node => this%head
    do while (associated(node%below))
      node => node%below
    end do
    do while (associated(node))
      next => node%next
      do while (associated(node))
        above => node%above
        deallocate(node)
        node => above
      end do
      node => next
    end do
    this%head => null()
    this%tail => null()
    this%size = 0
  end
  type(t_skip_list_map) function newslm0() result(res)
    type(t_node), pointer :: head, tail
    call random_seed_clock()
    head => new_node(-infty, 0, 1)
    tail => new_node(infty, 0, infty)
    head%next => tail
    tail%next => head
    res%head => head
    res%tail => tail
  end
  type(t_skip_list_map) function newslm1(default) result(res)
    integer(val_kind), intent(in) :: default
    res = newslm0()
    res%default = default
  end
  subroutine increase_level(this, level)
    type(t_skip_list_map), intent(inout) :: this
    integer, intent(in) :: level
    type(t_node), pointer :: head, tail, habove, tabove
    integer :: i
    if (this%level >= level) return
    head => this%head
    tail => this%tail
    do i = 1, this%level
      head => head%above
      tail => tail%above
    end do
    do i = this%level + 1, level
      habove => new_node(-infty, 0, 1)
      head%above => habove
      habove%below => head
      tabove => new_node(infty, 0, infty)
      tail%above => tabove
      tabove%below => tail
      head => habove
      tail => tabove
      head%next => tail
      tail%prev => head
    end do
    this%level = level
  end
  function search_node(this, key) result(res)
    type(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: res
    res => this%head
    do while (associated(res%above))
      res => res%above
    end do
    do
      do while (.not.less(key, res%next%key))
        res => res%next
      end do
      if (.not.associated(res%below)) exit
      res => res%below
    end do
  end
  integer(val_kind) function search(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = merge(node%val, this%default, node%key == key)
  end
  logical function contain(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%key == key
  end
  subroutine insert(this, key, val)
    class(t_skip_list_map), intent(inout) :: this
    integer(key_kind), intent(in) :: key
    integer(val_kind), intent(in) :: val
    type(t_node), pointer :: node, prev, next, above
    integer :: i, level, length, prevlength
    prev => search_node(this, key)
    if (prev%key == key) then
      prev%val = val
      return
    end if
    this%size = this%size + 1
    node => new_node(key, val, 1)
    next => prev%next
    prev%next => node
    node%prev => prev
    node%next => next
    next%prev => node
    level = 0
    do while (flip_coin())
      level = level + 1
    end do
    call increase_level(this, level)
    prevlength = 1
    length = 1
    do i = 1, level
      do while (.not.associated(prev%above))
        prev => prev%prev
        prevlength = prevlength + prev%length
      end do
      prev => prev%above
      prev%length = prevlength
      do while (.not.associated(next%above))
        length = length + next%length
        next => next%next
      end do
      next => next%above
      above => new_node(key, val, length)
      above%below => node
      node%above => above
      node => above
      prev%next => node
      node%prev => prev
      node%next => next
      next%prev => node
    end do
    do i = level + 1, this%level
      do while (.not.associated(prev%above))
        prev => prev%prev
      end do
      prev => prev%above
      prev%length = prev%length + 1
    end do
  end
  integer function index_of(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    res = 0
    node => this%head
    do while (associated(node%above))
      node => node%above
    end do
    do
      do while (.not.less(key, node%next%key))
        res = res + node%length
        node => node%next
      end do
      if (node%key == key) exit
      if (.not.associated(node%below)) exit
      node => node%below
    end do
    if (node%key /= key) res = -(res + 1)
  end
  integer(key_kind) function get_key_at(this, idx) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer, intent(in) :: idx
    integer :: length
    type(t_node), pointer :: node
    if (idx < 1) then
      res = -infty
      return
    end if
    if (idx > this%size) then
      res = infty
      return
    end if
    length = 0
    node => this%head
    do while (associated(node%above))
      node => node%above
    end do
    do
      do while (length + node%length <= idx)
        length = length + node%length
        node => node%next
      end do
      if (length == idx) exit
      if (.not.associated(node%below)) exit
      node => node%below
    end do
    res = node%key
  end
  subroutine remove(this, key)
    class(t_skip_list_map), intent(inout) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node, prev, next, above
    integer :: i, level
    node => search_node(this, key)
    if (node%key /= key) return
    this%size = this%size - 1
    level = 0
    prev => node%prev
    next => node%next
    prev%next => next
    next%prev => prev
    do
      above => node%above
      deallocate(node)
      node => above
      level = level + 1
      if (.not.associated(node)) exit
      do while (.not.associated(prev%above))
        prev => prev%prev
      end do
      prev => prev%above
      prev%length = prev%length + node%length - 1
      next => node%next
      prev%next => next
      next%prev => prev
    end do
    do i = level, this%level
      do while (.not.associated(prev%above))
        prev => prev%prev
      end do
      prev => prev%above
      prev%length = prev%length - 1
    end do
  end
  subroutine remove_key_at(this, idx)
    class(t_skip_list_map), intent(inout) :: this
    integer, intent(in) :: idx
    integer :: key
    if (idx < 1 .or. idx > this%size) return
    key = get_key_at(this, idx)
    call remove(this, key)
  end
  integer(key_kind) function first_key(this) result(res)
    class(t_skip_list_map), intent(in) :: this
    res = merge(-infty, this%head%next%key, this%size == 0)
  end
  integer(key_kind) function poll_first(this) result(res)
    class(t_skip_list_map), intent(inout) :: this
    type(t_node), pointer :: node
    res = merge(-infty, this%head%next%key, this%size == 0)
    if (this%size > 0) call remove(this, res)
  end
  integer(key_kind) function last_key(this) result(res)
    class(t_skip_list_map), intent(in) :: this
    res = merge(infty, this%tail%prev%key, this%size == 0)
  end
  integer(key_kind) function poll_last(this) result(res)
    class(t_skip_list_map), intent(inout) :: this
    res = merge(infty, this%tail%prev%key, this%size == 0)
    if (this%size > 0) call remove(this, res)
  end
  integer(key_kind) function floor_key(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%key
  end
  integer(key_kind) function lower_key(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = merge(node%prev%key, node%key, node%key == key)
  end
  integer(key_kind) function ceiling_key(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = merge(node%key, node%next%key, node%key == key)
  end
  integer(key_kind) function higher_key(this, key) result(res)
    class(t_skip_list_map), intent(in) :: this
    integer(key_kind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%next%key
  end
end module mod_skip_list_map