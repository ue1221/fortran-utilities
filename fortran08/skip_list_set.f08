module mod_skip_list_set
  implicit none
  integer, private, parameter :: intkind = 4
  real(8), private, parameter :: threshold = 0.5d0
  integer(intkind), private, parameter :: infty = lshift(1_intkind, 8 * intkind - 2)
  type t_node
    private
    integer(intkind) :: key
    integer :: length = 0
    type(t_node), pointer :: prev => null(), next => null()
    type(t_node), pointer :: above => null(), below => null()
  end type
  type t_skip_list_set
    integer :: size = 0
    integer :: level = 0
    type(t_node), private, pointer :: head => null()
    type(t_node), private, pointer :: tail => null()
  contains
    procedure :: contains => contain
    procedure :: insert => insert
    procedure :: index_of => index_of
    procedure :: get_at => get_at
    procedure :: remove => remove
    procedure :: remove_at => remove_at
    procedure :: first => first_key
    procedure :: poll_first => poll_first
    procedure :: last => last_key
    procedure :: poll_last => poll_last
    procedure :: floor => floor_key
    procedure :: lower => lower_key
    procedure :: ceiling => ceiling_key
    procedure :: higher => higher_key
    final :: finalize
  end type
  interface skip_list_set
    module procedure :: newslm0
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
  function new_node(key, length) result(res)
    integer(intkind), intent(in) :: key
    integer, intent(in) :: length
    type(t_node), pointer :: res
    allocate(res)
    res%key = key
    res%length = length
  end
  logical function less(key1, key2) result(res)
    integer(intkind), intent(in) :: key1, key2
    res = key1 < key2
  end
  subroutine finalize(this)
    type(t_skip_list_set), intent(inout) :: this
    call clear(this)
  end
  subroutine clear(this)
    class(t_skip_list_set), intent(inout) :: this
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
  type(t_skip_list_set) function newslm0() result(res)
    type(t_node), pointer :: head, tail
    call random_seed_clock()
    head => new_node(-infty, 1)
    tail => new_node(infty, infty)
    head%next => tail
    tail%next => head
    res%head => head
    res%tail => tail
  end
  subroutine increase_level(this, level)
    type(t_skip_list_set), intent(inout) :: this
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
      habove => new_node(-infty, 1)
      head%above => habove
      habove%below => head
      tabove => new_node(infty, infty)
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
    type(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
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
  logical function contain(this, key) result(res)
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%key == key
  end
  subroutine insert(this, key)
    class(t_skip_list_set), intent(inout) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node, prev, next, above
    integer :: i, level, length, prevlength
    prev => search_node(this, key)
    if (prev%key == key) return
    this%size = this%size + 1
    node => new_node(key, 1)
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
      above => new_node(key, length)
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
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
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
  integer(intkind) function get_at(this, idx) result(res)
    class(t_skip_list_set), intent(in) :: this
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
    class(t_skip_list_set), intent(inout) :: this
    integer(intkind), intent(in) :: key
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
  subroutine remove_at(this, idx)
    class(t_skip_list_set), intent(inout) :: this
    integer, intent(in) :: idx
    integer :: key
    if (idx < 1 .or. idx > this%size) return
    key = get_at(this, idx)
    call remove(this, key)
  end
  integer(intkind) function first_key(this) result(res)
    class(t_skip_list_set), intent(in) :: this
    res = merge(-infty, this%head%next%key, this%size == 0)
  end
  integer(intkind) function poll_first(this) result(res)
    class(t_skip_list_set), intent(inout) :: this
    type(t_node), pointer :: node
    res = merge(-infty, this%head%next%key, this%size == 0)
    if (this%size > 0) call remove(this, res)
  end
  integer(intkind) function last_key(this) result(res)
    class(t_skip_list_set), intent(in) :: this
    res = merge(infty, this%tail%prev%key, this%size == 0)
  end
  integer(intkind) function poll_last(this) result(res)
    class(t_skip_list_set), intent(inout) :: this
    res = merge(infty, this%tail%prev%key, this%size == 0)
    if (this%size > 0) call remove(this, res)
  end
  integer(intkind) function floor_key(this, key) result(res)
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%key
  end
  integer(intkind) function lower_key(this, key) result(res)
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = merge(node%prev%key, node%key, node%key == key)
  end
  integer(intkind) function ceiling_key(this, key) result(res)
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = merge(node%key, node%next%key, node%key == key)
  end
  integer(intkind) function higher_key(this, key) result(res)
    class(t_skip_list_set), intent(in) :: this
    integer(intkind), intent(in) :: key
    type(t_node), pointer :: node
    node => search_node(this, key)
    res = node%next%key
  end
end module mod_skip_list_set