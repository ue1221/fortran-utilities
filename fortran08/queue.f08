module mod_queue
  implicit none

  type t_node
    private
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_queue
    private
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  contains
    procedure :: enqueue => enqueue
    procedure :: dequeue => dequeue
    procedure :: peek => peek
    procedure :: clear => clear
    procedure :: size => size_of
    final :: finalize
  end type t_queue

contains

  subroutine finalize(this)
    type(t_queue), intent(inout) :: this

    call clear(this)
  end

  function new_node(item) result(node)
    integer, intent(in) :: item
    type(t_node), pointer :: node

    allocate(node)
    node%item = item
  end

  subroutine enqueue(this,item)
    class(t_queue), intent(inout) :: this
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(this%head)) then
      node%prev => this%tail
      this%tail%next => node
    else
      this%head => node
    end if
    this%tail => node
    this%num = this%num+1
  end

  function dequeue(this) result(item)
    class(t_queue), intent(inout) :: this
    integer :: item
    type(t_node), pointer :: node

    item = this%head%item
    node => this%head%next
    deallocate(this%head)
    this%head => node
    if (associated(node)) then
      node%prev => null()
    else
      this%tail => null()
    end if
    this%num = this%num-1
  end

  function peek(this) result(item)
    class(t_queue), intent(in) :: this
    integer :: item

    item = this%head%item
  end

  subroutine clear(this)
    class(t_queue), intent(inout) :: this
    type(t_node), pointer :: node, next

    if (.not.associated(this%head)) return
    node => this%head
    do while (associated(node%next))
      next => node%next
      deallocate(node)
      node => next
    end do
    this%head => null()
    this%tail => null()
    this%num = 0
  end

  integer function size_of(this)
    class(t_queue), intent(in) :: this

    size_of = this%num
  end

end module mod_queue
