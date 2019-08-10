module mod_stack
  implicit none

  type t_node
    private
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_stack
    private
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  contains
    procedure :: push => push
    procedure :: pop => pop
    procedure :: peek => peek
    procedure :: clear => clear
    procedure :: size => size_of
    final :: finalize
  end type t_stack

contains

  subroutine finalize(this)
    type(t_stack), intent(inout) :: this

    call clear(this)
  end

  function new_node(item) result(node)
    integer, intent(in) :: item
    type(t_node), pointer :: node

    allocate(node)
    node%item = item
  end

  subroutine push(this,item)
    class(t_stack), intent(inout) :: this
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

  function pop(this) result(item)
    class(t_stack), intent(inout) :: this
    integer :: item
    type(t_node), pointer :: node

    item = this%tail%item
    node => this%tail%prev
    deallocate(this%tail)
    this%tail => node
    if (associated(node)) then
      node%next => null()
    else
      this%head => null()
    end if
    this%num = this%num-1
  end

  function peek(this) result(item)
    class(t_stack), intent(in) :: this
    integer :: item

    item = this%tail%item
  end

  subroutine clear(this)
    class(t_stack), intent(inout) :: this
    type(t_node), pointer :: node, prev

    if (.not.associated(this%tail)) return
    node => this%tail
    do while (associated(node%prev))
      prev => node%prev
      deallocate(node)
      node => prev
    end do
    this%head => null()
    this%tail => null()
    this%num = 0
  end

  integer function size_of(this)
    class(t_stack), intent(in) :: this

    size_of = this%num
  end

end module mod_stack
