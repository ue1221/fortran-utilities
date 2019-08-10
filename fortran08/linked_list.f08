module mod_linked_list
  implicit none

  type t_node
    private
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_linked_list
    private
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  contains
    procedure :: add_first => add_first
    procedure :: add_last => add_last
    procedure :: poll_first => poll_first
    procedure :: poll_last => poll_last
    procedure :: peek_first => peek_first
    procedure :: peek_last => peek_last
    procedure :: clear => clear
    procedure :: get => get
    procedure :: remove => remove
    procedure :: replace => replace
    procedure :: first_index_of => first_index_of
    procedure :: last_index_of => last_index_of
    procedure :: size => size_of
    final :: finalize
  end type t_linked_list

contains

  subroutine finalize(this)
    type(t_linked_list), intent(inout) :: this

    call clear(this)
  end

  function new_node(item) result(node)
    integer, intent(in) :: item
    type(t_node), pointer :: node

    allocate(node)
    node%item = item
  end

  subroutine add_first(this,item)
    class(t_linked_list), intent(inout) :: this
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(this%tail)) then
      node%next => this%head
      this%head%prev => node
    else
      this%tail => node
    end if
    this%head => node
    this%num = this%num+1
  end

  subroutine add_last(this,item)
    class(t_linked_list), intent(inout) :: this
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

  function poll_first(this) result(item)
    class(t_linked_list), intent(inout) :: this
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

  function poll_last(this) result(item)
    class(t_linked_list), intent(inout) :: this
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

  function peek_first(this) result(item)
    class(t_linked_list), intent(in) :: this
    integer :: item

    item = this%head%item
  end

  function peek_last(this) result(item)
    class(t_linked_list), intent(in) :: this
    integer :: item

    item = this%tail%item
  end

  subroutine clear(this)
    class(t_linked_list), intent(inout) :: this
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

  function get(this,i) result(item)
    class(t_linked_list), intent(in) :: this
    integer, intent(in) :: i
    integer :: item, idx
    type(t_node), pointer :: node

    if (i <= (this%num+1)/2) then
      idx = 1
      node => this%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = this%num
      node => this%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if
    item = node%item
  end

  function remove(this,i) result(item)
    class(t_linked_list), intent(inout) :: this
    integer, intent(in) :: i
    integer :: item, idx
    type(t_node), pointer :: node

    if (i == 1) then
      item = poll_first(this)
      return
    end if

    if (i == this%num) then
      item = poll_last(this)
      return
    end if

    if (i <= (this%num+1)/2) then
      idx = 1
      node => this%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = this%num
      node => this%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if

    item = node%item
    node%prev%next => node%next
    node%next%prev => node%prev
    deallocate(node)
    this%num = this%num-1
  end

  subroutine replace(this,i,item)
    class(t_linked_list), intent(inout) :: this
    integer, intent(in) :: i, item
    integer :: idx
    type(t_node), pointer :: node

    if (i <= (this%num+1)/2) then
      idx = 1
      node => this%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = this%num
      node => this%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if
    node%item = item
  end

  subroutine show_all(this)
    type(t_linked_list), intent(in) :: this
    type(t_node), pointer :: node, next

    if (.not.associated(this%head)) return
    node => this%head
    write(*,'(i0)',advance='no') node%item
    do while (associated(node%next))
      node => node%next
      write(*,'(x,i0)',advance='no') node%item
    end do
    write(*,*)
  end

  function first_index_of(this,item) result(idx)
    class(t_linked_list), intent(in) :: this
    integer, intent(in) :: item
    integer :: idx
    type(t_node), pointer :: node

    idx = 1
    node => this%head
    do while (associated(node))
      if (node%item == item) return
      node => node%next
      idx = idx+1
    end do
    idx = -1
  end

  function last_index_of(this,item) result(idx)
    class(t_linked_list), intent(in) :: this
    integer, intent(in) :: item
    integer :: idx
    type(t_node), pointer :: node

    idx = this%num
    node => this%tail
    do while (associated(node))
      if (node%item == item) return
      node => node%prev
      idx = idx-1
    end do
    idx = -1
  end

  integer function size_of(this)
    class(t_linked_list), intent(in) :: this

    size_of = this%num
  end

end module mod_linked_list
