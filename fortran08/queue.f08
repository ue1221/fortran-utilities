module mod_queue

  type t_node
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_queue
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  end type t_queue

contains

  function new_node(item) result(node)
    implicit none
    integer, intent(in) :: item
    type(t_node), pointer :: node

    allocate(node)
    node%item = item
    return
  end function new_node

  subroutine enqueue(queue,item)
    implicit none
    type(t_queue), intent(inout) :: queue
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(queue%head)) then
      node%prev => queue%tail
      queue%tail%next => node
    else
      queue%head => node
    end if
    queue%tail => node
    queue%num = queue%num+1
    return
  end subroutine enqueue

  function dequeue(queue) result(item)
    implicit none
    type(t_queue), intent(inout) :: queue
    integer :: item
    type(t_node), pointer :: node
    
    item = queue%head%item
    node => queue%head%next
    deallocate(queue%head)
    queue%head => node
    if (associated(node)) then
      node%prev => null()
    else
      queue%tail => null()
    end if
    queue%num = queue%num-1
    return
  end function dequeue

end module mod_queue
