module mod_int_queue
  implicit none
  type :: int_node
    integer :: x, y
    type(int_node), pointer :: prev
    type(int_node), pointer :: next
  end type int_node
  type :: int_queue
    integer :: n
    type(int_node), pointer :: head
    type(int_node), pointer :: tail
  end type int_queue
  private
  public :: int_queue, init_queue, release_queue, enqueue, dequeue
contains
  function new_node(x,y) result(node)
    implicit none
    integer, intent(in) :: x, y
    type(int_node), pointer :: node
    allocate(node)
    nullify(node%prev)
    nullify(node%next)
    node%x = x
    node%y = y
    return
  end function new_node
  subroutine delete_node(node)
    implicit none
    type(int_node), pointer :: node
    deallocate(node)
    return
  end subroutine delete_node
  subroutine init_queue(queue)
    implicit none
    type(int_queue), intent(inout) :: queue
    queue%n = 0
    nullify(queue%head)
    nullify(queue%tail)
    return
  end subroutine init_queue
  subroutine release_queue(queue)
    implicit none
    type(int_queue), intent(inout) :: queue
    type(int_node), pointer :: now, next
    now => queue%head
    do while(associated(now))
      next => now%next
      call delete_node(now)
      now => next
    end do
    return
  end subroutine release_queue
  subroutine enqueue(queue,x,y)
    implicit none
    type(int_queue), intent(inout) :: queue
    integer, intent(in) :: x, y
    type(int_node), pointer :: node
    node => new_node(x,y)
    if (associated(queue%head)) then
      node%prev => queue%tail
      queue%tail%next => node
      queue%tail => node
    else
      queue%head => node
      queue%tail => node
    end if
    queue%n = queue%n+1
    return
  end subroutine enqueue
  function dequeue(queue) result(z)
    implicit none
    type(int_queue), intent(inout) :: queue
    type(int_node), pointer :: node
    integer :: z(2)
    z = (/queue%head%x,queue%head%y/)
    node => queue%head%next
    call delete_node(queue%head)
    queue%head => node
    if (associated(node)) then
      nullify(node%prev)
    else
      nullify(queue%tail)
    end if
    queue%n = queue%n-1
    return
  end function dequeue
end module mod_int_queue
