module mod_stack

  type t_node
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_stack
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  end type t_stack

contains

  function new_node(item) result(node)
    implicit none
    integer, intent(in) :: item
    type(t_node), pointer :: node
    
    allocate(node)
    node%item = item
    return
  end function new_node

  subroutine push(stack,item)
    implicit none
    type(t_stack), intent(inout) :: stack
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(stack%head)) then
      node%prev => stack%tail
      stack%tail%next => node
    else
      stack%head => node
    end if
    stack%tail => node
    stack%num = stack%num+1
    return
  end subroutine push

  function pop(stack) result(item)
    implicit none
    type(t_stack), intent(inout) :: stack
    integer :: item
    type(t_node), pointer :: node

    item = stack%tail%item
    node => stack%tail%prev
    deallocate(stack%tail)
    stack%tail => node
    if (associated(node)) then
      node%next => null()
    else
      stack%head => null()
    end if
    stack%num = stack%num-1
    return
  end function pop

end module mod_stack
