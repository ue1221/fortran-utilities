module mod_string_stack
  implicit none
  type :: int_node
    character(3) :: s
    type(int_node), pointer :: prev
    type(int_node), pointer :: next
  end type int_node
  type :: string_stack
    integer :: n
    type(int_node), pointer :: head
    type(int_node), pointer :: tail
  end type string_stack
  private
  public :: string_stack, init_stack, release_stack, push, pop
contains
  function new_node(s) result(node)
    implicit none
    character(*), intent(in) :: s
    type(int_node), pointer :: node
    allocate(node)
    nullify(node%prev)
    nullify(node%next)
    node%s = s
    return
  end function new_node
  subroutine delete_node(node)
    implicit none
    type(int_node), pointer :: node
    deallocate(node)
    return
  end subroutine delete_node
  subroutine init_stack(stack)
    implicit none
    type(string_stack), intent(inout) :: stack
    stack%n = 0
    nullify(stack%head)
    nullify(stack%tail)
    return
  end subroutine init_stack
  subroutine release_stack(stack)
    implicit none
    type(string_stack), intent(inout) :: stack
    type(int_node), pointer :: now, next
    now => stack%head
    do while(associated(now))
      next => now%next
      call delete_node(now)
      now => next
    end do
    return
  end subroutine release_stack
  subroutine push(stack,s)
    implicit none
    type(string_stack), intent(inout) :: stack
    character(*), intent(in) :: s
    type(int_node), pointer :: node
    node => new_node(s)
    if (associated(stack%head)) then
      node%prev => stack%tail
      stack%tail%next => node
      stack%tail => node
    else
      stack%head => node
      stack%tail => node
    end if
    stack%n = stack%n+1
    return
  end subroutine push
  function pop(stack) result(s)
    implicit none
    type(string_stack), intent(inout) :: stack
    type(int_node), pointer :: node
    character(3) :: s
    s = stack%tail%s
    node => stack%tail%prev
    call delete_node(stack%tail)
    stack%tail => node
    if (associated(node)) then
      nullify(node%next)
    else
      nullify(stack%head)
    end if
    stack%n = stack%n-1
    return
  end function pop
end module mod_string_stack
