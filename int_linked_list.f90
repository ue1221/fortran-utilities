module mod_int_linked_list
  implicit none
  type :: int_node
    integer :: intv
    type(int_node), pointer :: prev
    type(int_node), pointer :: next
  end type int_node
  type :: int_list
    type(int_node), pointer :: root
    type(int_node), pointer :: last
  end type int_list
  private
  public :: int_list, init_list, release_list, push_back, pop_back
contains
  subroutine make_node(node,item)
    implicit none
    type(int_node), pointer :: node
    integer, intent(in) :: item
    allocate(node)
    nullify(node%prev)
    nullify(node%next)
    node%intv = item
    return
  end subroutine make_node
  subroutine delete_node(node)
    implicit none
    type(int_node), pointer :: node
    deallocate(node)
    return
  end subroutine delete_node
  subroutine init_list(list)
    implicit none
    type(int_list), intent(inout) :: list
    nullify(list%root)
    nullify(list%last)
    return
  end subroutine init_list
  subroutine release_list(list)
    implicit none
    type(int_list), intent(inout) :: list
    type(int_node), pointer :: now, next
    now => list%root
    do while(associated(now))
      next => now%next
      call delete_node(now)
      now => next
    end do
    return
  end subroutine release_list
  subroutine push_back(list,item)
    implicit none
    type(int_list), intent(inout) :: list
    integer, intent(in) :: item
    type(int_node), pointer :: node
    call make_node(node,item)
    if (associated(list%root)) then
      node%prev => list%last
      list%last%next => node
      list%last => node
    else
      list%root => node
      list%last => node
    end if
    return
  end subroutine push_back
  function pop_back(list) result(item)
    implicit none
    type(int_list), intent(inout) :: list
    type(int_node), pointer :: node
    integer :: item
    item = list%last%intv
    node => list%last%prev
    call delete_node(list%last)
    list%last => node
    if (associated(node)) then
      nullify(node%next)
    else
      nullify(list%root)
    end if
    return
  end function pop_back
end module mod_int_linked_list
