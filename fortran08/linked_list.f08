module mod_linked_list

  type t_node
    integer :: item
    type(t_node), pointer :: prev => null()
    type(t_node), pointer :: next => null()
  end type t_node

  type t_linked_list
    integer :: num = 0
    type(t_node), pointer :: head => null()
    type(t_node), pointer :: tail => null()
  end type t_linked_list

contains

  function new_node(item) result(node)
    implicit none
    integer, intent(in) :: item
    type(t_node), pointer :: node

    allocate(node)
    node%item = item
    return
  end function new_node

  subroutine add_first(list,item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(list%tail)) then
      node%next => list%head
      list%head%prev => node
    else
      list%tail => node
    end if
    list%head => node
    list%num = list%num+1
    return
  end subroutine add_first

  subroutine add_last(list,item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: item
    type(t_node), pointer :: node

    node => new_node(item)
    if (associated(list%head)) then
      node%prev => list%tail
      list%tail%next => node
    else
      list%head => node
    end if
    list%tail => node
    list%num = list%num+1
    return
  end subroutine add_last

  function poll_first(list) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer :: item
    type(t_node), pointer :: node

    item = list%head%item
    node => list%head%next
    deallocate(list%head)
    list%head => node
    if (associated(node)) then
      node%prev => null()
    else
      list%tail => null()
    end if
    list%num = list%num-1
    return
  end function poll_first

  function poll_last(list) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer :: item
    type(t_node), pointer :: node

    item = list%tail%item
    node => list%tail%prev
    deallocate(list%tail)
    list%tail => node
    if (associated(node)) then
      node%next => null()
    else
      list%head => null()
    end if
    list%num = list%num-1
    return
  end function poll_last

  function peek_first(list) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer :: item

    item = list%head%item
    return
  end function peek_first

  function peek_last(list) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer :: item

    item = list%tail%item
    return
  end function peek_last

  subroutine clear(list)
    implicit none
    type(t_linked_list), intent(inout) :: list
    type(t_node), pointer :: node, next

    if (.not.associated(list%head)) return
    node => list%head
    do while (associated(node%next))
      next => node%next
      deallocate(node)
      node => next
    end do
    list%head => null()
    list%tail => null()
    list%num = 0
    return
  end subroutine clear

  function get(list,i) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item, idx
    type(t_node), pointer :: node

    if (i <= (list%num+1)/2) then
      idx = 1
      node => list%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = list%num
      node => list%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if
    item = node%item
    return
  end function get

  function remove(list,i) result(item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item, idx
    type(t_node), pointer :: node

    if (i == 1) then
      item = poll_first(list)
      return
    end if

    if (i == list%num) then
      item = poll_last(list)
      return
    end if

    if (i <= (list%num+1)/2) then
      idx = 1
      node => list%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = list%num
      node => list%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if

    item = node%item
    node%prev%next => node%next
    node%next%prev => node%prev
    deallocate(node)
    list%num = list%num-1
    return
  end function remove

  subroutine replace(list,i,item)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: i, item
    integer :: idx
    type(t_node), pointer :: node

    if (i <= (list%num+1)/2) then
      idx = 1
      node => list%head
      do while (idx < i)
        node => node%next
        idx = idx+1
      end do
    else
      idx = list%num
      node => list%tail
      do while (idx > i)
        node => node%prev
        idx = idx-1
      end do
    end if
    node%item = item
    return
  end subroutine replace

  subroutine show_all(list)
    implicit none
    type(t_linked_list), intent(inout) :: list
    type(t_node), pointer :: node, next

    if (.not.associated(list%head)) return
    node => list%head
    write(*,'(i0)',advance='no') node%item
    do while (associated(node%next))
      node => node%next
      write(*,'(x,i0)',advance='no') node%item
    end do
    write(*,*)
    return
  end subroutine show_all

  function first_index_of(list,item) result(idx)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: item
    integer :: idx
    type(t_node), pointer :: node

    idx = 1
    node => list%head
    do while (associated(node))
      if (node%item == item) return
      node => node%next
      idx = idx+1
    end do
    idx = -1
    return
  end function first_index_of

  function last_index_of(list,item) result(idx)
    implicit none
    type(t_linked_list), intent(inout) :: list
    integer, intent(in) :: item
    integer :: idx
    type(t_node), pointer :: node

    idx = list%num
    node => list%tail
    do while (associated(node))
      if (node%item == item) return
      node => node%prev
      idx = idx-1
    end do
    idx = -1
    return
  end function last_index_of

end module mod_linked_list
