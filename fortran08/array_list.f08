module mod_array_list
  implicit none

  type t_array_list
    integer :: num = 0
    integer, pointer :: arr(:) => null()
  end type t_array_list

contains

  subroutine add(list,item)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: item
    integer, allocatable :: tmp(:)

    if (.not.associated(list%arr)) allocate(list%arr(1))
    if (list%num == size(list%arr)) then
      allocate(tmp(list%num))
      tmp = list%arr
      deallocate(list%arr)
      allocate(list%arr(2*list%num))
      list%arr(1:list%num) = tmp
      deallocate(tmp)
    end if

    list%num = list%num+1
    list%arr(list%num) = item
    return
  end subroutine add

  subroutine clear(list)
    implicit none
    type(t_array_list), intent(inout) :: list

    if (associated(list%arr)) deallocate(list%arr)
    list%num = 0
    return
  end subroutine clear

  function get(list,i) result(item)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item

    item = list%arr(i)
    return
  end function get

  function remove(list,i) result(item)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item

    item = list%arr(i)
    list%arr(i:) = list%arr(i+1:)
    list%num = list%num-1
    return
  end function remove

  subroutine replace(list,i,item)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item

    list%arr(i) = item
    return
  end subroutine replace

  function first_index_of(list,item) result(idx)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = 1, list%num
      if (list%arr(i) == item) then
        idx = i
        return
      end if
    end do
    return
  end function first_index_of

  function last_index_of(list,item) result(idx)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = list%num, 1, -1
      if (list%arr(i) == item) then
        idx = i
        return
      end if
    end do
    return
  end function last_index_of

  subroutine show_all(list)
    implicit none
    type(t_array_list), intent(inout) :: list
    integer :: i

    if (.not.associated(list%arr)) return
    write(*,'(i0)',advance='no') list%arr(1)
    do i = 2, list%num
      write(*,'(x,i0)',advance='no') list%arr(i)
    end do
    write(*,*)
    return
  end subroutine show_all

end module mod_array_list
