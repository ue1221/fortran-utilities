module mod_array_list
  implicit none

  type t_array_list
    private
    integer :: num = 0
    integer, pointer :: arr(:) => null()
  contains
    procedure :: add => add
    procedure :: clear => clear
    procedure :: get => get
    procedure :: remove => remove
    procedure :: replace => replace
    procedure :: first_index_of => first_index_of
    procedure :: last_index_of => last_index_of
    procedure :: size => size_of
    final :: finalize
  end type t_array_list

contains

  subroutine finalize(this)
    type(t_array_list), intent(inout) :: this

    if (associated(this%arr)) deallocate(this%arr)
  end

  subroutine add(list,item)
    class(t_array_list), intent(inout) :: list
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
  end

  subroutine clear(list)
    class(t_array_list), intent(inout) :: list

    if (associated(list%arr)) deallocate(list%arr)
    list%num = 0
  end

  function get(list,i) result(item)
    class(t_array_list), intent(in) :: list
    integer, intent(in) :: i
    integer :: item

    item = list%arr(i)
  end

  function remove(list,i) result(item)
    class(t_array_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item

    item = list%arr(i)
    list%arr(i:) = list%arr(i+1:)
    list%num = list%num-1
  end

  subroutine replace(list,i,item)
    class(t_array_list), intent(inout) :: list
    integer, intent(in) :: i
    integer :: item

    list%arr(i) = item
  end

  function first_index_of(list,item) result(idx)
    class(t_array_list), intent(in) :: list
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = 1, list%num
      if (list%arr(i) == item) then
        idx = i
        return
      end if
    end do
  end

  function last_index_of(list,item) result(idx)
    class(t_array_list), intent(in) :: list
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = list%num, 1, -1
      if (list%arr(i) == item) then
        idx = i
        return
      end if
    end do
  end

  integer function size_of(this)
    class(t_array_list), intent(in) :: this

    size_of = this%num
  end

  subroutine show_all(list)
    type(t_array_list), intent(in) :: list
    integer :: i

    if (.not.associated(list%arr)) return
    write(*,'(i0)',advance='no') list%arr(1)
    do i = 2, list%num
      write(*,'(x,i0)',advance='no') list%arr(i)
    end do
    write(*,*)
  end

end module mod_array_list
