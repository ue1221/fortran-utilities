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

  subroutine add(this,item)
    class(t_array_list), intent(inout) :: this
    integer, intent(in) :: item
    integer, allocatable :: tmp(:)

    if (.not.associated(this%arr)) allocate(this%arr(1))
    if (this%num == size(this%arr)) then
      allocate(tmp(this%num))
      tmp = this%arr
      deallocate(this%arr)
      allocate(this%arr(2*this%num))
      this%arr(1:this%num) = tmp
      deallocate(tmp)
    end if

    this%num = this%num+1
    this%arr(this%num) = item
    return
  end

  subroutine clear(this)
    class(t_array_list), intent(inout) :: this

    if (associated(this%arr)) deallocate(this%arr)
    this%num = 0
  end

  function get(this,i) result(item)
    class(t_array_list), intent(in) :: this
    integer, intent(in) :: i
    integer :: item

    item = this%arr(i)
  end

  function remove(this,i) result(item)
    class(t_array_list), intent(inout) :: this
    integer, intent(in) :: i
    integer :: item

    item = this%arr(i)
    this%arr(i:) = this%arr(i+1:)
    this%num = this%num-1
  end

  subroutine replace(this,i,item)
    class(t_array_list), intent(inout) :: this
    integer, intent(in) :: i
    integer :: item

    this%arr(i) = item
  end

  function first_index_of(this,item) result(idx)
    class(t_array_list), intent(in) :: this
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = 1, this%num
      if (this%arr(i) == item) then
        idx = i
        return
      end if
    end do
  end

  function last_index_of(this,item) result(idx)
    class(t_array_list), intent(in) :: this
    integer, intent(in) :: item
    integer :: idx, i

    idx = -1
    do i = this%num, 1, -1
      if (this%arr(i) == item) then
        idx = i
        return
      end if
    end do
  end

  integer function size_of(this)
    class(t_array_list), intent(in) :: this

    size_of = this%num
  end

  subroutine show_all(this)
    type(t_array_list), intent(in) :: this
    integer :: i

    if (.not.associated(this%arr)) return
    write(*,'(i0)',advance='no') this%arr(1)
    do i = 2, this%num
      write(*,'(x,i0)',advance='no') this%arr(i)
    end do
    write(*,*)
  end

end module mod_array_list
