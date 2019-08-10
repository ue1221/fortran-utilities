module mod_priority_queue
  implicit none

  type t_priority_queue
    private
    integer :: num = 0
    integer, pointer :: heap(:) => null()
  contains
    procedure :: offer => offer
    procedure :: clear => clear
    procedure :: poll => poll
    procedure :: peek => peek
    procedure :: size => size_of
    final :: finalize
  end type t_priority_queue

contains

  integer function compare(a,b)
    integer, intent(in) :: a, b
    compare = a-b
  end

  subroutine finalize(this)
    type(t_priority_queue), intent(inout) :: this

    if (associated(this%heap)) deallocate(this%heap)
  end

  subroutine offer(this,item)
    class(t_priority_queue), intent(inout) :: this
    integer, intent(in) :: item
    integer :: n, i, t
    integer, allocatable :: tmp(:)

    if (.not.associated(this%heap)) allocate(this%heap(1))
    if (this%num == size(this%heap)) then
      allocate(tmp(this%num))
      tmp = this%heap
      deallocate(this%heap)
      allocate(this%heap(2*this%num))
      this%heap(1:this%num) = tmp
      deallocate(tmp)
    end if

    this%num = this%num+1
    this%heap(this%num) = item

    n = this%num
    do while (n > 1)
      i = n/2
      if (compare(this%heap(n),this%heap(i)) < 0) then
        t = this%heap(n)
        this%heap(n) = this%heap(i)
        this%heap(i) = t
      end if
      n = i
    end do
  end

  subroutine clear(this)
    class(t_priority_queue), intent(inout) :: this

    if (associated(this%heap)) deallocate(this%heap)
    this%num = 0
  end

  function poll(this) result(item)
    class(t_priority_queue), intent(inout) :: this
    integer :: item, n, i, j, tmp

    n = this%num
    item = this%heap(1)
    this%heap(1) = this%heap(n)
    this%num = this%num-1

    i = 1
    do while (2*i < n)
      j = 2*i
      if (j+1 < n .and. compare(this%heap(j+1),this%heap(j)) < 0) j = j+1
      if (compare(this%heap(j),this%heap(i)) < 0) then
        tmp = this%heap(j)
        this%heap(j) = this%heap(i)
        this%heap(i) = tmp
      end if
      i = j
    end do
  end

  function peek(this) result(item)
    class(t_priority_queue), intent(in) :: this
    integer :: item

    item = this%heap(1)
  end

  integer function size_of(this)
    class(t_priority_queue), intent(in) :: this

    size_of = this%num
  end

end module mod_priority_queue
