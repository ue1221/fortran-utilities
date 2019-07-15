module mod_priority_queue
  implicit none

  type t_priority_queue
    integer :: num = 0
    integer, pointer :: heap(:) => null()
  end type t_priority_queue

contains

  subroutine offer(pq,item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    integer, intent(in) :: item
    integer :: n, i, t
    integer, allocatable :: tmp(:)

    if (.not.associated(pq%heap)) allocate(pq%heap(1))
    if (pq%num == size(pq%heap)) then
      allocate(tmp(pq%num))
      tmp = pq%heap
      deallocate(pq%heap)
      allocate(pq%heap(2*pq%num))
      pq%heap(1:pq%num) = tmp
      deallocate(tmp)
    end if

    pq%num = pq%num+1
    pq%heap(pq%num) = item

    n = pq%num
    do while (n > 1)
      i = n/2
      if (pq%heap(n) < pq%heap(i)) then
        t = pq%heap(n)
        pq%heap(n) = pq%heap(i)
        pq%heap(i) = t
      end if
      n = i
    end do
    return
  end subroutine offer

  subroutine clear(pq)
    implicit none
    type(t_priority_queue), intent(inout) :: pq

    if (associated(pq%heap)) deallocate(pq%heap)
    pq%num = 0
    return
  end subroutine clear

  function poll(pq) result(item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    integer :: item, n, i, j, tmp

    n = pq%num
    item = pq%heap(1)
    pq%heap(1) = pq%heap(n)
    pq%num = pq%num-1

    i = 1
    do while (2*i < n)
      j = 2*i
      if (j+1 < n .and. pq%heap(j+1) < pq%heap(j)) j = j+1
      if (pq%heap(j) < pq%heap(i)) then
        tmp = pq%heap(j)
        pq%heap(j) = pq%heap(i)
        pq%heap(i) = tmp
      end if
      i = j
    end do
    return
  end function poll

  function peek(pq) result(item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    integer :: item

    item = pq%heap(1)
    return
  end function peek

end module mod_priority_queue
