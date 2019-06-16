module mod_t_item
  implicit none
  type :: t_item
    integer :: pos, cost
  end type t_item
  private
  public :: t_item, itemize, compare, swap
contains
  function itemize(pos,cost) result(i)
    implicit none
    integer :: pos, cost
    type(t_item) :: i
    i%pos = pos
    i%cost = cost
    return
  end function itemize
  function compare(i1,i2) result(c)
    implicit none
    type(t_item), intent(in) :: i1, i2
    integer :: c
    c = i1%cost-i2%cost
    return
  end function compare
  subroutine swap(i1,i2)
    implicit none
    type(t_item), intent(inout) :: i1, i2
    type(t_item) :: i
    i = i1
    i1 = i2
    i2 = i
    return
  end subroutine swap
end module mod_t_item
module mod_t_list
  use mod_t_item
  implicit none
  type :: t_list
    integer :: vol
    type(t_item), pointer :: arr(:)
  end type t_list
  private
  public :: t_list, init_list, release_list,push_back
  public :: pop_back, get_at, size_of_list
contains
  subroutine init_list(list)
    implicit none
    type(t_list), intent(inout) :: list
    list%vol = 0
    nullify(list%arr)
    allocate(list%arr(1))
    return
  end subroutine init_list
  subroutine release_list(list)
    implicit none
    type(t_list), intent(inout) :: list
    if (associated(list%arr)) deallocate(list%arr)
    return
  end subroutine release_list
  subroutine push_back(list,item)
    implicit none
    type(t_list), intent(inout) :: list
    type(t_item), intent(in) :: item
    type(t_item), allocatable :: tmp(:)
    if (size(list%arr).eq.list%vol) then
      allocate(tmp(list%vol))
      tmp = list%arr
      deallocate(list%arr)
      allocate(list%arr(2*list%vol))
      list%arr(1:list%vol) = tmp
      deallocate(tmp)
    end if
    list%vol = list%vol+1
    list%arr(list%vol) = item
    return
  end subroutine push_back
  function pop_back(list) result(item)
    implicit none
    type(t_list), intent(inout) :: list
    type(t_item) :: item
    item = list%arr(list%vol)
    list%vol = list%vol-1
    return
  end function pop_back
  function get_at(list,i) result(item)
    implicit none
    type(t_list), intent(in) :: list
    integer, intent(in) :: i
    type(t_item) :: item
    item = list%arr(i)
    return
  end function get_at
  function size_of_list(list) result(s)
    implicit none
    type(t_list), intent(in) :: list
    integer :: s
    s = list%vol
    return
  end function size_of_list
end module mod_t_list
module mod_t_priority_queue
  use mod_t_item
  use mod_t_list
  implicit none
  type :: t_heap
    type(t_list) :: list
  end type t_heap
  private
  public :: t_heap, init_heap, release_heap, push, pop, size_of_heap
contains
  subroutine init_heap(heap)
    implicit none
    type(t_heap), intent(inout) :: heap
    call init_list(heap%list)
    return
  end subroutine init_heap
  subroutine release_heap(heap)
    implicit none
    type(t_heap), intent(inout) :: heap
    call release_list(heap%list)
    return
  end subroutine release_heap
  subroutine push(heap,item)
    implicit none
    type(t_heap), intent(inout) :: heap
    type(t_item), intent(in) :: item
    integer :: n, i
    call push_back(heap%list,item)
    n = heap%list%vol
    do while (n.gt.1)
      i = n/2
      if (compare(heap%list%arr(n),heap%list%arr(i)).lt.0) then
        call swap(heap%list%arr(n),heap%list%arr(i))
      end if
      n = i
    end do
    return
  end subroutine push
  function pop(heap) result(item)
    implicit none
    type(t_heap), intent(inout) :: heap
    type(t_item) :: item, tmp
    integer :: n, i, j
    n = heap%list%vol
    item = heap%list%arr(1)
    heap%list%arr(1) = heap%list%arr(n)
    tmp = pop_back(heap%list)
    i = 1
    do while (2*i.lt.n)
      j = 2*i
      if ((j.lt.n-1).and.(compare(heap%list%arr(j+1),heap%list%arr(j)).lt.0)) then
        j = j+1
      end if
      if (compare(heap%list%arr(j),heap%list%arr(i)).lt.0) then
        call swap(heap%list%arr(j),heap%list%arr(i))
      end if
      i = j
    end do
    return
  end function pop
  function size_of_heap(heap) result(s)
    implicit none
    type(t_heap), intent(in) :: heap
    integer :: s
    s = size_of_list(heap%list)
    return
  end function size_of_heap
end module mod_t_priority_queue
module mod_t_graph
  use mod_t_item
  use mod_t_list
  use mod_t_priority_queue
  implicit none
  integer, parameter :: infty = 1000000000
  type :: t_graph
    type(t_list), pointer :: edges(:)
  end type t_graph
  private
  public :: infty
  public :: t_graph, init_graph, release_graph, add_edge, add, dijkstra
contains
  subroutine init_graph(graph,n)
    implicit none
    type(t_graph), intent(inout) :: graph
    integer, intent(in) :: n
    integer :: i
    if (associated(graph%edges)) deallocate(graph%edges)
    allocate(graph%edges(n))
    do i = 1, n
      call init_list(graph%edges(i))
    end do
    return
  end subroutine init_graph
  subroutine release_graph(graph)
    implicit none
    type(t_graph), intent(inout) :: graph
    integer :: n, i
    if (.not.associated(graph%edges)) return
    n = size(graph%edges)
    do i = 1, n
      call release_list(graph%edges(i))
    end do
    deallocate(graph%edges)
    return
  end subroutine release_graph
  subroutine add_edge(graph,i,edge)
    implicit none
    type(t_graph), intent(inout) :: graph
    type(t_item), intent(in) :: edge
    integer, intent(in) :: i
    call push_back(graph%edges(i),edge)
    return
  end subroutine add_edge
  subroutine add(graph,i,pos,cost)
    implicit none
    type(t_graph), intent(inout) :: graph
    integer, intent(in) :: i,pos,cost
    call push_back(graph%edges(i),itemize(pos,cost))
    return
  end subroutine add
  function dijkstra(graph,start) result(c)
    implicit none
    type(t_graph), intent(in) :: graph
    integer, intent(in) :: start
    type(t_heap) :: pq
    type(t_item) :: now, next
    integer :: n, c(size(graph%edges)), i
    n = size(graph%edges)
    c = infty
    call init_heap(pq)
    c(start) = 0
    call push(pq,itemize(start,0))
    do while (size_of_heap(pq).gt.0)
      now = pop(pq)
      if (now%cost.gt.c(now%pos)) cycle
      do i = 1, size_of_list(graph%edges(now%pos))
        next = get_at(graph%edges(now%pos),i)
        if (c(next%pos).gt.c(now%pos)+next%cost) then
          c(next%pos) = c(now%pos)+next%cost
          call push(pq,itemize(next%pos,c(next%pos)))
        end if
      end do
    end do
    call release_heap(pq)
    return
  end function dijkstra
end module mod_t_graph
