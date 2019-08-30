module mod_graph
  implicit none
  integer(8), parameter :: inf = 1000000000000000000_8

  type item
    integer :: id = 0
    integer(8) :: wt = 0_8
  end type

  type priorityqueue
    integer :: num = 0
    type(item), pointer :: heap(:) => null()
  end type

  type edge
    integer :: fm = 0, to = 0, cp = 0, rv = 0
    integer(8) :: wt = 0_8
  end type

  type arraylist
    integer :: num = 0
    type(edge), pointer :: arr(:) => null()
  contains
    procedure :: add => adde
    procedure :: bellman_ford => bellman_ford
  end type

  type graph
    type(arraylist), pointer :: egs(:) => null()
    logical, pointer :: used(:) => null()
  contains
    procedure :: add => add
    procedure :: dijkstra => dijkstra
    procedure :: ford_fulkerson => ford_fulkerson
  end type

  interface graph
    module procedure :: newg
  end interface graph

contains

  function newi(id,wt) result(ret)
    integer, intent(in) :: id
    integer(8), intent(in) :: wt
    type(item) :: ret
    ret%id = id
    ret%wt = wt
  end

  subroutine swapi(a,b)
    type(item), intent(inout) :: a, b
    type(item) :: c
    c = a
    a = b
    b = c
  end

  subroutine appendi(a)
    type(item), pointer, intent(inout) :: a(:)
    integer :: n
    type(item), allocatable :: tmp(:)
    n = size(a)
    allocate(tmp(n))
    tmp = a
    deallocate(a)
    allocate(a(2*n))
    a(1:n) = tmp
    deallocate(tmp)
  end

  function lessi(a,b) result(ret)
    type(item), intent(in) :: a, b
    logical :: ret
    ret = a%wt < b%wt
  end

  subroutine finalizei(pq)
    type(priorityqueue), intent(inout) :: pq
    if (associated(pq%heap)) deallocate(pq%heap)
    pq%num = 0
  end

  subroutine offer(pq,it)
    type(priorityqueue), intent(inout) :: pq
    type(item), intent(in) :: it
    integer :: i, j
    if (.not.associated(pq%heap)) allocate(pq%heap(1))
    if (pq%num == size(pq%heap)) call appendi(pq%heap)
    pq%num = pq%num+1
    pq%heap(pq%num) = it
    i = pq%num
    do while (i > 1)
      j = i/2
      if (lessi(pq%heap(i),pq%heap(j))) call swapi(pq%heap(i),pq%heap(j))
      i = j
    end do
  end

  function poll(pq) result(ret)
    type(priorityqueue), intent(inout) :: pq
    type(item) :: ret
    integer :: n, i, j
    n = pq%num
    ret = pq%heap(1)
    pq%heap(1) = pq%heap(n)
    pq%num = pq%num-1
    i = 1
    do while (2*i < n)
      j = 2*i
      if (j+1 < n .and. lessi(pq%heap(j+1),pq%heap(j))) j = j+1
      if (lessi(pq%heap(j),pq%heap(i))) call swapi(pq%heap(j),pq%heap(i))
      i = j
    end do
  end

  function newe(to,wt,fm,cp,rv) result(ret)
    integer, intent(in) :: to
    integer(8), intent(in) :: wt
    integer, intent(in), optional :: fm, cp, rv
    type(edge) :: ret
    if (present(fm)) ret%fm = fm
    ret%to = to
    ret%wt = wt
    if (present(cp) .and. present(rv)) then
      ret%cp = cp
      ret%rv = rv
    end if
  end

  subroutine appende(a)
    type(edge), pointer, intent(inout) :: a(:)
    integer :: n
    type(edge), allocatable :: tmp(:)
    n = size(a)
    allocate(tmp(n))
    tmp = a
    deallocate(a)
    allocate(a(2*n))
    a(1:n) = tmp
    deallocate(tmp)
  end

  function lesse(a,b) result(ret)
    type(edge), intent(in) :: a, b
    logical :: ret
    ret = a%wt < b%wt
  end

  subroutine finalizee(list)
    type(arraylist), intent(inout) :: list
    if (associated(list%arr)) deallocate(list%arr)
    list%num = 0
  end

  subroutine adde(list,e)
    class(arraylist), intent(inout) :: list
    type(edge), intent(in) :: e
    if (.not.associated(list%arr)) allocate(list%arr(1))
    if (list%num == size(list%arr)) call appende(list%arr)
    list%num = list%num+1
    list%arr(list%num) = e
  end

  function pope(list) result(ret)
    type(arraylist), intent(inout) :: list
    type(edge) :: ret
    ret = list%arr(list%num)
    list%num = list%num-1
  end

  function newg(n) result(ret)
    integer, intent(in) :: n
    type(graph) :: ret
    allocate(ret%egs(n))
  end

  subroutine add(g,fm,to,wt,cp)
    class(graph), intent(inout) :: g
    integer, intent(in) :: fm, to
    integer, intent(in), optional :: cp
    integer(8) :: wt
    if (present(cp)) then
      call adde(g%egs(fm),newe(to,wt,cp=cp,rv=g%egs(to)%num))
      call adde(g%egs(to),newe(fm,wt,cp=0,rv=g%egs(fm)%num-1))
    else
      call adde(g%egs(fm),newe(to,wt))
    end if
  end

  function dijkstra(g,s) result(ret)
    class(graph), intent(in) :: g
    integer, intent(in) :: s
    integer(8) :: ret(size(g%egs))
    type(priorityqueue) :: pq
    type(item) :: it
    type(edge) :: e
    integer :: i
    ret = inf
    ret(s) = 0_8
    call offer(pq,newi(s,ret(s)))
    do while (pq%num > 0)
      it = poll(pq)
      if (it%wt > ret(it%id)) cycle
      do i = 1, g%egs(it%id)%num
        e = g%egs(it%id)%arr(i)
        if (ret(e%to) > ret(it%id)+e%wt) then
          ret(e%to) = ret(it%id)+e%wt
          call offer(pq,newi(e%to,ret(e%to)))
        end if
      end do
    end do
  end

  recursive subroutine dfs_bf(g,u)
    type(graph), intent(inout) :: g
    integer, intent(in) :: u
    integer :: i, v
    g%used(u) = .true.
    do i = 1, g%egs(u)%num
      v = g%egs(u)%arr(i)%to
      if (g%used(v)) cycle
      call dfs_bf(g,v)
    end do
  end

  function bellman_ford(es,s,t) result(ret)
    class(arraylist), intent(in) :: es
    integer, intent(in) :: s, t
    integer(8) :: ret
    integer :: n, i, fm, to, step
    integer(8) :: wt
    logical :: updated
    integer(8), allocatable :: tmp(:)
    type(graph) :: gs, gt
    type(arraylist) :: reach
    n = 0
    do i = 1, es%num
      n = max(n,es%arr(i)%fm,es%arr(i)%to)
    end do
    gs = newg(n)
    gt = newg(n)
    allocate(gs%used(n),gt%used(n))
    gs%used = .false.
    gt%used = .false.
    do i = 1, es%num
      fm = es%arr(i)%fm
      to = es%arr(i)%to
      wt = es%arr(i)%wt
      call add(gs,fm,to,wt)
      call add(gt,to,fm,wt)
    end do
    call dfs_bf(gs,s)
    call dfs_bf(gt,t)
    n = 0
    do i = 1, es%num
      fm = es%arr(i)%fm
      to = es%arr(i)%to
      if (gs%used(fm) .and. gt%used(fm) .and. gs%used(to) .and. gt%used(to)) then
        call adde(reach,es%arr(i))
        n = max(n,fm,to)
      end if
    end do
    deallocate(gs%used,gt%used)
    allocate(tmp(n))
    tmp = inf
    tmp(s) = 0_8
    step = 0
    updated = .true.
    do while (updated)
      updated = .false.
      do i = 1, reach%num
        fm = reach%arr(i)%fm
        to = reach%arr(i)%to
        wt = reach%arr(i)%wt
        if (tmp(to) > tmp(fm)+wt) then
          tmp(to) = tmp(fm)+wt
          updated = .true.
        end if
      end do
      step = step+1
      if (step > n) then
        ret = -inf
        return
      end if
    end do
    ret = tmp(t)
    deallocate(tmp)
  end

  recursive function dfs_ff(g,u,t,f) result(ret)
    class(graph), intent(inout) :: g
    integer, intent(in) :: u, t, f
    integer :: ret
    integer :: i, v, cp, rv
    ret = f
    if (u == t) return
    g%used(u) = .true.
    do i = 1, g%egs(u)%num
      v = g%egs(u)%arr(i)%to
      cp = g%egs(u)%arr(i)%cp
      if (g%used(v) .or. cp <= 0) cycle
      ret = dfs_ff(g,v,t,min(f,cp))
      rv = g%egs(u)%arr(i)%rv
      if (ret > 0) then
        g%egs(u)%arr(i)%cp = g%egs(u)%arr(i)%cp-ret
        g%egs(v)%arr(rv)%cp = g%egs(v)%arr(rv)%cp+ret
        return
      end if
    end do
    ret = 0
  end

  function ford_fulkerson(g,s,t) result(ret)
    class(graph), intent(inout) :: g
    integer, intent(in) :: s, t
    integer :: ret
    integer :: f
    ret = 0
    if (.not.associated(g%used)) allocate(g%used(size(g%egs)))
    do
      g%used = .false.
      f = dfs_ff(g,s,t,1000000000)
      if (f == 0) then
        deallocate(g%used)
        return
      end if
      ret = ret+f
    end do
  end

end module mod_graph
