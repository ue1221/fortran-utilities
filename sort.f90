module mod_sort
  implicit none
  private
  public :: swap, quick_sort, merge_sort, heap_sort, comb_sort
  public :: shell_sort, insertion_sort, gnome_sort, shaker_sort
  public :: bitonic_sort !ToDo, Array length must be a power of 2.
contains
  subroutine swap(a,b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: c
    c = a
    a = b
    b = c
    return
  end subroutine swap
  recursive subroutine quick_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, l, r, p
    n = size(a)
    l = 1
    r = n
    p = a((l+r)/2)+a(l)+a(r)-max(a((l+r)/2),a(l),a(r))-min(a((l+r)/2),a(l),a(r))
    do
      do while (a(l).lt.p)
        l = l+1
      end do
      do while (a(r).gt.p)
        r = r-1
      end do
      if (l.ge.r) exit
      call swap(a(l),a(r))
      l = l+1
      r = r-1
    end do
    if (l.gt.2) call quick_sort(a(1:l-1))
    if (n.gt.r+1) call quick_sort(a(r+1:n))
    return
  end subroutine quick_sort
  subroutine merge_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, m, l, i, u
    n = size(a)
    m = n
    l = 1
    do while (m.gt.1)
      do i = 1, m/2
        u = min(2*i*l,n)
        call merger(a(2*(i-1)*l+1:(2*i-1)*l),a((2*i-1)*l+1:u))
      end do
      l = 2*l
      m = (m+1)/2
    end do
    return
  end subroutine merge_sort
  subroutine merger(a1,a2)
    implicit none
    integer, intent(inout) :: a1(:), a2(:)
    integer :: a(size(a1)+size(a2))
    integer :: i1, i2, n1, n2
    i1 = 1
    i2 = 1
    n1 = size(a1)
    n2 = size(a2)
    do while (i1.le.n1.and.i2.le.n2)
      if (a1(i1).le.a2(i2)) then
        a(i1+i2-1) = a1(i1)
        i1 = i1+1
      else
        a(i1+i2-1) = a2(i2)
        i2 = i2+1
       end if
    end do
    if (i1.le.n1) then
      a(i1+i2-1:n1+n2) = a1(i1:n1)
    else if (i2.le.n2) then
      a(i1+i2-1:n1+n2) = a2(i2:n2)
    end if
    a1 = a(1:n1)
    a2 = a(n1+1:n1+n2)
    return
  end subroutine merger
  subroutine heap_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, i, j, k
    n = size(a)
    do i = 1, n
      j = i
      do while (j.gt.1)
        k = j/2
        if (a(j).gt.a(k)) call swap(a(j),a(k))
        j = k
      end do
    end do
    do i = n, 1, -1
      call swap(a(i),a(1))
      j = 1
      do while (2*j.lt.i)
        k = 2*j
        if ((k.lt.i-1).and.(a(k+1).gt.a(k))) k = k+1
        if (a(j).lt.a(k)) call swap(a(j),a(k))
        j = k
      end do
    end do
    return
  end subroutine heap_sort
  subroutine comb_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    logical :: ok
    integer :: n, i, h
    n = size(a)
    h = n
    ok = .false.
    do while ((h.gt.1).or.(.not.ok))
      h = (10*h)/13
      if ((h.eq.9).or.(h.eq.10)) h = 11
      ok = .true.
      do i = 1, n-h
        if (a(i).gt.a(i+h)) then
          call swap(a(i),a(i+h))
          ok = .false.
        end if
      end do
    end do
    return
  end subroutine comb_sort
  subroutine shell_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, i, h
    n = size(a)
    h = 1
    do while (3*h+1.lt.n)
      h = 3*h+1
    end do
    do while (h.ge.1)
      do i = 1, h
        call insertion_sort(a(i:n:h))
      end do
      h = (h-1)/3
    end do
    return
  end subroutine shell_sort
  subroutine insertion_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, i, j, t
    n = size(a)
    do i = 2, n
      if (a(i).ge.a(i-1)) cycle
      t = a(i)
      j = i
      do while ((j.gt.1).and.(t.lt.a(j-1)))
        a(j) = a(j-1)
        j = j-1
      end do
      a(j) = t
    end do
    return
  end subroutine insertion_sort
  subroutine gnome_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, i
    n = size(a)
    i = 2
    do while (i.le.n)
      if (a(i).ge.a(i-1)) then
        i = i+1
        cycle
      end if
      call swap(a(i),a(i-1))
      i = i-1
      if (i.eq.1) i = 2
    end do
    return
  end subroutine gnome_sort
  subroutine shaker_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, l, r, m, i
    n = size(a)
    l = 1
    r = n
    do
      m = l
      do i = l, r-1
        if (a(i).gt.a(i+1)) then
          call swap(a(i),a(i+1))
          m = i
        end if
      end do
      r = m
      if (l.eq.r) exit
      m = r
      do i = r, l+1, -1
        if (a(i).lt.a(i-1)) then
          call swap(a(i),a(i-1))
          m = i
        end if
      end do
      l = m
      if (l.eq.r) exit
    end do
    return
  end subroutine shaker_sort
  subroutine bitonic_sort(a)
    implicit none
    integer, intent(inout) :: a(:)
    integer :: n, i
    n = size(a)
    if (n.eq.1) return
    call bitonic_sorter(a(1:n/2),.true.)
    call bitonic_sorter(a(n/2+1:n),.false.)
    call bitonic_merger(a,.true.)
    return
  end subroutine bitonic_sort
  recursive subroutine bitonic_sorter(a,ascend)
    implicit none
    integer, intent(inout) :: a(:)
    logical, intent(in) :: ascend
    integer :: n, i
    n = size(a)
    if (n.eq.1) return
    call bitonic_sorter(a(1:n/2),.true.)
    call bitonic_sorter(a(n/2+1:n),.false.)
    call bitonic_merger(a,ascend)
    return
  end subroutine bitonic_sorter
  recursive subroutine bitonic_merger(a,ascend)
    implicit none
    integer, intent(inout) :: a(:)
    logical, intent(in) :: ascend
    integer :: n, i
    n = size(a)
    if (n.eq.1) return
    call bitonic_comparator(a,ascend)
    call bitonic_merger(a(1:n/2),ascend)
    call bitonic_merger(a(n/2+1:n),ascend)
    return
  end subroutine bitonic_merger
  subroutine bitonic_comparator(a,ascend)
    implicit none
    integer, intent(inout) :: a(:)
    logical, intent(in) :: ascend
    integer :: n, m, i
    n = size(a)
    m = n/2
    do i = 1, m
      if ((a(i).gt.a(i+m)).eqv.ascend) call swap(a(i),a(i+m))
    end do
    return
  end subroutine bitonic_comparator
end module mod_sort
