module mod_AA_tree_map
  implicit none
  type t_key
    integer :: k
  end type t_key
  type t_val
    integer :: v
  end type t_val
  type t_node
    type(t_node), pointer :: left, right
    type(t_key) :: key
    type(t_val) :: val
    integer :: level
  end type t_node
  type(t_node), pointer :: nil
  type t_map
    type(t_node), pointer :: root
  end type t_map
  private
  public :: t_map, init_map, release_map, put, belong, get, remove
  public :: size_of, first_key, last_key, poll_first_entry, poll_last_entry
contains
  function new_key(k) result(key)
    implicit none
    integer, intent(in) :: k
    type(t_key) :: key
    key%k = k
    return
  end function new_key
  function new_val(v) result(val)
    implicit none
    integer, intent(in) :: v
    type(t_val) :: val
    val%v = v
    return
  end function new_val
  function compare(k1,k2) result(c)
    implicit none
    type(t_key) :: k1, k2
    integer :: c
    c = k1%k-k2%k
    return
  end function compare
  function new_node(key,val) result(node)
    implicit none
    type(t_key) :: key
    type(t_val) :: val
    type(t_node), pointer :: node
    allocate(node)
    node%left => nil
    node%right => nil
    node%key = key
    node%val = val
    node%level = 1
    return
  end function new_node
  recursive subroutine release_node(node)
    type(t_node), pointer :: node
    if (associated(node,nil)) return
    call release_node(node%left)
    call release_node(node%right)
    deallocate(node)
    return
  end subroutine release_node
  logical function is_leaf(x)
    implicit none
    type(t_node), pointer :: x
    is_leaf = (.not.associated(x,nil)).and. &
    &          associated(x%left,nil).and.associated(x%right,nil)
    return
  end function is_leaf
  recursive function size_of_tree(root) result(s)
    implicit none
    type(t_node), pointer :: root
    integer :: s
    s = 0
    if (associated(root,nil)) return
    s = size_of_tree(root%left)+size_of_tree(root%right)+1
    return
  end function size_of_tree
  subroutine skew(root)
    implicit none
    type(t_node), pointer :: root, temp
    if (associated(root,nil).or.associated(root%left,nil).or. &
    &   root%left%level.ne.root%level) return
    temp => root
    root => root%left
    temp%left => root%right
    root%right => temp
    return
  end subroutine skew
  subroutine split(root)
    implicit none
    type(t_node), pointer :: root, temp
    if (associated(root,nil).or.associated(root%right,nil).or. &
    &   associated(root%right%right,nil).or. &
    &   root%right%right%level.ne.root%level) return
    temp => root
    root => root%right
    temp%right => root%left
    root%left => temp
    root%level = root%level+1
    return
  end subroutine split
  recursive subroutine insert(root,key,val)
    implicit none
    type(t_node), pointer :: root
    type(t_key) :: key
    type(t_val) :: val
    if (associated(root,nil)) then
      root => new_node(key,val)
      return
    else if (compare(key,root%key).lt.0) then
      call insert(root%left,key,val)
    else if (compare(key,root%key).gt.0) then
      call insert(root%right,key,val)
    else
      root%val = val
    end if
    call skew(root)
    call split(root)
    return
  end subroutine insert
  recursive subroutine delete(root,key)
    type(t_node), pointer :: root, leaf
    type(t_key) :: key
    if (associated(root,nil)) then
      return
    else if (compare(key,root%key).lt.0) then
      call delete(root%left,key)
    else if (compare(key,root%key).gt.0) then
      call delete(root%right,key)
    else
      if (is_leaf(root)) then
        root => nil
      else if (associated(root%left,nil)) then
        leaf => root%right
        do while (.not.associated(leaf%left,nil))
          leaf => leaf%left
        end do
        call delete(root%right,leaf%key)
        root%key = leaf%key
        root%val = leaf%val
      else
        leaf => root%left
        do while (.not.associated(leaf%right,nil))
          leaf => leaf%right
        end do
        call delete(root%left,leaf%key)
        root%key = leaf%key
        root%val = leaf%val
      end if
    end if
    call decrease_level(root)
    call skew(root)
    call skew(root%right)
    if (.not.associated(root%right,nil)) call skew(root%right%right)
    call split(root)
    call split(root%right)
    return
  end subroutine delete
  subroutine decrease_level(root)
    implicit none
    type(t_node), pointer :: root
    integer :: should_be
    should_be = min(root%left%level,root%right%level)+1
    if (should_be.lt.root%level) then
      root%level = should_be
      if (should_be.lt.root%right%level) root%right%level = should_be
    end if
    return
  end subroutine decrease_level
  subroutine init_map(map)
    implicit none
    type(t_map) :: map
    allocate(nil)
    nil%left => nil
    nil%right => nil
    nil%level = 0
    map%root => nil
    map%root%left => nil
    map%root%right => nil
    return
  end subroutine init_map
  subroutine release_map(map)
    implicit none
    type(t_map) :: map
    call release_node(map%root)
    deallocate(nil)
    return
  end subroutine release_map
  subroutine put(map,k,v)
    implicit none
    type(t_map) :: map
    integer, intent(in) :: k, v
    call insert(map%root,new_key(k),new_val(v))
    return
  end subroutine put
  function belong(map,k) result(b)
    implicit none
    type(t_map) :: map
    integer, intent(in) :: k
    logical :: b
    type(t_key) :: key
    type(t_node), pointer :: node
    key = new_key(k)
    b = .false.
    node => map%root
    do while (.not.associated(node,nil))
      if (compare(key,node%key).lt.0) then
        node => node%left
      else if (compare(key,node%key).gt.0) then
        node => node%right
      else
        b = .true.
        return
      end if
    end do
    return
  end function belong
  function get(map,k) result(v)
    implicit none
    type(t_map) :: map
    integer, intent(in) :: k
    integer :: v
    type(t_key) :: key
    type(t_node), pointer :: node
    key = new_key(k)
    v = -1
    node => map%root
    do while (.not.associated(node,nil))
      if (compare(key,node%key).lt.0) then
        node => node%left
      else if (compare(key,node%key).gt.0) then
        node => node%right
      else
        v = node%val%v
        return
      end if
    end do
    return
  end function get
  subroutine remove(map,k)
    implicit none
    type(t_map) :: map
    integer, intent(in) :: k
    call delete(map%root,new_key(k))
    return
  end subroutine remove
  function size_of(map) result(s)
    implicit none
    type(t_map) :: map
    integer :: s
    s = size_of_tree(map%root)
    return
  end function size_of
  function first_key(map) result(k)
    implicit none
    type(t_map) :: map
    integer :: k
    type(t_node), pointer :: node
    k = -1
    node => map%root
    if (associated(node,nil)) return
    do while (.not.associated(node%left,nil))
      node => node%left
    end do
    k = node%key%k
    return
  end function first_key
  function last_key(map) result(k)
    implicit none
    type(t_map) :: map
    integer :: k
    type(t_node), pointer :: node
    k = -1
    node => map%root
    if (associated(node,nil)) return
    do while (.not.associated(node%right,nil))
      node => node%right
    end do
    k = node%key%k
    return
  end function last_key
  function poll_first_entry(map) result(k)
    implicit none
    type(t_map) :: map
    integer :: k
    type(t_node), pointer :: node
    k = -1
    node => map%root
    if (associated(node,nil)) return
    do while (.not.associated(node%left,nil))
      node => node%left
    end do
    k = node%key%k
    call remove(map,k)
    return
  end function poll_first_entry
  function poll_last_entry(map) result(k)
    implicit none
    type(t_map) :: map
    integer :: k
    type(t_node), pointer :: node
    k = -1
    node => map%root
    if (associated(node,nil)) return
    do while (.not.associated(node%right,nil))
      node => node%right
    end do
    k = node%key%k
    call remove(map,k)
    return
  end function poll_last_entry
end module mod_AA_tree_map
