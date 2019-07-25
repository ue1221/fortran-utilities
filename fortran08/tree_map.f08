module mod_tree_map

  type t_entry
    integer :: key
    integer :: val
  end type t_entry

  type t_node
    type(t_node), pointer :: left => null(), right => null()
    integer :: level = 1
    type(t_entry), pointer :: e => null()
  end type t_node

  type t_tree_map
    type(t_node), pointer :: root => null()
    integer :: deflt = -1
  end type t_tree_map

contains

  function new_entry(key,val) result(e)
    implicit none
    integer, intent(in) :: key
    integer, intent(in) :: val
    type(t_entry), pointer :: e

    e => null()
    allocate(e)
    e%key = key
    e%val = val
    return
  end function new_entry

  function new_node(e) result(n)
    implicit none
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n

    n => null()
    allocate(n)
    n%e => e
    return
  end function new_node

  logical function is_leaf(n)
    implicit none
    type(t_node), pointer, intent(in) :: n

    is_leaf = associated(n) .and. .not.associated(n%left) .and. &
    &         .not.associated(n%right)
    return
  end function is_leaf

  recursive function tree_size(n) result(s)
    implicit none
    type(t_node), pointer, intent(in) :: n
    integer :: s

    s = 0
    if (.not.associated(n)) return
    s = 1+tree_size(n%left)+tree_size(n%right)
    return
  end function tree_size

  function skew(n) result(l)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: l

    l => n
    if (.not.(associated(n) .and. associated(n%left) .and. &
    &   n%left%level == n%level)) return
    l => n%left
    n%left => l%right
    l%right => n
    return
  end function skew

  function split(n) result(r)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: r

    r => n
    if (.not.(associated(n) .and. associated(n%right) .and. &
    &   associated(n%right%right) .and. n%right%right%level == n%level)) return
    r => n%right
    n%right => r%left
    r%left => n
    r%level = r%level+1
    return
  end function split

  function predecessor(n) result(p)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: p

    p => null()
    if (.not.associated(n%left)) return
    p => n%left
    do while (associated(p%right))
      p => p%right
    end do
    return
  end function predecessor

  function successor(n) result(s)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: s

    s => null()
    if (.not.associated(n%right)) return
    s => n%right
    do while (associated(s%left))
      s => s%left
    end do
    return
  end function successor

  recursive function insert(n,e) result(t)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: t

    t => new_node(e)
    if (.not.associated(n)) return
    t => n
    if (e%key < t%e%key) then
      t%left => insert(t%left,e)
    else if (e%key > t%e%key) then
      t%right => insert(t%right,e)
    else
      t%e => e
    end if

    t => skew(t)
    t => split(t)
    return
  end function insert

  recursive function delete(n,e) result(t)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: t, l

    t => n
    if (.not.associated(n)) return
    if (e%key < t%e%key) then
      t%left => delete(t%left,e)
    else if (e%key > t%e%key) then
      t%right => delete(t%right,e)
    else
      if (is_leaf(t)) return
      if (.not.associated(t%left)) then
        l => successor(t)
        t%right => delete(t%right,l%e)
        t%e => l%e
      else
        l => predecessor(t)
        t%left => delete(t%left,l%e)
        t%e => l%e
      end if
    end if

    t => decrease_level(t)
    t => skew(t)
    t%right => skew(t%right)
    if (associated(t%right)) t%right%right => skew(t%right%right)
    t => split(t)
    t%right => split(t%right)
    return
  end function delete

  function decrease_level(n) result(t)
    implicit none
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: t
    integer :: should_be

    t => n
    should_be = min(t%left%level,t%right%level)+1
    if (t%level > should_be) then
      t%level = should_be
      if (t%right%level > should_be) t%right%level = should_be
    end if
    return
  end function decrease_level

  recursive subroutine release_tree(t)
    implicit none
    type(t_node), pointer, intent(inout) :: t

    if (.not.associated(t)) return
    call release_tree(t%left)
    call release_tree(t%right)
    deallocate(t)
    return
  end subroutine release_tree

  recursive subroutine get_keys_list(t,keys,num)
    implicit none
    type(t_node), pointer, intent(in) :: t
    integer, intent(inout) :: keys(:)
    integer, intent(inout) :: num

    if (.not.associated(t)) return
    call get_keys_list(t%left,keys,num)
    num = num+1
    keys(num) = t%e%key
    call get_keys_list(t%right,keys,num)
    return
  end subroutine get_keys_list

  integer function size_of(map)
    implicit none
    type(t_tree_map), intent(in) :: map

    size_of = tree_size(map%root)
    return
  end function size_of

  subroutine clear(map)
    implicit none
    type(t_tree_map), intent(inout) :: map

    call release_tree(map%root)
    map%root => null()
    return
  end subroutine clear

  subroutine set_default(map,deflt)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: deflt

    map%deflt = deflt
    return
  end subroutine set_default

  subroutine put_entry(map,e)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e

    map%root => insert(map%root,e)
    return
  end subroutine put_entry

  subroutine remove_entry(map,e)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e

    map%root => delete(map%root,e)
    return
  end subroutine remove_entry

  function get_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    do while (associated(n))
      if (e%key < n%e%key) then
        n => n%left
      else if (e%key > n%e%key) then
        n => n%right
      else
        ret => n%e
        return
      end if
    end do
    return
  end function get_entry

  function contain_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n
    logical :: ret

    ret = .false.
    n => map%root
    do while (associated(n))
      if (e%key < n%e%key) then
        n => n%left
      else if (e%key > n%e%key) then
        n => n%right
      else
        ret = .true.
        return
      end if
    end do
    return
  end function contain_entry

  function get_first_entry(map) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    if (.not.associated(n)) return
    do while (associated(n%left))
      n => n%left
    end do
    ret => n%e
    return
  end function get_first_entry

  function poll_first_entry(map) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    if (.not.associated(n)) return
    do while (associated(n%left))
      n => n%left
    end do
    ret => n%e

    map%root => delete(map%root,ret)
    return
  end function poll_first_entry

  function get_last_entry(map) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    if (.not.associated(n)) return
    do while (associated(n%right))
      n => n%right
    end do
    ret => n%e
    return
  end function get_last_entry

  function poll_last_entry(map) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    if (.not.associated(n)) return
    do while (associated(n%right))
      n => n%right
    end do
    ret => n%e

    map%root => delete(map%root,ret)
    return
  end function poll_last_entry

  function floor_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    do while (associated(n))
      if (e%key < n%e%key) then
        n => n%left
      else if (e%key > n%e%key) then
        if (.not.associated(ret)) then
          ret => n%e
          cycle
        end if
        if (e%key-ret%key > e%key-n%e%key) ret => n%e
        n => n%right
      else
        ret => n%e
        return
      end if
    end do
    return
  end function floor_entry

  function lower_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_entry), pointer :: ret

    ret => floor_entry(map,new_entry(e%key-1,0))
    return
  end function lower_entry

  function ceiling_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n
    type(t_entry), pointer :: ret

    ret => null()
    n => map%root
    do while (associated(n))
      if (e%key < n%e%key) then
        if (.not.associated(ret)) then
          ret => n%e
          cycle
        end if
        if (e%key-ret%key < e%key-n%e%key) ret => n%e
        n => n%left
      else if (e%key > n%e%key) then
        n => n%right
      else
        ret => n%e
        return
      end if
    end do
    return
  end function ceiling_entry

  function higher_entry(map,e) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_entry), pointer :: ret

    ret => ceiling_entry(map,new_entry(e%key+1,0))
    return
  end function higher_entry

  subroutine get_keys(map,keys,num)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(inout) :: keys(:)
    integer, intent(inout) :: num

    keys = 0
    num = 0
    call get_keys_list(map%root,keys,num)
    return
  end subroutine get_keys

  subroutine put(map,key,val)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    integer, intent(in) :: val

    call put_entry(map,new_entry(key,val))
    return
  end subroutine put

  subroutine remove(map,key)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key

    call remove_entry(map,new_entry(key,0))
    return
  end subroutine remove

  function get(map,key) result(val)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: val

    val = map%deflt
    tmp => get_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    val = tmp%val
    return
  end function get

  logical function contain(map,key)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key

    contain = contain_entry(map,new_entry(key,0))
    return
  end function contain

  function get_first_key(map) result(key)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer :: tmp
    integer :: key

    key = map%deflt
    tmp => get_first_entry(map)
    if (.not.associated(tmp)) return
    key = tmp%key
    return
  end function get_first_key

  function get_last_key(map) result(key)
    implicit none
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer :: tmp
    integer :: key

    key = map%deflt
    tmp => get_last_entry(map)
    if (.not.associated(tmp)) return
    key = tmp%key
    return
  end function get_last_key

  function floor_key(map,key) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret

    ret = map%deflt
    tmp => floor_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
    return
  end function floor_key

  function lower_key(map,key) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret

    ret = map%deflt
    tmp => lower_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
    return
  end function lower_key

  function ceiling_key(map,key) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret

    ret = map%deflt
    tmp => ceiling_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
    return
  end function ceiling_key

  function higher_key(map,key) result(ret)
    implicit none
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret

    ret = map%deflt
    tmp => higher_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
    return
  end function higher_key

end module mod_tree_map
