module mod_tree_map
  implicit none

  type t_entry
    private
    integer :: key
    integer :: val
  end type t_entry

  type t_node
    private
    type(t_node), pointer :: left => null(), right => null()
    integer :: level = 1
    type(t_entry), pointer :: e => null()
  end type t_node

  type t_tree_map
    private
    type(t_node), pointer :: root => null()
    integer :: deflt = -1
  end type t_tree_map

contains

  function new_entry(key,val) result(e)
    integer, intent(in) :: key
    integer, intent(in) :: val
    type(t_entry), pointer :: e
    e => null()
    allocate(e)
    e%key = key
    e%val = val
  end

  function new_node(e) result(n)
    type(t_entry), pointer, intent(in) :: e
    type(t_node), pointer :: n
    n => null()
    allocate(n)
    n%e => e
  end

  integer function level(n)
    type(t_node), pointer, intent(in) :: n
    level = 0
    if (.not.associated(n)) return
    level = n%level
  end

  logical function is_leaf(n)
    type(t_node), pointer, intent(in) :: n
    is_leaf = associated(n) .and. .not.associated(n%left) .and. &
    &         .not.associated(n%right)
  end

  recursive function tree_size(n) result(s)
    type(t_node), pointer, intent(in) :: n
    integer :: s
    s = 0
    if (.not.associated(n)) return
    s = 1+tree_size(n%left)+tree_size(n%right)
  end

  function skew(n) result(l)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: l
    l => n
    if (.not.(associated(n) .and. associated(n%left) .and. &
    &   n%left%level == n%level)) return
    l => n%left
    n%left => l%right
    l%right => n
  end

  function split(n) result(r)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: r
    r => n
    if (.not.(associated(n) .and. associated(n%right) .and. &
    &   associated(n%right%right) .and. n%right%right%level == n%level)) return
    r => n%right
    n%right => r%left
    r%left => n
    r%level = r%level+1
  end

  function predecessor(n) result(p)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: p
    p => null()
    if (.not.associated(n%left)) return
    p => n%left
    do while (associated(p%right))
      p => p%right
    end do
  end

  function successor(n) result(s)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: s
    s => null()
    if (.not.associated(n%right)) return
    s => n%right
    do while (associated(s%left))
      s => s%left
    end do
  end

  recursive function insert(n,e) result(t)
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
  end

  recursive function delete(n,e) result(t)
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
  end

  function decrease_level(n) result(t)
    type(t_node), pointer, intent(in) :: n
    type(t_node), pointer :: t
    integer :: should_be
    t => n
    should_be = min(level(t%left),level(t%right))+1
    if (t%level > should_be) then
      t%level = should_be
      if (level(t%right) > should_be) t%right%level = should_be
    end if
  end

  recursive subroutine release_tree(t)
    type(t_node), pointer, intent(inout) :: t
    if (.not.associated(t)) return
    call release_tree(t%left)
    call release_tree(t%right)
    deallocate(t)
  end

  recursive subroutine get_keys_list(t,keys,num)
    type(t_node), pointer, intent(in) :: t
    integer, intent(inout) :: keys(:)
    integer, intent(inout) :: num
    if (.not.associated(t)) return
    call get_keys_list(t%left,keys,num)
    num = num+1
    keys(num) = t%e%key
    call get_keys_list(t%right,keys,num)
  end

  integer function size_of(map)
    type(t_tree_map), intent(in) :: map
    size_of = tree_size(map%root)
  end

  subroutine clear(map)
    type(t_tree_map), intent(inout) :: map
    call release_tree(map%root)
    map%root => null()
  end

  subroutine set_default(map,deflt)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: deflt
    map%deflt = deflt
  end

  subroutine put_entry(map,e)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    map%root => insert(map%root,e)
  end

  subroutine remove_entry(map,e)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    map%root => delete(map%root,e)
  end

  function get_entry(map,e) result(ret)
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
  end

  function contain_entry(map,e) result(ret)
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
  end

  function get_first_entry(map) result(ret)
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
  end

  function poll_first_entry(map) result(ret)
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
  end

  function get_last_entry(map) result(ret)
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
  end

  function poll_last_entry(map) result(ret)
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
  end

  function floor_entry(map,e) result(ret)
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
  end

  function lower_entry(map,e) result(ret)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_entry), pointer :: ret
    ret => floor_entry(map,new_entry(e%key-1,0))
  end

  function ceiling_entry(map,e) result(ret)
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
  end

  function higher_entry(map,e) result(ret)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer, intent(in) :: e
    type(t_entry), pointer :: ret
    ret => ceiling_entry(map,new_entry(e%key+1,0))
  end

  subroutine get_keys(map,keys,num)
    type(t_tree_map), intent(inout) :: map
    integer, intent(inout) :: keys(:)
    integer, intent(inout) :: num
    keys = 0
    num = 0
    call get_keys_list(map%root,keys,num)
  end

  subroutine put(map,key,val)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    integer, intent(in) :: val
    call put_entry(map,new_entry(key,val))
  end

  subroutine remove(map,key)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    call remove_entry(map,new_entry(key,0))
  end

  function get(map,key) result(val)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: val
    val = map%deflt
    tmp => get_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    val = tmp%val
  end

  logical function contain(map,key)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    contain = contain_entry(map,new_entry(key,0))
  end

  function get_first_key(map) result(key)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer :: tmp
    integer :: key
    key = map%deflt
    tmp => get_first_entry(map)
    if (.not.associated(tmp)) return
    key = tmp%key
  end

  function get_last_key(map) result(key)
    type(t_tree_map), intent(inout) :: map
    type(t_entry), pointer :: tmp
    integer :: key
    key = map%deflt
    tmp => get_last_entry(map)
    if (.not.associated(tmp)) return
    key = tmp%key
  end

  function floor_key(map,key) result(ret)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret
    ret = map%deflt
    tmp => floor_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
  end

  function lower_key(map,key) result(ret)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret
    ret = map%deflt
    tmp => lower_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
  end

  function ceiling_key(map,key) result(ret)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret
    ret = map%deflt
    tmp => ceiling_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
  end

  function higher_key(map,key) result(ret)
    type(t_tree_map), intent(inout) :: map
    integer, intent(in) :: key
    type(t_entry), pointer :: tmp
    integer :: ret
    ret = map%deflt
    tmp => higher_entry(map,new_entry(key,0))
    if (.not.associated(tmp)) return
    ret = tmp%key
  end

end module mod_tree_map
