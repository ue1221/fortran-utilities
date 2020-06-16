module mod_hash_map
  implicit none
  integer, parameter :: int_max_value = 2147483647
  integer, parameter :: default_initial_capacity = 16
  integer, parameter :: maximum_capacity = lshift(1, 30)
  real, parameter :: default_load_factor = 0.75

  type t_entry
    integer :: key
    integer :: val
    type(t_entry), pointer :: next => null()
    integer :: hash
  end type

  type t_entry_ptr
    type(t_entry), pointer :: ref => null()
  end type

  type t_hash_map
    type(t_entry_ptr), private, allocatable :: table(:)
    integer :: size = 0
    integer, private :: threshold = int(default_initial_capacity * default_load_factor)
    real, private :: load_factor = default_load_factor
  contains
    procedure :: is_empty => is_empty
    procedure :: is_not_empty => is_not_empty
    procedure :: put => put
    procedure :: get => get
    procedure :: get_or_default => get_or_default
    procedure :: remove => remove
    procedure :: clear => clear
    procedure :: contains_key => contains_key
    procedure :: contains_val => contains_val
  end type

  interface hash_map
    module procedure :: newhm0, newhm1, newhm2
  end interface

contains

  function new_entry(key, val, h) result(res)
    integer, intent(in) :: key
    integer, intent(in) :: val
    integer, intent(in) :: h
    type(t_entry), pointer :: res
    allocate(res)
    res%key = key
    res%val = val
    res%hash = h
  end

  integer function hash_code(i) result(res)
    integer, intent(in) :: i
    res = i
  end

  type(t_hash_map) function newhm0() result(res)
    allocate(res%table(default_initial_capacity))
  end

  type(t_hash_map) function newhm1(initial_capacity) result(res)
    integer, intent(in) :: initial_capacity
    res = newhm2(initial_capacity, default_load_factor)
  end

  type(t_hash_map) function newhm2(initial_capacity, load_factor) result(res)
    integer, intent(in) :: initial_capacity
    real, intent(in) :: load_factor
    integer :: capacity
    if (initial_capacity < 0) then
      capacity = default_initial_capacity
    else
      capacity = 1
      do while (capacity < min(initial_capacity, maximum_capacity))
        capacity = lshift(capacity, 1)
      end do
    end if

    if (load_factor <= 0 .or. load_factor /= load_factor) then
      res%load_factor = default_load_factor
    else
      res%load_factor = load_factor
    end if

    res%threshold = int(capacity * res%load_factor)
    allocate(res%table(capacity))
  end

  integer function shr(i, n) result(res)
    integer, intent(in) :: i, n
    res = ibclr(rshift(i, n), 31)
  end

  integer function hash(i) result(res)
    integer, intent(in) :: i
    integer :: h
    h = i
    h = xor(h, xor(shr(h, 20), shr(h, 12)))
    res = xor(h, xor(shr(h, 7), shr(h, 4)))
  end

  integer function index_for(h, length) result(res)
    integer, intent(in) :: h, length
    res = and(h, length - 1) + 1
  end

  logical function is_empty(this) result(res)
    class(t_hash_map), intent(in) :: this
    res = this%size == 0
  end

  logical function is_not_empty(this) result(res)
    class(t_hash_map), intent(in) :: this
    res = this%size /= 0
  end

  integer function get(this, key) result(res)
    class(t_hash_map), intent(in) :: this
    integer, intent(in) :: key
    integer :: h
    type(t_entry), pointer :: e
    h = hash(hash_code(key))
    e => this%table(index_for(h, size(this%table)))%ref
    do while (associated(e))
      if (e%hash == h .and. e%key == key) then
        res = e%val
        return
      end if
      e => e%next
    end do
    res = 0
  end

  integer function get_or_default(this, key, def) result(res)
    class(t_hash_map), intent(in) :: this
    integer, intent(in) :: key
    integer, intent(in) :: def
    integer :: h
    type(t_entry), pointer :: e
    h = hash(hash_code(key))
    e => this%table(index_for(h, size(this%table)))%ref
    do while (associated(e))
      if (e%hash == h .and. e%key == key) then
        res = e%val
        return
      end if
      e => e%next
    end do
    res = def
  end

  logical function contains_key(this, key) result(res)
    class(t_hash_map), intent(in) :: this
    integer, intent(in) :: key
    type(t_entry), pointer :: e
    e => get_entry(this, key)
    res = associated(e)
  end

  function get_entry(this, key) result(res)
    class(t_hash_map), intent(in) :: this
    integer, intent(in) :: key
    integer :: h
    type(t_entry), pointer :: e
    type(t_entry), pointer :: res
    h = hash(hash_code(key))
    e => this%table(index_for(h, size(this%table)))%ref
    do while (associated(e))
      if (e%hash == h .and. e%key == key) then
        res => e
        return
      end if
      e => e%next
    end do
    res => null()
  end

  subroutine put(this, key, val)
    class(t_hash_map), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: val
    integer :: h, i
    type(t_entry), pointer :: e
    h = hash(hash_code(key))
    i = index_for(h, size(this%table))
    e => this%table(i)%ref
    do while (associated(e))
      if (e%hash == h .and. e%key == key) then
        e%val = val
        return
      end if
      e => e%next
    end do
    call add_entry(this, key, val, h, i)
  end

  subroutine add_entry(this, key, val, h, idx)
    class(t_hash_map), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: val
    integer, intent(in) :: h, idx
    type(t_entry), pointer :: e
    e => this%table(idx)%ref
    this%table(idx)%ref => new_entry(key, val, h)
    this%table(idx)%ref%next => e
    this%size = this%size + 1
    if (this%size >= this%threshold) call resize(this, 2 * size(this%table))
  end

  subroutine resize(this, new_capacity)
    class(t_hash_map), intent(inout) :: this
    integer, intent(in) :: new_capacity
    integer :: capacity, i, j
    type(t_entry), pointer :: e, next
    type(t_entry_ptr) :: table(new_capacity)
    capacity = size(this%table)
    if (capacity == maximum_capacity) then
      this%threshold = int_max_value
      return
    end if

    do j = 1, capacity
      e => this%table(j)%ref
      if (associated(e)) then
        this%table(j)%ref => null()
        do
          next => e%next
          i = index_for(e%hash, new_capacity)
          e%next => table(i)%ref
          table(i)%ref => e
          e => next
          if (.not.associated(e)) exit
        end do
      end if
    end do

    deallocate(this%table)
    allocate(this%table(new_capacity))
    do j = 1, new_capacity
      this%table(j)%ref => table(j)%ref
    end do
    this%threshold = int(new_capacity * this%load_factor)
  end

  subroutine remove(this, key)
    class(t_hash_map), intent(inout) :: this
    integer, intent(in) :: key
    integer :: h, i
    type(t_entry), pointer :: e, prev, next
    h = hash(hash_code(key))
    i = index_for(h, size(this%table))
    prev => this%table(i)%ref
    e => prev
    do while (associated(e))
      next => e%next
      if (e%hash == h .and. e%key == key) then
        this%size = this%size - 1
        if (associated(prev, e)) then
          this%table(i)%ref => next
        else
          prev%next => next
        end if
        return
      end if
      prev => e
      e => next
    end do
  end

  subroutine clear(this)
    class(t_hash_map), intent(inout) :: this
    deallocate(this%table)
    allocate(this%table(default_initial_capacity))
    this%size = 0
  end

  logical function contains_val(this, val) result(res)
    class(t_hash_map), intent(in) :: this
    integer, intent(in) :: val
    integer :: i
    type(t_entry), pointer :: e
    do i = 1, size(this%table)
      e => this%table(i)%ref
      do while (associated(e))
        if (e%val == val) then
          res = .true.
          return
        end if
        e => e%next
      end do
    end do
    res = .false.
  end
end module mod_hash_map