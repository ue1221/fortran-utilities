module mod_hash_set
  implicit none
  integer, parameter :: int_max_value = 2147483647
  integer, parameter :: default_initial_capacity = 16
  integer, parameter :: maximum_capacity = lshift(1, 30)
  real, parameter :: default_load_factor = 0.75

  type t_element
    integer :: key
    type(t_element), pointer :: next => null()
    integer :: hash
  end type

  type t_element_ptr
    type(t_element), pointer :: ref => null()
  end type

  type t_hash_set
    type(t_element_ptr), private, allocatable :: table(:)
    integer :: size = 0
    integer, private :: threshold = int(default_initial_capacity * default_load_factor)
    real, private :: load_factor = default_load_factor
  contains
    procedure :: is_empty => is_empty
    procedure :: is_not_empty => is_not_empty
    procedure :: add => add
    procedure :: add_all => add_all
    procedure :: remove => remove
    procedure :: clear => clear
    procedure :: contains => contains_key
    procedure :: get_keys => get_keys
  end type

  interface hash_set
    module procedure :: newhs0, newhs1, newhs2
  end interface

contains

  function new_element(key, h) result(res)
    integer, intent(in) :: key
    integer, intent(in) :: h
    type(t_element), pointer :: res
    allocate(res)
    res%key = key
    res%hash = h
  end

  integer function hash_code(i) result(res)
    integer, intent(in) :: i
    res = i
  end

  type(t_hash_set) function newhs0() result(res)
    allocate(res%table(default_initial_capacity))
  end

  type(t_hash_set) function newhs1(initial_capacity) result(res)
    integer, intent(in) :: initial_capacity
    res = newhs2(initial_capacity, default_load_factor)
  end

  type(t_hash_set) function newhs2(initial_capacity, load_factor) result(res)
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
    class(t_hash_set), intent(in) :: this
    res = this%size == 0
  end

  logical function is_not_empty(this) result(res)
    class(t_hash_set), intent(in) :: this
    res = this%size /= 0
  end

  logical function contains_key(this, key) result(res)
    class(t_hash_set), intent(in) :: this
    integer, intent(in) :: key
    type(t_element), pointer :: e
    e => get_element(this, key)
    res = associated(e)
  end

  function get_element(this, key) result(res)
    class(t_hash_set), intent(in) :: this
    integer, intent(in) :: key
    integer :: h
    type(t_element), pointer :: e
    type(t_element), pointer :: res
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

  subroutine add(this, key)
    class(t_hash_set), intent(inout) :: this
    integer, intent(in) :: key
    integer :: h, i
    type(t_element), pointer :: e
    h = hash(hash_code(key))
    i = index_for(h, size(this%table))
    e => this%table(i)%ref
    do while (associated(e))
      if (e%hash == h .and. e%key == key) return
      e => e%next
    end do
    call add_element(this, key, h, i)
  end

  subroutine add_element(this, key, h, idx)
    class(t_hash_set), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: h, idx
    type(t_element), pointer :: e
    e => this%table(idx)%ref
    this%table(idx)%ref => new_element(key, h)
    this%table(idx)%ref%next => e
    this%size = this%size + 1
    if (this%size >= this%threshold) call resize(this, 2 * size(this%table))
  end

  subroutine add_all(this, keys)
    class(t_hash_set), intent(inout) :: this
    integer, intent(in) :: keys(:)
    integer :: i
    do i = 1, size(keys)
      call add(this, keys(i))
    end do
  end

  subroutine resize(this, new_capacity)
    class(t_hash_set), intent(inout) :: this
    integer, intent(in) :: new_capacity
    integer :: capacity, i, j
    type(t_element), pointer :: e, next
    type(t_element_ptr) :: table(new_capacity)
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
    class(t_hash_set), intent(inout) :: this
    integer, intent(in) :: key
    integer :: h, i
    type(t_element), pointer :: e, prev, next
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
    class(t_hash_set), intent(inout) :: this
    deallocate(this%table)
    allocate(this%table(default_initial_capacity))
    this%size = 0
  end

  function get_keys(this) result(res)
    class(t_hash_set), intent(in) :: this
    integer :: res(this%size)
    integer :: i, k
    type(t_element), pointer :: e
    k = 0
    do i = 1, size(this%table)
      e => this%table(i)%ref
      do while (associated(e))
        k = k + 1
        res(k) = e%key
        e => e%next
      end do
    end do
  end
end module mod_hash_set