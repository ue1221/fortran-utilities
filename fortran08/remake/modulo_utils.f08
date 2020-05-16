module modulo_utils
  use module_modint
  implicit none
  type(modint), allocatable :: f(:), invf(:)
contains
  subroutine init(n)
    integer, intent(in) :: n
    integer :: i
    if (allocated(f)) deallocate(f)
    if (allocated(invf)) deallocate(invf)
    allocate(f(0:n),invf(0:n))
    f(0) = 1
    do i = 1, n
      f(i) = f(i-1)*i
    end do
    invf(n) = f(n)%inv()
    do i = n, 1, -1
      invf(i-1) = invf(i)*i
    end do
  end
  pure elemental type(modint) function perm(n,k)
    integer, intent(in) :: n, k
    if (k > n .or. n < 0 .or. k < 0) return
    perm = f(n)*invf(n-k)
  end
  pure elemental type(modint) function comb(n,k)
    integer, intent(in) :: n, k
    comb = perm(n,k)*invf(k)
  end
  pure elemental type(modint) function homo(n,k)
    integer, intent(in) :: n, k
    homo = 1
    if (n == 0 .and. k == 0) return
    homo = comb(n+k-1,k)
  end
end module modulo_utils
