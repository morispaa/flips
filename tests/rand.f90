module rand
  ! Module for different random number utilities
  ! (c) 2005, Mikko Orispaa <mikko.orispaa@oulu.fi>
  ! Licensed under GPL version 2

 ! private

  interface nrand
     module procedure nrands_s, nrands_d,nrandv_s,nrandv_d,nrandm_s,nrandm_d
  end interface


 ! public :: set_rand_seed, nrand, intrand


contains

  subroutine set_rand_seed()
    ! Sets random seed using system clock
    implicit none

    integer :: s
    integer, dimension(8) :: t
    integer, dimension(:), allocatable :: seed

    call random_seed(size = s)
    allocate(seed(s))
    call date_and_time(values = t)
    seed = 100*t(7) + t(8)/10
    call random_seed(put = seed)
  end subroutine set_rand_seed


  !  subroutine nrands_d(num)
  !    ! returns normally distributed (pseudo) random number
  !    implicit none

  !    real(8), parameter :: pi = 3.141592653589793238462643

  !    real(8) :: num

  !    real(8), dimension(2) :: x

  !    call random_number(x)

  !    num = sqrt(-2*log(x(1)))*cos(2*pi*x(2))

  !  end subroutine nrands_d


  subroutine nrands_s(num)
    ! Returns normally distributed random number (polar-form of the
    ! Box-Muller transformation). More robust numerically and faster.
    implicit none

    real(4) :: num
    real(4) :: x_1,x_2,w

    w = 2.0D0

    do while ( w >= 1.0D0 )
       call random_number(x_1)
       call random_number(x_2)
       x_1 = 2.0D0*x_1 - 1.0D0
       x_2 = 2.0D0*x_2 - 1.0D0
       w = x_1**2 + x_2**2
    end do

    w = sqrt((-2.0D0*log(w))/w)
    num = x_1 * w
  end subroutine nrands_s


  subroutine nrands_d(num)
    ! Returns normally distributed random number (polar-form of the
    ! Box-Muller transformation). More robust numerically and faster.
    implicit none

    real(8) :: num
    real(8) :: x_1,x_2,w

    w = 2.0D0

    do while ( w >= 1.0D0 )
       call random_number(x_1)
       call random_number(x_2)
       x_1 = 2.0D0*x_1 - 1.0D0
       x_2 = 2.0D0*x_2 - 1.0D0
       w = x_1**2 + x_2**2
    end do

    w = sqrt((-2.0D0*log(w))/w)
    num = x_1 * w
  end subroutine nrands_d


  subroutine nrandv_d(vec)
    ! Returns normally distributed random vector
    implicit none

    real(8), dimension(:) :: vec
    real(8) :: rnum
    integer :: i, n 

    n = size(vec)

    do i=1,n
       call nrands_d(rnum)
       vec(i) = rnum
    end do

  end subroutine nrandv_d


  subroutine nrandv_s(vec)
    ! Returns normally distributed random vector
    implicit none

    real(4), dimension(:) :: vec
    real(4) :: rnum
    integer :: i, n 

    n = size(vec)

    do i=1,n
       call nrands_s(rnum)
       vec(i) = rnum
    end do

  end subroutine nrandv_s


  subroutine nrandm_d(mat)
    ! Returns normally distributed random matrix
    real(8), dimension(:,:) :: mat
    real(8) :: rnum
    integer :: m,n,i,j

    m = size(mat,1)
    n = size(mat,2)

    do i = 1,m
       do j = 1,n
          call nrands_d(rnum)
          mat(i,j) = rnum
       end do
    end do
  end subroutine nrandm_d


  subroutine nrandm_s(mat)
    ! Returns normally distributed random matrix
    real(4), dimension(:,:) :: mat
    real(4) :: rnum
    integer :: m,n,i,j

    m = size(mat,1)
    n = size(mat,2)

    do i = 1,m
       do j = 1,n
          call nrands_s(rnum)
          mat(i,j) = rnum
       end do
    end do
  end subroutine nrandm_s





  function randi(min,max) result(rnum)
    ! Gives a random integer between min and max
    implicit none

    integer :: min, max, rnum
    real(4) :: rand_num

    call random_number(rand_num)

    rnum = floor(rand_num * (max+1 - min) + min)

  end function randi
!!$  


end module rand
