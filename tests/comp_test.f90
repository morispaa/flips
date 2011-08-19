program comp_test
  use flips

  ! This program tests single and double precision reals in FLIPS

  implicit none

  !integer, parameter :: sp = kind(1.0E0)
  !integer, parameter :: dp = kind(1.0D0)

  real(dp) :: t,f
  integer :: sstart,send,sstep,bmin,bmax,bstep,loopcnt,order, prec,mem
  integer :: oa,ob,oc,ia,ib,ic, i, j, bs
  logical :: flag = .FALSE.,use_files

  write(*,*) 'Give precision:'
  write(*,*) '1. Single'
  write(*,*) '2. Double'
  write(*,*) 'Anything else terminates'
  read(*,*) prec
  if (.NOT.(prec==1 .OR. prec==2)) then
     write(*,*) 'Bye!'
     stop
  end if

  write(*,*) 'Memory or binary files:'
  write(*,*) '1. Memory'
  write(*,*) '2. Binary files'
  write(*,*) 'Anything else terminates'
  read(*,*) mem
  if (.NOT.(mem==1 .OR. mem==2)) then
     write(*,*) 'Bye!'
     stop
  end if

  if (mem==1) then
     use_files=.FALSE.
  else
     use_files=.TRUE.
  end if

  write(*,*) 'Give start size, end size, step'
  read(*,*) sstart,send,sstep
  if (sstep==0) sstep=1
  write(*,*) 'Give buffer min size, max size, step'
  read(*,*) bmin,bmax,bstep
  if (bstep==0) bstep=1

  write(*,*) 'Give order:'
  write(*,*) '1. By buffer size'
  write(*,*) '2. By matrix size'
  write(*,*) 'Anything else terminates'
  read(*,*) order

  select case (order)

  case(1)
     do i = bmin,bmax,bstep
        do j = sstart,send,sstep

           loopcnt = floor(max(1.0,1100*exp(-0.01*j)+1))

           if (i==0) then
              if (prec==1) then
                 call test_run_c(j,1,loopcnt,use_files,t,f)
              else
                 call test_run_z(j,1,loopcnt,use_files,t,f)
              end if
              bs = 1
           else

              if (prec==1) then
                 call test_run_c(j,i,loopcnt,use_files,t,f)
              else
                 call test_run_z(j,i,loopcnt,use_files,t,f)
              end if
              bs = i
           end if

           write(*,'(i6,i6,f15.8,f6.3)') j,bs,t

        end do
     end do

  case(2)
     do i = sstart,send,sstep
        do j = bmin,bmax,bstep

           loopcnt = floor(max(1.0,1100*exp(-0.01*i)+1))

           if (j==0) then
              if (prec==1) then
                 call test_run_c(i,1,loopcnt,use_files,t,f)
              else
                 call test_run_z(i,1,loopcnt,use_files,t,f)
              end if
              bs = 1
           else

              if (prec==1) then
                 call test_run_c(i,j,loopcnt,use_files,t,f)
              else
                 call test_run_z(i,j,loopcnt,use_files,t,f)
              end if
              bs = j
           end if


           write(*,'(i6,i6,f15.8,f6.3)') i,bs,t,f/t/1.0D9

        end do
     end do
  case default
     write(*,*) 'Bye!'
     stop
  end select









contains

  subroutine test_run_c(size,buffer,loops,files,time,flops)
    implicit none

    integer, intent(in) :: size, buffer, loops
    logical :: files
    real(dp), intent(out) :: time, flops

    type(flips_c) :: gs
    real(dp) :: ts,te,total
    complex(sp), dimension(:), allocatable :: amat
    complex(sp), dimension(:), allocatable :: mvec
    real(sp), dimension(:), allocatable :: aux1,aux2
    integer :: n

    allocate(amat(size*size),mvec(size),aux1(size*size),aux2(size))

    time = 0.0D0
    flops = 0.0

    ! Make data
    call random_number(aux1)
    amat = aux1
    call random_number(aux1)
    amat = amat + (0.0,1.0)*aux1
    call random_number(aux2)
    mvec = aux2
    call random_number(aux2)
    mvec = mvec + (0.0,1.0) * aux2

    ! Start clock
    call cpu_time(ts)

    ! Loop
    do n = 1,loops

       ! Initialize flips object
       if (files) then
          call flips_init(gs,size,1,idnum=100,buffersize=buffer)
       else
          call flips_init(gs,size,1,buffersize=buffer)
       end if

       ! Feed the data
       call flips_add(gs,size,amat,mvec)
       ! Solve
       call flips_solve(gs)

       ! Update flops
       flops = flops + gs%fc

       ! Kill fob
       call flips_kill(gs,.FALSE.)

    end do

    ! Stop clock
    call cpu_time(te)

    !  time
    time = (te-ts)/loops
    ! flops
    flops = flops/loops

    deallocate(amat,mvec)

  end subroutine test_run_c


  subroutine test_run_z(size,buffer,loops,files,time,flops)
    implicit none

    integer, intent(in) :: size, buffer, loops
    logical :: files
    real(dp), intent(out) :: time, flops

    type(flips_z) :: gs
    real(dp) :: ts,te,total
    complex(dp), dimension(:), allocatable :: amat
    complex(dp), dimension(:), allocatable :: mvec
    real(dp), dimension(:), allocatable :: aux1,aux2
    integer :: n

    allocate(amat(size*size),mvec(size),aux1(size*size),aux2(size))

    time = 0.0D0
    flops = 0

    ! Make data
    call random_number(aux1)
    amat = aux1
    call random_number(aux1)
    amat = amat + (0.0_dp,1.0_dp)*aux1
    call random_number(aux2)
    mvec = aux2
    call random_number(aux2)
    mvec = mvec + (0.0_dp,1.0_dp) * aux2

    ! Start clock
    call cpu_time(ts)

    ! Loop
    do n = 1,loops

       ! Initialize flips object
       if (files) then
          call flips_init(gs,size,1,idnum=100,buffersize=buffer)
       else
          call flips_init(gs,size,1,buffersize=buffer)
       end if

       ! Feed the data
       call flips_add(gs,size,amat,mvec)
       ! Solve
       call flips_solve(gs)

       ! Update flops
       flops = flops + gs%fc

       ! Kill fob
       call flips_kill(gs,.FALSE.)

    end do

    ! Stop clock
    call cpu_time(te)

    !  time
    time = (te-ts)/loops
    ! flops
    flops = flops/loops

    deallocate(amat,mvec)

  end subroutine test_run_z



end program comp_test
