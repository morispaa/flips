program real_test
  use flips
  use rand
  use flipstest

  ! This program tests single and double precision reals in FLIPS

  implicit none

  !integer, parameter :: sp = kind(1.0E0)
  !integer, parameter :: dp = kind(1.0D0)

  real(dp) :: t,f
  integer :: sstart,send,sstep,bmin,bmax,bstep,loopcnt,order, prec,mem,&
       matmode, bsize, csize
  integer :: oa,ob,oc,ia,ib,ic, i, j, bs,h
  logical :: flag = .FALSE.,use_files
  real(sp) :: flns

  write(*,*) 'Give precision:'
  write(*,*) '1. Single'
  write(*,*) '2. Double'
  write(*,*) 'Anything else terminates'
  read(*,*) prec
  if (.NOT.(prec==1 .OR. prec==2)) then
     write(*,*) 'Bye!'
     stop
  end if

  write(*,*) 'Give direct theory matrix mode:'
  write(*,*) '1. Full random'
  write(*,*) '2. Band random'
  write(*,*) '3. Band random with common variables'
  write(*,*) '4. Random sparse'
  write(*,*) 'Anything else terminates'
  read(*,*) matmode
  if (.NOT.(matmode==1 .OR. matmode==2 .OR. matmode==3 .OR. matmode==4)) then
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

  if (matmode==1) then
     bsize = -1
     csize = -1
  end if

  if (matmode==2) then
     write(*,*) 'Give maximum band size'
     read(*,*) bsize
     csize = -1
  end if

  if (matmode==3) then
     write(*,*) 'Give maximum band size'
     read(*,*) bsize
     write(*,*) 'Give number od common variables'
     read(*,*) csize
  end if

  if (matmode==4) then
     write(*,*) 'Give fullness of the sparse matrix'
     read(*,*) flns
     bsize = -1
     csize = floor(flns*100) 
  end if



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
                 call test_run_s(j,1,bsize,csize,loopcnt,use_files,t,f)
              else
                 call test_run_d(j,1,bsize,csize,loopcnt,use_files,t,f)
              end if
              bs = 1
           else
           
              if (prec==1) then
                 call test_run_s(j,i,bsize,csize,loopcnt,use_files,t,f)
              else
                 call test_run_d(j,i,bsize,csize,loopcnt,use_files,t,f)
              end if
              bs = i
           end if

	       write(*,'(i6,i6,f15.8,f6.3)') j,bs,t,f/t/1.0D9
	
        end do
     end do

  case(2)
     do i = sstart,send,sstep
        do j = bmin,bmax,bstep

           loopcnt = floor(max(1.0,1100*exp(-0.01*i)+1))

           if (j==0) then
              if (prec==1) then
                 call test_run_s(i,1,bsize,csize,loopcnt,use_files,t,f)
              else
                 call test_run_d(i,1,bsize,csize,loopcnt,use_files,t,f)
              end if
              bs = 1
           else

              if (prec==1) then
                 call test_run_s(i,j,bsize,csize,loopcnt,use_files,t,f)
              else
                 call test_run_d(i,j,bsize,csize,loopcnt,use_files,t,f)
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

  subroutine test_run_s(size,buffer,bsize,csize,loops,files,time,flops)
    implicit none

    integer, intent(in) :: size, buffer, bsize, csize, loops
    logical :: files
    real(dp), intent(out) :: time, flops

    type(flips_s) :: gs
    real(dp) :: ts,te,total
    real(sp), dimension(:,:), allocatable :: amat
    real(sp), dimension(:,:), allocatable :: mvec
    integer :: n

    allocate(amat(size,size),mvec(size,1))


 


    time = 0.0D0
    flops = 0.0D0

    ! Make data
!write(*,*) 'Construct data'
    call construct_mat_s(size,bsize,csize,amat)
    call nrand(mvec)
!write(*,*) 'Data constructed'

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
       do h = 1,size
          call flips_add(gs,1,amat(h,:),mvec(h,:))
       end do
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

  end subroutine test_run_s


  subroutine test_run_d(size,buffer,bsize,csize,loops,files,time,flops)
    implicit none

    integer, intent(in) :: size, buffer, bsize, csize, loops
    logical :: files
    real(dp), intent(out) :: time, flops

    type(flips_d) :: gs
    real(dp) :: ts,te,total
    real(dp), dimension(:,:), allocatable :: amat
    real(dp), dimension(:,:), allocatable :: mvec
    integer :: n

    allocate(amat(size,size),mvec(size,1))





    time = 0.0D0
    flops = 0.0D0

    ! Make data
    call construct_mat_d(size,bsize,csize,amat)
    call nrand(mvec)

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
       do h = 1,size
          call flips_add(gs,1,amat(h,:),mvec(h,:))
       end do
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

  end subroutine test_run_d


subroutine construct_mat_s(size,bsize,csize,mat)
  implicit none

  integer, intent(in) :: size, bsize, csize
  real(sp), dimension(:,:), intent(out) :: mat

  if (bsize==-1 .AND. csize==-1) then
     call nrand(mat)
     return
  end if

  if (bsize==-1 .AND. csize>0) then
     call random_sparse_matrix(real(csize/100.0),mat)
     return
  end if

  if(bsize>0 .AND. csize==-1) then
     call random_band_matrix(bsize,mat)
     return
  end if

  if (bsize>0 .AND. csize>0) then
     call random_band_matrix_with_common(bsize,csize,mat)
     return
  end if

end subroutine construct_mat_s


subroutine construct_mat_d(size,bsize,csize,mat)
  implicit none

  integer, intent(in) :: size, bsize, csize
  real(dp), dimension(:,:), intent(out) :: mat

  if (bsize==-1 .AND. csize==-1) then
     call nrand(mat)
     return
  end if

  if (bsize==-1 .AND. csize>0) then
     call random_sparse_matrix(real(csize/100.0,dp),mat)
     return
  end if

  if(bsize>0 .AND. csize==-1) then
     call random_band_matrix(bsize,mat)
     return
  end if

  if (bsize>0 .AND. csize>0) then
     call random_band_matrix_with_common(bsize,csize,mat)
     return
  end if

end subroutine construct_mat_d



  
  

end program real_test
