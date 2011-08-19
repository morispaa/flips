program deletetest
  use flips

  ! Reads the riometer data and makes a fit iteratively. 
  ! Outliers are deleted in every step.

  ! Writes the result as a file rio_res_<prec>.dat

  implicit none

  type(flips_d) :: h

  integer, parameter :: xlen = 40
  real(dp), parameter :: limit = 2.9
  real(dp), parameter :: pi = 3.14159265358979

  real(dp), dimension(2048) :: riodata, xax, fit,tmpx,tmpr
  real(dp), dimension(2048,xlen) :: model,tmpm
  real(dp), dimension(xlen) :: ntmp, sol, tmpsol
  real(dp) :: khi
  integer :: i,idx,currentProblemSize, auxidx,pidx
  integer, dimension(2048) :: oliers,modelmask
  logical, dimension(2048) :: olmask

  ! Read riometer data
  open(123,FILE="rio_d.data",FORM='unformatted',access='stream')
  read(123,pos=1) riodata
  close(123)

  !write(*,*) riodata(1:10)
  !write(*,*) riodata(2048)

  ! Construct model matrix (sinusoidal model)
  xax_const: do i = 1,2048
     xax(i) = real(i)
  end do xax_const
  xax = (xax - 1.0)/2048.0

  ntmp_const: do i = 1,xlen
     ntmp(i) = real(i)
  end do ntmp_const

  model_const: do i = 1,2048
     model(i,:) = sin(pi * xax(i) * ntmp)
  end do model_const

  ! First FLIPS solution 

  currentProblemSize = 2048

  call flips_init(h,xlen,1,buffersize=50)
  fadd: do i = 1,currentProblemSize
     call flips_add(h,1,model(i,:),(/ riodata(i) /))
  end do fadd

  !sol = 0.0

  ! Iteration starts here!
  main_iteration: do




     call flips_solve(h)
     !tmpsol = sol
     call flips_get('solu',h,sol)

     !write(*,*) sol

     fit = 0.0

     fit(1:currentProblemSize) = matmul(model(1:currentProblemSize,:),sol)

     khi = sqrt(sum((riodata(1:currentProblemSize)-fit(1:currentProblemSize))**2)/currentProblemSize)

     write(*,*) 'khi:',khi



!!$     ! Find outliers
!!$     idx = 0
!!$     oliers = 0
!!$     ol_search: do i = 1,currentProblemSize
!!$        if(abs(riodata(i) - fit(i)) > khi * limit) then
!!$           idx = idx + 1
!!$           oliers(idx) = i
!!$        end if
!!$     end do ol_search
!!$
!!$     write(*,*) oliers(1:idx)
!!$
!!$     !currentProblemSize = currentProblemSize - idx

     olmask = .FALSE.
     where(abs(riodata(1:currentProblemSize) - fit(1:currentProblemSize)) > khi * limit)
        olmask(1:currentProblemSize) = .TRUE.
     end where

     idx = count(olmask)

     write(*,*) 'Number of accepted data points:',currentProblemSize - idx,'rejected:',idx

     if (idx == 0) then
        exit main_iteration
     end if

     ! Delete rejected points
     deletion: do i = 1,currentProblemSize
        if (olmask(i)) then
           call flipsdelete_d(h,1,model(i,:),riodata(i))
        end if
     end do deletion

!!$     ! Reformat data and model
!!$     auxidx = 1
!!$     pidx = 0
!!$     tmpx = 0.0
!!$     tmpr = 0.0
!!$     tmpm = 0.0
!!$     reformat: do i = 1,currentProblemSize
!!$        if (i == oliers(auxidx)) then
!!$           auxidx = auxidx + 1
!!$           !if (auxidx == idx + 1) then
!!$           !   exit reformat
!!$           !else
!!$           cycle reformat
!!$           !end if
!!$        else
!!$           pidx = pidx + 1
!!$           tmpx(pidx) = xax(i)
!!$           tmpr(pidx) = riodata(i)
!!$           tmpm(pidx,:) = model(i,:)
!!$        end if
!!$     end do reformat
!!$     !write(*,*) 'auxidx:',auxidx,'idx:',idx,'pidx:',pidx
!!$

!!$
!!$     xax = 0.0
!!$     riodata = 0.0
!!$     model = 0.0
!!$
!!$     xax = tmpx
!!$     riodata = tmpr
!!$     model = tmpm

     riodata(1:currentProblemSize-idx) = pack(riodata(1:currentProblemSize),.NOT.olmask(1:currentProblemSize))

     oliers(1:currentProblemSize) = (/ (i,i=1,currentProblemSize ) /)

     modelmask(1:currentProblemSize-idx) = pack(oliers(1:currentProblemSize),.NOT.olmask(1:currentProblemSize))

     model(1:currentProblemSize-idx,:) = model(modelmask,:)


     currentProblemSize = currentProblemSize - idx

     !write(*,*) riodata(1:10)

  end do main_iteration

  call flips_kill(h)

  ! Write solution
  open(123,FILE="rio_sol.data",access='stream',form='unformatted',status='replace')
  write(123,pos=1) fit(1:currentProblemSize)
  close(123)



end program deletetest
