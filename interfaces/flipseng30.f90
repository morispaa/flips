program rlipseng
  ! file: rlipseng30.f90
  ! FLIPS Interface engine
  !   Copyright 2005-2009 University of Oulu, Finland. All rights reserved.
  !   Written by  Mikko Orispaa <mikko.orispaa@oulu.fi>
  !   
  !   Redistribution and use in source and binary forms, with or without modification, 
  !   are permitted provided that the following conditions are met:
  !   
  !      1. Redistributions of source code must retain the above copyright notice, this 
  !         list of conditions and the following disclaimer.
  !      2. Redistributions in binary form must reproduce the above copyright notice, this list 
  !         of conditions and the following disclaimer in the documentation and/or other materials 
  !         provided with the distribution.
  !   
  !   THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY OF OULU ``AS IS'' AND ANY EXPRESS OR IMPLIED 
  !   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
  !   FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF OULU 
  !   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
  !   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
  !   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
  !   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
  !   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
  !   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  !   
  !   The views and conclusions contained in the software and documentation are those of 
  !   the authors and should not be interpreted as representing official policies, either 
  !   expressed or implied, of University of Oulu.

  ! flipseng 3.0
  ! - support for reading data from the disk in parts,
  !   making it possible to solve larger problems

  ! flipseng 2.9
  ! - support for common variables

  ! flipseng 2.8
  ! - support for new bandwidth system

  ! flipseng2.7
  ! - buffesize can be set by user

  ! flipseng2.6
  ! - flips_delete for real problems
  ! - bug in the residual calculations fixed

  ! flipseng2.4
  ! - Modifications needed for the new rotation system

  ! flipseng2.3
  ! What's new:
  ! - File unit numbers changed. Does not seem to do any difference.

  ! flipseng2.2
  ! What's new:
  ! - Forced rotations

  use flips


  implicit none

  ! Namelist variables ----------
  integer :: mode,cplx,dbl,ncols,nrhs,crows,newrows,rexists,&
       calc_resid,calc_cov,full_cov,naddvar,ndelvar,bsize,common,band
  character(len=32) :: id
 
  real(sp) :: a
  real(dp) :: b
  integer :: c
  
  ! BW vars -----------
   character(len=40) :: bwfile
   logical :: bwexists
   integer :: cbw


  ! Global variables follow! Shame! Shame!! Shame!!!
  integer :: r_len, r_len_i

  ! -----------------------------

  ! FLIPS types -----------------
  type(flips_s) :: ss
  type(flips_c) :: cc
  type(flips_d) :: dd
  type(flips_z) :: zz
  ! -----------------------------



  namelist /params/ mode,cplx,dbl,id,ncols,nrhs,crows,newrows,naddvar,ndelvar,rexists,&
       calc_resid,calc_cov,full_cov,bsize,common,band

  ! Read namelist
  open(11,FILE="rflips.nml")
  read(11,NML=params)
  close(11)

  !write(*,*) bsize,ndelvar

  ! construct the bw file name
  write(bwfile,'("FLIPS_",A,"/cbw.dat")') trim(id)

  ! read the current bandwidth from the file. If file does not exist
  ! set bandwidth to 1.
  inquire(exist=bwexists,file=bwfile)
  if (bwexists) then
     open(123,file=bwfile,form='unformatted',access='stream')
     read(123,pos=1) cbw
     close(123)
  else 
     if (band==1) then
        cbw = 1
     else
        cbw = ncols
     end if
  end if


    

  ! ---------------------------------------
  ! Check RECL length
  if (dbl==0) then
     inquire(IOLENGTH=r_len) a
  else
     inquire(IOLENGTH=r_len) b
  end if

  inquire(iolength=r_len_i) c
  ! ---------------------------------------


  ! Choose action
  select case(mode)

  case(1)
     ! Solve
     if (cplx == 0) then

        if (dbl==0) then
           call f_solve_s(id,ncols,nrhs,crows,newrows,rexists,&
                calc_resid,calc_cov,full_cov,bsize,cbw,common)
        else
           call f_solve_d(id,ncols,nrhs,crows,newrows,rexists,&
                calc_resid,calc_cov,full_cov,bsize,cbw,common)
        end if
     else
        if (dbl==0) then
           call f_solve_c(id,ncols,nrhs,crows,newrows,rexists,&
                calc_resid,calc_cov,full_cov,bsize,cbw,common)
        else
           call f_solve_z(id,ncols,nrhs,crows,newrows,rexists,&
                calc_resid,calc_cov,full_cov,bsize,cbw,common)
        end if
     end if

  case(2)
     ! Resize
     if (cplx == 0) then

        if (dbl==0) then
           call f_resize_s(id,ncols,nrhs,crows,newrows,rexists,&
                naddvar,ndelvar,bsize,cbw,common)
        else
           call f_resize_d(id,ncols,nrhs,crows,newrows,rexists,&
                naddvar,ndelvar,bsize,cbw,common)
        end if
     else
        if (dbl==0) then
           call f_resize_c(id,ncols,nrhs,crows,newrows,rexists,&
                naddvar,ndelvar,bsize,cbw,common)
        else
           call f_resize_z(id,ncols,nrhs,crows,newrows,rexists,&
                naddvar,ndelvar,bsize,cbw,common)
        end if
     end if

     case(3)
        ! Rotate only
        if (cplx == 0) then
           
           if (dbl==0) then
              call f_rotate_s(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           else
              call f_rotate_d(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           end if
        else
           if (dbl==0) then
              call f_rotate_c(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           else
              call f_rotate_z(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           end if
        end if

     case(4)
        ! Delete
        if (cplx == 0) then
           
           if (dbl==0) then
              call f_delete_s(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           else
              call f_delete_d(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
           end if
        else
           !if (dbl==0) then
           !   call f_rotate_c(id,ncols,nrhs,crows,newrows,rexists)
           !else
           !   call f_rotate_z(id,ncols,nrhs,crows,newrows,rexists)
           !end if

           write(0,*) 'flipseng28 error: flips_delete is not implemented for complex problems!'
           stop

        end if


  case default
     write(*,*) 'This mode is not implemented!',mode
     stop

  end select

contains




  subroutine read_single(fname,n,data,pos,closefile)

    ! 091019
    ! Added optional argument pos
    ! Should convert all I/O into Stream!!

    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    real(sp), dimension(n), intent(out) :: data
    integer, intent(in), optional :: pos
    logical, optional, intent(in) :: closefile

    integer :: readpos
    logical :: cfile

    if (present(pos)) then
       readpos = pos
    else
       readpos = 1
    end if

    if (present(closefile)) then
       cfile = closefile
    else
       cfile = .TRUE.
    end if

    if (cfile) then
       open(12,file=fname,form='unformatted',access='direct',recl=r_len*n,action='read')
    end if

    read(12,rec=readpos) data

    if (cfile) then
       close(12)
    end if

  end subroutine read_single


  subroutine read_double(fname,n,data,pos,closefile)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    real(dp), dimension(n), intent(out) :: data
    integer, intent(in), optional :: pos
    logical, optional, intent(in) :: closefile

    integer :: readpos
    logical :: cfile

    if (present(pos)) then
       readpos = pos
    else
       readpos = 1
    end if

    if (present(closefile)) then
       cfile = closefile
    else
       cfile = .TRUE.
    end if

    open(12,file=fname,form='unformatted',access='direct',recl=r_len*n,action='read')

    read(12,rec=readpos) data

    if (cfile) then
       close(12)
    end if


  end subroutine read_double



  subroutine write_single(fname,n,data)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    real(sp), dimension(n), intent(in) :: data

    open(12,file=fname,form='unformatted',access='direct',status='replace',recl=r_len*n,action='write')

    write(12,rec=1) data

    close(12)

  end subroutine write_single



  subroutine write_double(fname,n,data)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    real(dp), dimension(n), intent(in) :: data

    open(12,file=fname,form='unformatted',access='direct',status='replace',recl=r_len*n,action='write')

    write(12,rec=1) data

    close(12)

  end subroutine write_double



  subroutine read_complex(fname,n,data,pos,closefile)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    complex(sp), dimension(n), intent(out) :: data
    real(sp), dimension(:), allocatable :: rdata
    integer, intent(in), optional :: pos
    logical, optional, intent(in) :: closefile

    integer :: readpos
    logical :: cfile

    if (present(pos)) then
       readpos = pos
    else
       readpos = 1
    end if

    if (present(closefile)) then
       cfile = closefile
    else
       cfile = .TRUE.
    end if


    allocate(rdata(2*n))


    open(12,file=fname,form='unformatted',access='direct',recl=(2*r_len*n),action='read')

    read(12,rec=readpos) rdata

    data = rdata(1:(2*n):2) + (0.0,1.0) * rdata(2:(2*n):2)


    if (cfile) then
       close(12)
    end if

    deallocate(rdata)

  end subroutine read_complex



  subroutine read_d_complex(fname,n,data,pos,closefile)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    complex(dp), dimension(n), intent(out) :: data
    real(dp), dimension(:), allocatable :: rdata
    integer, intent(in), optional :: pos
    logical, optional, intent(in) :: closefile

    integer :: readpos
    logical :: cfile

    if (present(pos)) then
       readpos = pos
    else
       readpos = 1
    end if

    if (present(closefile)) then
       cfile = closefile
    else
       cfile = .TRUE.
    end if

    allocate(rdata(2*n))


    open(12,file=fname,form='unformatted',access='direct',recl=(2*r_len*n),action='read')

    read(12,rec=readpos) rdata

    data = rdata(1:(2*n):2) + (0.0,1.0) * rdata(2:(2*n):2)

    !write(*,*) 'Rdata:',data

    if (cfile) then
       close(12)
    end if

    deallocate(rdata)

  end subroutine read_d_complex




  subroutine write_complex(fname,n,data)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    complex(sp), dimension(n), intent(in) :: data
    real(sp), dimension(:), allocatable :: rdata

    allocate(rdata(2*n))

    rdata(1:(2*n):2) = real(data)
    rdata(2:(2*n):2) = aimag(data)

    open(12,file=fname,form='unformatted',access='direct',status='replace',recl=(2*r_len*n),action='write')

    write(12,rec=1) rdata

    close(12)

    deallocate(rdata)

  end subroutine write_complex



  subroutine write_d_complex(fname,n,data)
    implicit none

    character(*), intent(in) :: fname
    integer, intent(in) :: n
    complex(dp), dimension(n), intent(in) :: data
    real(dp), dimension(:), allocatable :: rdata

    allocate(rdata(2*n))

    rdata(1:(2*n):2) = real(data)
    rdata(2:(2*n):2) = aimag(data)

    open(12,file=fname,form='unformatted',access='direct',status='replace',recl=(2*r_len*n),action='write')

    write(12,rec=1) rdata
!write(*,*) 'Wsol:',rdata

    close(12)

    deallocate(rdata)

  end subroutine write_d_complex


  subroutine write_bw(id,curbw)
    implicit none
    
    character(len=32) :: id
    integer :: curbw

    character(len=40) :: fn
    
     write(fn,'("FLIPS_",A,"/cbw.dat")') trim(id)

     open(123,file=fn,form='unformatted',access='stream',status='replace')
     write(123,pos=1) curbw
     close(123)

   end subroutine write_bw





  subroutine f_delete_s(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    real(sp), dimension(:), allocatable :: Adata
    real(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata
    real(sp), dimension(ncols) :: Rrow
    integer :: i

    !real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(ss,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then

       call read_single(rfile,size(ss%rmat),ss%rmat)
       call read_single(yfile,ncols*nrhs,ss%ymat)

       if (common > 0 ) then
          call read_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if

       !Read also tempres from *_resid.dat
       call read_single(tresidfile,nrhs,ss%tmpres)

       if (crows > ncols) then
          ss%nrows = ncols
       else
          ss%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,ss%nrows
          Rrow = 0.0
          Rrow(i:min(i+ss%bw-1,ncols-common)) = ss%rmat(rind(i,i,ncols-common,ss%bw):rind(i,min(i+ss%bw-1,ncols-common),ncols-common,ss%bw))

          call find_first_and_len_s(ncols-common,1,ss%zeroth,Rrow(1:ncols-common),ss%rst(i),ss%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common,ncols
             ss%rst(i) = i
             ss%rle(i) = ncols
          end do
       end if

    end if

    ! If there are unread/unrotated data rows read and antirotate them
    if (newrows > 0) then

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_single(afile,newrows*ncols,Adata)
       call read_single(mfile,newrows*nrhs,Mdata)
       call read_single(efile,newrows,Edata)
       
       ! Antirotate 
       call flipsdelete_s(ss,newrows,Adata,Mdata,Edata)
       
       deallocate(Adata,Mdata,Edata)

       !where(abs(ss%rmat) < ss%zeroth)
       !   ss%rmat = 0.0
       !end where



       ! Write R, Y and tmpres back to disk.
       call write_single(rfile,size(ss%rmat),ss%rmat)
       call write_single(yfile,ncols*nrhs,ss%ymat)

       if (common > 0 ) then
          call write_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if

       
       ! Write also tmpres into *_resid.dat
       !do i = 1,newrows
          !ss%tmpres = ss%tmpres - Mdata(yind(i,1,nrhs):yind(i,nrhs,nrhs))**2
       !end do
       call write_single(tresidfile,nrhs,ss%tmpres)

       ! Write bandwidth
       call write_bw(id,ss%bw)
   

    end if

    call flips_kill(ss)



  end subroutine f_delete_s




  subroutine f_delete_d(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    real(dp), dimension(:), allocatable :: Adata
    real(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata
    real(dp), dimension(ncols) :: Rrow
    integer :: i

    !real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(dd,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    !dd%rmat = 0.0_dp
    !dd%ymat = 0.0_dp

    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_double(rfile,size(dd%rmat),dd%rmat)
       call read_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0 ) then
          call read_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if


       !Read also tempres from *_resid.dat
       call read_double(tresidfile,nrhs,dd%tmpres)

       ! Debug 080515
!       write(*,*) 'max R before:', maxval(dd%rmat)

!       write(*,*) dd%rmat

       if (crows > ncols) then
          dd%nrows = ncols
       else
          dd%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,dd%nrows
          Rrow = 0.0
          Rrow(i:min(i+dd%bw-1,ncols-common)) = dd%rmat(rind(i,i,ncols-common,dd%bw):rind(i,min(i+dd%bw-1,ncols-common),ncols-common,dd%bw))
          call find_first_and_len_d(ncols-common,1,dd%zeroth,Rrow(1:ncols-common),dd%rst(i),dd%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common,ncols
             dd%rst(i) = i
             dd%rle(i) = ncols
          end do
       end if

    end if

    ! If there are unread/unrotated data rows read and antirotate them
    if (newrows > 0) then

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

      ! Adata = 0.0_dp
      ! Mdata = 0.0_dp
      ! Edata = 0.0_dp

       ! Read data
       call read_double(afile,newrows*ncols,Adata)
       call read_double(mfile,newrows*nrhs,Mdata)
       call read_double(efile,newrows,Edata)



       
       ! Antirotate 
       call flipsdelete_d(dd,newrows,Adata,Mdata,Edata)
       


       deallocate(Adata,Mdata,Edata)


       !where(abs(dd%rmat) < dd%zeroth)
       !   dd%rmat = 0.0
       !end where


       ! Write R, Y and tmpres back to disk.
       call write_double(rfile,size(dd%rmat),dd%rmat)
       call write_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0 ) then
          call write_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if
       
       ! Write also tmpres into *_resid.dat
       !do i = 1,newrows
          !ss%tmpres = ss%tmpres - Mdata(yind(i,1,nrhs):yind(i,nrhs,nrhs))**2
       !end do
       call write_double(tresidfile,nrhs,dd%tmpres)

       ! Write bandwidth
       call write_bw(id,dd%bw)
       
       call flips_kill(dd)

    end if



  end subroutine f_delete_d





  subroutine f_rotate_s(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    real(sp), dimension(:), allocatable :: Adata
    real(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata
    real(sp), dimension(ncols) :: Rrow
    integer :: i,n_of_full_loops, n_of_extra_rows

    !real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(ss,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_single(rfile,size(ss%rmat),ss%rmat)
       call read_single(yfile,ncols*nrhs,ss%ymat)

       if (common > 0 ) then
          call read_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if

 

       !Read also tempres from *_resid.dat
       call read_single(tresidfile,nrhs,ss%tmpres)

       
       if (crows > ncols) then
          ss%nrows = ncols
       else
          ss%nrows = crows
       end if


       ! Find the R row lengths
       do i = 1,ss%nrows
          Rrow = 0.0
          Rrow(i:min(i+ss%bw-1,ncols-common)) = ss%rmat(rind(i,i,ncols-common,ss%bw):rind(i,min(i+ss%bw-1,ncols-common),ncols-common,ss%bw))
          ! Get the whole R row
          !call get_row('rmat',ss,ncols-i+1,Rrow(i:ncols),i)
          ! Find 1st and last elements of the regular part
          call find_first_and_len_s(ncols-common,1,ss%zeroth,Rrow(1:ncols-common),ss%rst(i),ss%rle(i))
       end do

       ! If common variables exist, write the 1st and last elements manually
       if ( common > 0 ) then
          do i = ncols-common+1,ncols
             ss%rle(i) = 0
             ss%rst(i) = i
          end do
       end if
       

    end if


    ! If there are unread/unrotated data rows read and rotate them
    if (newrows > 0) then
       if (newrows > bsize) then
          n_of_full_loops = newrows/bsize
          n_of_extra_rows = mod(newrows,bsize)

          allocate(Adata(bsize*ncols),Mdata(bsize*nrhs),Edata(bsize))

          !open(12,file=afile,form='unformatted',access='direct',recl=r_len*bsize*ncols,action='read')
          !open(13,file=mfile,form='unformatted',access='direct',recl=r_len*bsize*nrhs,action='read')
          !open(14,file=efile,form='unformatted',access='direct',recl=r_len*bsize,action='read')

          open(12,file=afile,form='unformatted',access='stream',action='read')
          open(13,file=mfile,form='unformatted',access='stream',action='read')
          open(14,file=efile,form='unformatted',access='stream',action='read')

          do i = 1,n_of_full_loops

             !write(*,*) 'Loop',i,'/',n_of_full_loops

             read(12,pos=1+(i-1)*bsize*ncols*r_len) Adata
             read(13,pos=1+(i-1)*bsize*nrhs*r_len) Mdata
             read(14,pos=1+(i-1)*bsize*r_len) Edata

 

             call flips_add(ss,bsize,Adata,Mdata,Edata,.TRUE.)  

          end do

          close(12)
          close(13)
          close(14)

          deallocate(Adata,Mdata,Edata)

          if ( n_of_extra_rows > 0) then

             !write(*,*) 'Extra rows!'
             
             allocate(Adata(n_of_extra_rows*ncols),Mdata(n_of_extra_rows*nrhs),Edata(n_of_extra_rows))
             
             open(12,file=afile,form='unformatted',access='stream',action='read')
             open(13,file=mfile,form='unformatted',access='stream',action='read')
             open(14,file=efile,form='unformatted',access='stream',action='read')
             
             read(12,pos=n_of_full_loops*bsize*ncols*r_len + 1) Adata
             read(13,pos=n_of_full_loops*bsize*nrhs*r_len + 1) Mdata
             read(14,pos=n_of_full_loops*bsize*r_len + 1) Edata
                
             call flips_add(ss,n_of_extra_rows,Adata,Mdata,Edata,.TRUE.)  
                
             close(12)
             close(13)
             close(14)

             deallocate(Adata,Mdata,Edata)
             
          end if

       else

          allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

          ! Read data
          call read_single(afile,newrows*ncols,Adata)
          call read_single(mfile,newrows*nrhs,Mdata)
          call read_single(efile,newrows,Edata)
          
          ! Feed FLIPS
          call flips_add(ss,newrows,Adata,Mdata,Edata,.TRUE.)
       
          deallocate(Adata,Mdata,Edata)

       end if

       ! Write R, Y and tmpres back to disk.
       call write_single(rfile,size(ss%rmat),ss%rmat)
       call write_single(yfile,ncols*nrhs,ss%ymat)

       ! if common variables, write them also
       if (common > 0 ) then
          call write_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if

       ! Write also tmpres into *_resid.dat
       call write_single(tresidfile,nrhs,ss%tmpres)

       ! Write bandwidth
       call write_bw(id,ss%bw)
       
       call flips_kill(ss)

    end if


  end subroutine f_rotate_s



  subroutine f_rotate_d(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    real(dp), dimension(:), allocatable :: Adata
    real(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata
    real(dp), dimension(ncols) :: Rrow
    !real(sp), dimension(:,:), allocatable :: covmat

    integer :: i,n_of_full_loops,n_of_extra_rows

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(dd,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_double(rfile,size(dd%rmat),dd%rmat)
       call read_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0) then
          call read_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if

       !Read also tempres from *_resid.dat
       call read_double(tresidfile,nrhs,dd%tmpres)
       
       if (crows > ncols) then
          dd%nrows = ncols
       else
          dd%nrows = crows
       end if


       ! Find the R row lengths
       do i = 1,dd%nrows
          Rrow = 0.0
          Rrow(i:min(i+dd%bw-1,ncols-common)) = dd%rmat(rind(i,i,ncols-common,dd%bw):rind(i,min(i+dd%bw-1,ncols-common),ncols-common,dd%bw))
          call find_first_and_len_d(ncols-common,1,dd%zeroth,Rrow(1:ncols-common),dd%rst(i),dd%rle(i))
       end do
    end if

    if (common > 0 ) then
       do i = ncols-common+1,ncols
          dd%rle(i) = 0
          dd%rst(i) = i
       end do
    end if


!    ! If there are unread/unrotated data rows read and rotate them
!    if (newrows > 0) then
!
!       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))
!
!       ! Read data
!       call read_double(afile,newrows*ncols,Adata)
!       call read_double(mfile,newrows*nrhs,Mdata)
!       call read_double(efile,newrows,Edata)
!       
!       ! Feed FLIPS
!       call flips_add(dd,newrows,Adata,Mdata,Edata,.TRUE.)
!       
!       deallocate(Adata,Mdata,Edata)





! If there are unread/unrotated data rows read and rotate them
    if (newrows > 0) then
       if (newrows > bsize) then
          n_of_full_loops = newrows/bsize
          n_of_extra_rows = mod(newrows,bsize)

          allocate(Adata(bsize*ncols),Mdata(bsize*nrhs),Edata(bsize))

          !open(12,file=afile,form='unformatted',access='direct',recl=r_len*bsize*ncols,action='read')
          !open(13,file=mfile,form='unformatted',access='direct',recl=r_len*bsize*nrhs,action='read')
          !open(14,file=efile,form='unformatted',access='direct',recl=r_len*bsize,action='read')

          open(12,file=afile,form='unformatted',access='stream',action='read')
          open(13,file=mfile,form='unformatted',access='stream',action='read')
          open(14,file=efile,form='unformatted',access='stream',action='read')

          do i = 1,n_of_full_loops

             !write(*,*) 'Loop',i,'/',n_of_full_loops

             read(12,pos=1+(i-1)*bsize*ncols*r_len) Adata
             read(13,pos=1+(i-1)*bsize*nrhs*r_len) Mdata
             read(14,pos=1+(i-1)*bsize*r_len) Edata

 

             call flips_add(dd,bsize,Adata,Mdata,Edata,.TRUE.)  

          end do

          close(12)
          close(13)
          close(14)

          deallocate(Adata,Mdata,Edata)

          if ( n_of_extra_rows > 0) then

             !write(*,*) 'Extra rows!'
             
             allocate(Adata(n_of_extra_rows*ncols),Mdata(n_of_extra_rows*nrhs),Edata(n_of_extra_rows))
             
             open(12,file=afile,form='unformatted',access='stream',action='read')
             open(13,file=mfile,form='unformatted',access='stream',action='read')
             open(14,file=efile,form='unformatted',access='stream',action='read')
             
             read(12,pos=n_of_full_loops*bsize*ncols*r_len + 1) Adata
             read(13,pos=n_of_full_loops*bsize*nrhs*r_len + 1) Mdata
             read(14,pos=n_of_full_loops*bsize*r_len + 1) Edata
                
             call flips_add(dd,n_of_extra_rows,Adata,Mdata,Edata,.TRUE.)  
                
             close(12)
             close(13)
             close(14)

             deallocate(Adata,Mdata,Edata)
             
          end if

       else

          allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

          ! Read data
          call read_double(afile,newrows*ncols,Adata)
          call read_double(mfile,newrows*nrhs,Mdata)
          call read_double(efile,newrows,Edata)
          
          ! Feed FLIPS
          call flips_add(dd,newrows,Adata,Mdata,Edata,.TRUE.)
       
          deallocate(Adata,Mdata,Edata)

       end if







       ! Write R, Y and tmpres back to disk.
       call write_double(rfile,size(dd%rmat),dd%rmat)
       call write_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0 ) then
          call write_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if

       ! Write also tmpres into *_resid.dat
       call write_double(tresidfile,nrhs,dd%tmpres)
       
       ! Write bandwidth
       call write_bw(id,dd%bw)

       call flips_kill(dd)

    end if


  end subroutine f_rotate_d



  subroutine f_rotate_c(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    complex(sp), dimension(:), allocatable :: Adata
    complex(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata,rdataE,rdataA,rdataM
    complex, dimension(ncols) :: Rrow

    integer :: i,nn,n_of_full_loops,n_of_extra_rows

    !real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(cc,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_complex(rfile,size(cc%rmat),cc%rmat)
       call read_complex(yfile,ncols*nrhs,cc%ymat)

       if (common > 0 ) then
          call read_complex(cvfile,size(cc%cvmat),cc%cvmat)
       end if

       !Read also tempres from *_resid.dat
       call read_single(tresidfile,nrhs,cc%tmpres)
       
       if (crows > ncols) then
          cc%nrows = ncols
       else
          cc%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,cc%nrows
          Rrow = 0.0
          Rrow(i:min(i+cc%bw-1,ncols-common)) = cc%rmat(rind(i,i,ncols-common,cc%bw):rind(i,min(i+cc%bw-1,ncols-common),ncols-common,cc%bw))
          call find_first_and_len_c(ncols-common,1,cc%zeroth,Rrow(1:ncols-common),cc%rst(i),cc%rle(i))
       end do
    end if

    if (common > 0) then
       do i = ncols-common+1,ncols
          cc%rst(i) = i
          cc%rle = 0
       end do
    end if

!    ! If there are unread/unrotated data rows read and rotate them
!    if (newrows > 0) then!
!
!       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))
!
!       ! Read data
!       call read_complex(afile,newrows*ncols,Adata)
!       call read_complex(mfile,newrows*nrhs,Mdata)
!       call read_single(efile,newrows,Edata)
!       
!       ! Feed FLIPS
!       call flips_add(cc,newrows,Adata,Mdata,Edata,.TRUE.)
!       
!       deallocate(Adata,Mdata,Edata)






! If there are unread/unrotated data rows read and rotate them
    if (newrows > 0) then
       if (newrows > bsize) then
          n_of_full_loops = newrows/bsize
          n_of_extra_rows = mod(newrows,bsize)

          allocate(Adata(bsize*ncols),Mdata(bsize*nrhs),Edata(bsize),&
                rdataA(bsize*ncols*2),rdataM(bsize*nrhs*2))

          !write(*,*) 'bsize ',bsize
          !write(*,*) 'nrhs ',nrhs
          !write(*,*) 'bsize ',bsize


          !open(12,file=afile,form='unformatted',access='direct',recl=r_len*bsize*ncols,action='read')
          !open(13,file=mfile,form='unformatted',access='direct',recl=r_len*bsize*nrhs,action='read')
          !open(14,file=efile,form='unformatted',access='direct',recl=r_len*bsize,action='read')

          open(12,file=afile,form='unformatted',access='stream',action='read')
          open(13,file=mfile,form='unformatted',access='stream',action='read')
          open(14,file=efile,form='unformatted',access='stream',action='read')

          do i = 1,n_of_full_loops

             !write(*,*) 'Loop',i,'/',n_of_full_loops

             read(12,pos=1+(i-1)*bsize*ncols*r_len*2) rdataA
             read(13,pos=1+(i-1)*bsize*nrhs*r_len*2) rdataM
             read(14,pos=1+(i-1)*bsize*r_len) Edata

             !read(12) rdataA
             !read(13) rdataM
             !read(14) Edata

             nn = bsize*ncols
             Adata = rdataA(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             nn = bsize*nrhs
             Mdata = rdataM(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             !nn = bsize
             !Edata = rdataE(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)

             call flips_add(cc,bsize,Adata,Mdata,Edata,.TRUE.)  

          end do

          close(12)
          close(13)
          close(14)

          deallocate(Adata,Mdata,Edata,rdataA,rdataM)

          if ( n_of_extra_rows > 0) then

             !write(*,*) 'Extra rows!'
             
             allocate(Adata(n_of_extra_rows*ncols),Mdata(n_of_extra_rows*nrhs),Edata(n_of_extra_rows),&
                  rdataA(n_of_extra_rows*ncols*2),rdataM(n_of_extra_rows*nrhs*2))
             
             open(12,file=afile,form='unformatted',access='stream',action='read')
             open(13,file=mfile,form='unformatted',access='stream',action='read')
             open(14,file=efile,form='unformatted',access='stream',action='read')
             
             read(12,pos=n_of_full_loops*bsize*ncols*r_len*2 + 1) rdataA
             read(13,pos=n_of_full_loops*bsize*nrhs*r_len*2 + 1) rdataM
             read(14,pos=n_of_full_loops*bsize*r_len + 1) Edata

             nn = n_of_extra_rows*ncols
             Adata = rdataA(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             nn = n_of_extra_rows*nrhs
             Mdata = rdataM(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             !nn = n_of_extra_rows
             !Edata = rdataE(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)

                
             call flips_add(cc,n_of_extra_rows,Adata,Mdata,Edata,.TRUE.)  
                
             close(12)
             close(13)
             close(14)

             deallocate(Adata,Mdata,Edata,rdataA,rdataM)
             
          end if

       else

          allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

          ! Read data
          call read_complex(afile,newrows*ncols,Adata)
          call read_complex(mfile,newrows*nrhs,Mdata)
          call read_single(efile,newrows,Edata)
          
          ! Feed FLIPS
          call flips_add(cc,newrows,Adata,Mdata,Edata,.TRUE.)
       
          deallocate(Adata,Mdata,Edata)

       end if






       ! Write R, Y and tmpres back to disk.
       call write_complex(rfile,size(cc%rmat),cc%rmat)
       call write_complex(yfile,ncols*nrhs,cc%ymat)

       if (common > 0 ) then
          call write_complex(cvfile,size(cc%cvmat),cc%cvmat)
       end if

       ! Write also tmpres into *_resid.dat
       call write_single(tresidfile,nrhs,cc%tmpres)

       ! Write bandwidth
       call write_bw(id,cc%bw)
       
       call flips_kill(cc)

    end if


  end subroutine f_rotate_c



 subroutine f_rotate_z(id,ncols,nrhs,crows,newrows,rexists,bsize,cbw,common)
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,bsize,cbw,common
    
    complex(dp), dimension(:), allocatable :: Adata
    complex(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata,rdataA,rdataM

    complex(dp), dimension(ncols) :: Rrow
    integer :: i,n_of_full_loops,n_of_extra_rows,nn

    !real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile
    
    ! Initialize FLIPS
    call flips_init(zz,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_d_complex(rfile,size(zz%rmat),zz%rmat)
       call read_d_complex(yfile,ncols*nrhs,zz%ymat)

       if (common > 0 ) then
          call read_d_complex(cvfile,size(zz%cvmat),zz%cvmat)
       end if

       !Read also tempres from *_resid.dat
       call read_double(tresidfile,nrhs,zz%tmpres)
       
       if (crows > ncols) then
          zz%nrows = ncols
       else
          zz%nrows = crows
       end if


       ! Find the R row lengths
       do i = 1,zz%nrows
          Rrow = 0.0
          Rrow(i:min(i+zz%bw-1,ncols-common)) = zz%rmat(rind(i,i,ncols-common,zz%bw):rind(i,min(i+zz%bw-1,ncols-common),ncols-common,zz%bw))
          call find_first_and_len_z(ncols-common,1,zz%zeroth,Rrow(1:ncols-common),zz%rst(i),zz%rle(i))
       end do
    end if

    if (common > 0) then
       do i = ncols-common+1,ncols
          zz%rst(i) = i
          zz%rle(i) = 0
       end do
    end if

!    ! If there are unread/unrotated data rows read and rotate them
!    if (newrows > 0) then
!
!       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))
!
!       ! Read data
!       call read_d_complex(afile,newrows*ncols,Adata)
!       call read_d_complex(mfile,newrows*nrhs,Mdata)
!       call read_double(efile,newrows,Edata)
!       
!       ! Feed FLIPS
!       call flips_add(zz,newrows,Adata,Mdata,Edata,.TRUE.)
!       
!       deallocate(Adata,Mdata,Edata)






! If there are unread/unrotated data rows read and rotate them
    if (newrows > 0) then
       if (newrows > bsize) then
          n_of_full_loops = newrows/bsize
          n_of_extra_rows = mod(newrows,bsize)

          allocate(Adata(bsize*ncols),Mdata(bsize*nrhs),Edata(bsize),&
                rdataA(bsize*ncols*2),rdataM(bsize*nrhs*2))

          !write(*,*) 'bsize ',bsize
          !write(*,*) 'nrhs ',nrhs
          !write(*,*) 'bsize ',bsize


          !open(12,file=afile,form='unformatted',access='direct',recl=r_len*bsize*ncols,action='read')
          !open(13,file=mfile,form='unformatted',access='direct',recl=r_len*bsize*nrhs,action='read')
          !open(14,file=efile,form='unformatted',access='direct',recl=r_len*bsize,action='read')

          open(12,file=afile,form='unformatted',access='stream',action='read')
          open(13,file=mfile,form='unformatted',access='stream',action='read')
          open(14,file=efile,form='unformatted',access='stream',action='read')

          do i = 1,n_of_full_loops

             !write(*,*) 'Loop',i,'/',n_of_full_loops

             read(12,pos=1+(i-1)*bsize*ncols*r_len*2) rdataA
             read(13,pos=1+(i-1)*bsize*nrhs*r_len*2) rdataM
             read(14,pos=1+(i-1)*bsize*r_len) Edata

             !read(12) rdataA
             !read(13) rdataM
             !read(14) Edata

             nn = bsize*ncols
             Adata = rdataA(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             nn = bsize*nrhs
             Mdata = rdataM(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             !nn = bsize
             !Edata = rdataE(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)

             call flips_add(zz,bsize,Adata,Mdata,Edata,.TRUE.)  

          end do

          close(12)
          close(13)
          close(14)

          deallocate(Adata,Mdata,Edata,rdataA,rdataM)

          if ( n_of_extra_rows > 0) then

             !write(*,*) 'Extra rows!'
             
             allocate(Adata(n_of_extra_rows*ncols),Mdata(n_of_extra_rows*nrhs),Edata(n_of_extra_rows),&
                  rdataA(n_of_extra_rows*ncols*2),rdataM(n_of_extra_rows*nrhs*2))
             
             open(12,file=afile,form='unformatted',access='stream',action='read')
             open(13,file=mfile,form='unformatted',access='stream',action='read')
             open(14,file=efile,form='unformatted',access='stream',action='read')
             
             read(12,pos=n_of_full_loops*bsize*ncols*r_len*2 + 1) rdataA
             read(13,pos=n_of_full_loops*bsize*nrhs*r_len*2 + 1) rdataM
             read(14,pos=n_of_full_loops*bsize*r_len + 1) Edata

             nn = n_of_extra_rows*ncols
             Adata = rdataA(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             nn = n_of_extra_rows*nrhs
             Mdata = rdataM(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)
             !nn = n_of_extra_rows
             !Edata = rdataE(1:(2*nn):2) + (0.0,1.0) * rdataA(2:(2*nn):2)

                
             call flips_add(zz,n_of_extra_rows,Adata,Mdata,Edata,.TRUE.)  
                
             close(12)
             close(13)
             close(14)

             deallocate(Adata,Mdata,Edata,rdataA,rdataM)
             
          end if

       else

          allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

          ! Read data
          call read_d_complex(afile,newrows*ncols,Adata)
          call read_d_complex(mfile,newrows*nrhs,Mdata)
          call read_double(efile,newrows,Edata)
          
          ! Feed FLIPS
          call flips_add(zz,newrows,Adata,Mdata,Edata,.TRUE.)
       
          deallocate(Adata,Mdata,Edata)

       end if







       ! Write R, Y and tmpres back to disk.
       call write_d_complex(rfile,size(zz%rmat),zz%rmat)
       call write_d_complex(yfile,ncols*nrhs,zz%ymat)

       if (common > 0 ) then
          call write_d_complex(cvfile,size(zz%cvmat),zz%cvmat)
       end if

       ! Write also tmpres into *_resid.dat
       call write_double(tresidfile,nrhs,zz%tmpres)

       ! Write bandwidth
       call write_bw(id,zz%bw)
       
       call flips_kill(zz)

    end if


  end subroutine f_rotate_z




  subroutine f_solve_s(id,ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common)
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common

    real(sp), dimension(:), allocatable :: Adata
    real(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata

    real(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile

    real(sp), dimension(ncols) :: Rrow
    integer :: i
    
    ! Initialize FLIPS
    call flips_init(ss,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    !write(yfile,'("FLIPS_",I6.6,"/Y.dat")') id
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_single(rfile,size(ss%rmat),ss%rmat)
       call read_single(yfile,ncols*nrhs,ss%ymat)
       !Read also tempres from *_resid.dat
       call read_single(tresidfile,nrhs,ss%tmpres)

       if (common > 0 ) then
          call read_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if
       
       if (crows > ncols) then
          ss%nrows = ncols
       else
          ss%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,ss%nrows
          Rrow = 0.0
          Rrow(i:min(i+ss%bw-1,ncols-common)) = ss%rmat(rind(i,i,ncols-common,ss%bw):rind(i,min(i+ss%bw-1,ncols-common),ncols-common,ss%bw))
          call find_first_and_len_s(ncols-common,1,ss%zeroth,Rrow(1:ncols-common),ss%rst(i),ss%rle(i))
       end do

       ! If common variables exist, write the 1st and last elements manually
       if ( common > 0 ) then
          do i = ncols-common+1,ncols
             ss%rle(i) = 0
             ss%rst(i) = i
          end do
       end if

    end if

    ! If there are unread/unrotated data rows read and rotate them
    if (newrows > 0) then

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_single(afile,newrows*ncols,Adata)
       call read_single(mfile,newrows*nrhs,Mdata)
       call read_single(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(ss,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)

    end if

    ! Solve and write results
    if (calc_resid==1) then
       call flips_solve(ss,.TRUE.)
       call write_single(sfile,ncols*nrhs,ss%solmat)
       call write_single(residfile,nrhs,ss%residual)
    else
       call flips_solve(ss,.FALSE.)
       call write_single(sfile,ncols*nrhs,ss%solmat)
    end if

    ! Calculate covariance and write results
    if (calc_cov == 1) then
       
       if (full_cov == 1) then
          allocate(covmat(ncols,ncols))
          call flips_calc_cov(ss,.TRUE.)
          call flips_get('cova',ss,covmat)
          call write_single(covfile,ncols*ncols,covmat)
          deallocate(covmat)
       else
          call flips_calc_cov(ss,.FALSE.)
          call write_single(covfile,ncols,ss%cmat)
       end if
    end if


    ! Write R and Y
    call write_single(rfile,size(ss%rmat),ss%rmat)
    call write_single(yfile,ncols*nrhs,ss%ymat)

    ! if common variables, write them also
    if (common > 0 ) then
       call write_single(cvfile,size(ss%cvmat),ss%cvmat)
    end if


    ! Write also tmpres into *_resid.dat
    call write_single(tresidfile,nrhs,ss%tmpres)
    
    ! Write bandwidth
    call write_bw(id,ss%bw)


    call flips_kill(ss)

  end subroutine f_solve_s





  subroutine f_solve_d(id,ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common)
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common

    real(dp), dimension(:), allocatable :: Adata
    real(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata

    real(dp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile

    real(dp), dimension(ncols) :: Rrow
    integer :: i
 
  
    ! Initialize FLIPS
    call flips_init(dd,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then
       call read_double(rfile,size(dd%rmat),dd%rmat)
       call read_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0 ) then
          call read_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if

       call read_double(tresidfile,nrhs,dd%tmpres)
       if (crows > ncols) then
          dd%nrows = ncols
       else
          dd%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,dd%nrows
          Rrow = 0.0
          Rrow(i:min(i+dd%bw-1,ncols-common)) = dd%rmat(rind(i,i,ncols-common,dd%bw):rind(i,min(i+dd%bw-1,ncols-common),ncols-common,dd%bw))
          call find_first_and_len_d(ncols-common,1,dd%zeroth,Rrow(1:ncols-common),dd%rst(i),dd%rle(i))
       end do

    end if

    if (common > 0 ) then
       do i = ncols-common+1,ncols
          dd%rst(i) = i
          dd%rle(i) = 0
       end do
    end if


    if (newrows > 0) then
       ! Read data
       call read_double(afile,newrows*ncols,Adata)
       call read_double(mfile,newrows*nrhs,Mdata)
       call read_double(efile,newrows,Edata)


       ! Feed FLIPS
       call flips_add(dd,newrows,Adata,Mdata,Edata,.TRUE.)

       deallocate(Adata,Mdata,Edata)
    end if

    ! Solve and write results
    if (calc_resid == 1) then
       call flips_solve(dd,.TRUE.)
       call write_double(sfile,ncols*nrhs,dd%solmat)
       call write_double(residfile,nrhs,dd%residual)
    else
       call flips_solve(dd,.FALSE.)
       call write_double(sfile,ncols*nrhs,dd%solmat)
    end if


    ! Calculate covariance and write results
    if (calc_cov == 1) then
       
       if (full_cov == 1) then
          allocate(covmat(ncols,ncols))
          call flips_calc_cov(dd,.TRUE.)
          call flips_get('cova',dd,covmat)
          call write_double(covfile,ncols*ncols,covmat)
          deallocate(covmat)
       else
          call flips_calc_cov(dd,.FALSE.)
          call write_double(covfile,ncols,dd%cmat)
       end if
    end if

    ! Write R and Y
    call write_double(rfile,size(dd%rmat),dd%rmat)
    call write_double(yfile,ncols*nrhs,dd%ymat)

    if (common > 0 ) then
       call write_double(cvfile,size(dd%cvmat),dd%cvmat)
    end if

    call write_double(tresidfile,nrhs,dd%tmpres)
    
    ! Write bandwidth
    call write_bw(id,dd%bw)

    call flips_kill(dd)

  end subroutine f_solve_d




  subroutine f_solve_c(id,ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common)
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common

    complex(sp), dimension(:), allocatable :: Adata
    complex(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata

    complex(sp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile, tresidfile,cvfile

    complex(sp), dimension(ncols) :: Rrow
    integer :: i
    
    allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

    ! Initialize FLIPS
    call flips_init(cc,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)
    
    ! If R exists, load it and put it into place
    if (rexists == 1) then

       call read_complex(rfile,size(cc%rmat),cc%rmat)
       call read_complex(yfile,ncols*nrhs,cc%ymat)

       if (common > 0 ) then
          call read_complex(cvfile,size(cc%cvmat),cc%cvmat)
       end if

       call read_single(tresidfile,nrhs,cc%tmpres)

       if (crows > ncols) then
          cc%nrows = ncols
       else
          cc%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,cc%nrows
          Rrow = 0.0
          Rrow(i:min(i+cc%bw-1,ncols-common)) = cc%rmat(rind(i,i,ncols-common,cc%bw):rind(i,min(i+cc%bw-1,ncols-common),ncols-common,cc%bw))
          call find_first_and_len_c(ncols-common,1,cc%zeroth,Rrow(1:ncols-common),cc%rst(i),cc%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common+1,ncols
             cc%rst(i) = i
             cc%rle(i) = 0
          end do
       end if

    end if

    if (newrows >0) then

       ! Read data
       call read_complex(afile,newrows*ncols,Adata)
       call read_complex(mfile,newrows*nrhs,Mdata)
       call read_single(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(cc,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)
    end if

    ! Solve and write results
    if (calc_resid==1) then
       call flips_solve(cc,.TRUE.)
       call write_complex(sfile,ncols*nrhs,cc%solmat)
       call write_single(residfile,nrhs,cc%residual)
    else
       call flips_solve(cc,.FALSE.)
       call write_complex(sfile,ncols*nrhs,cc%solmat)
    end if

    ! Calculate covariance and write results
    if (calc_cov == 1) then
       
       if (full_cov == 1) then
          allocate(covmat(ncols,ncols))
          call flips_calc_cov(cc,.TRUE.)
          call flips_get('cova',cc,covmat)
          call write_complex(covfile,ncols*ncols,covmat)
          deallocate(covmat)
       else
          call flips_calc_cov(cc,.FALSE.)
          call write_complex(covfile,ncols,cc%cmat)
       end if
    end if

    ! Write R and Y
    call write_complex(rfile,size(cc%rmat),cc%rmat)
    call write_complex(yfile,ncols*nrhs,cc%ymat)

    if (common > 0 ) then
       call write_complex(cvfile,size(cc%cvmat),cc%cvmat)
    end if

    call write_single(tresidfile,nrhs,cc%tmpres)
    
    ! Write bandwidth
    call write_bw(id,cc%bw)

    call flips_kill(cc)

  end subroutine f_solve_c




  subroutine f_solve_z(id,ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common)
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,calc_resid,&
       calc_cov,full_cov,bsize,cbw,common

    complex(dp), dimension(:), allocatable :: Adata
    complex(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata

    complex(dp), dimension(:,:), allocatable :: covmat

    character(len=40) :: afile,mfile,efile,sfile,rfile,&
         covfile,residfile,irfile,yfile,tresidfile,cvfile

    complex(dp), dimension(ncols) :: Rrow
    integer :: i
    
    allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

    ! Initialize FLIPS
    call flips_init(zz,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)

    ! If R exists, load it and put it into place
    if (rexists == 1) then

       call read_d_complex(rfile,size(zz%rmat),zz%rmat)
       call read_d_complex(yfile,ncols*nrhs,zz%ymat)

       if (common > 0) then
          call read_d_complex(cvfile,size(zz%cvmat),zz%cvmat)
       end if

       call read_double(tresidfile,nrhs,zz%tmpres)

       if (crows > ncols) then
          zz%nrows = ncols
       else
          zz%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,zz%nrows
          Rrow = 0.0
          Rrow(i:min(i+zz%bw-1,ncols-common)) = zz%rmat(rind(i,i,ncols-common,zz%bw):rind(i,min(i+zz%bw-1,ncols-common),ncols-common,zz%bw))
          call find_first_and_len_z(ncols-common,1,zz%zeroth,Rrow(1:ncols-common),zz%rst(i),zz%rle(i))
       end do

       if (common > 0) then
          do i = ncols-common+1,ncols
             zz%rst(i) = i
             zz%rle(i) = 0
          end do
       end if


    end if


    if (newrows >0) then
       ! Read data
       call read_d_complex(afile,newrows*ncols,Adata)
       call read_d_complex(mfile,newrows*nrhs,Mdata)
       call read_double(efile,newrows,Edata)
       
       
       ! Feed FLIPS
       call flips_add(zz,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)
    end if

    ! Solve and write results
    if (calc_resid==1) then
       call flips_solve(zz,.TRUE.)
       call write_d_complex(sfile,ncols*nrhs,zz%solmat)
!write(*,*) 'Sol:',zz%solmat
       call write_double(residfile,nrhs,zz%residual)
    else
       call flips_solve(zz,.FALSE.)
       call write_d_complex(sfile,ncols*nrhs,zz%solmat)
    end if

    ! Calculate covariance and write results
    if (calc_cov == 1) then
       
       if (full_cov == 1) then
          allocate(covmat(ncols,ncols))
          call flips_calc_cov(zz,.TRUE.)
          call flips_get('cova',zz,covmat)
          call write_d_complex(covfile,ncols*ncols,covmat)
          deallocate(covmat)
       else
          call flips_calc_cov(zz,.FALSE.)
          call write_d_complex(covfile,ncols,zz%cmat)
       end if
    end if

    ! Write R and Y
    call write_d_complex(rfile,size(zz%rmat),zz%rmat)
    call write_d_complex(yfile,ncols*nrhs,zz%ymat)
    
    if (common > 0 ) then
       call write_d_complex(cvfile,size(zz%cvmat),zz%cvmat)
    end if

    call write_double(tresidfile,nrhs,zz%tmpres)
    
    ! Write bandwidth
    call write_bw(id,zz%bw)

    call flips_kill(zz)

  end subroutine f_solve_z



!! RESIZE !!!


  subroutine f_resize_d(id,ncols,nrhs,crows,newrows,rexists,&
       naddvar,ndelvar,bsize,cbw,common)
    
    implicit none

    character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,&
         naddvar,ndelvar,bsize,cbw,common

    real(dp), dimension(:), allocatable :: Adata
    real(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata

    logical, dimension(:), allocatable :: maskvec
    integer :: aa, i, nsize, new_rsize
    character(len=40) :: rmaskfile, afile, mfile, efile,&
         sfile, rfile, covfile, residfile, irfile, yfile,tresidfile,cvfile

    real(dp), dimension(ncols) :: Rrow

    type(flips_d) :: new_dd

    nsize = ncols-ndelvar+naddvar


    !write(*,*) id
    !write(*,*) ncols
    !write(*,*) nrhs
    !write(*,*) crows
    !write(*,*) newrows
    !write(*,*) rexists
    !write(*,*) naddvar
    !write(*,*) ndelvar
    !write(*,*) bsize


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)


    ! Initialize the original FLIPS problem
    call flips_init(dd,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Do we have R written in the disk?
    if (rexists==1) then
       ! Read R and Y
       call read_double(rfile,size(dd%rmat),dd%rmat)
       call read_double(yfile,ncols*nrhs,dd%ymat)

       if (common > 0 ) then
          call read_double(cvfile,size(dd%cvmat),dd%cvmat)
       end if

       call read_double(tresidfile,nrhs,dd%tmpres)
       if (crows > ncols) then
          dd%nrows = ncols
       else
          dd%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,dd%nrows
          Rrow = 0.0
          Rrow(i:min(i+dd%bw-1,ncols-common)) = dd%rmat(rind(i,i,ncols-common,dd%bw):rind(i,min(i+dd%bw-1,ncols-common),ncols-common,dd%bw))
          call find_first_and_len_d(ncols-common,1,dd%zeroth,Rrow(1:ncols-common),dd%rst(i),dd%rle(i))
          !dd%rst(i) = i
          !dd%rle(i) = min(i+cbw-1,ncols-common)
          !write(*,*) 'row',i,dd%rst(i),dd%rle(i)
       end do

       if (common > 0 ) then
          do i = ncols-common+1,ncols
             dd%rst(i) = i
             dd%rle(i) = 0
          end do
       end if

    end if

    ! Are there unrotated data rows?
    if (newrows > 0) then

       ! Read data and feed it into FLIPS

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_double(afile,newrows*ncols,Adata)
       call read_double(mfile,newrows*nrhs,Mdata)
       call read_double(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(dd,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)

    end if

    ! Are we marginalizing unknowns?
    if (ndelvar > 0) then
       allocate(maskvec(ncols))
       maskvec = .FALSE.
       write(rmaskfile,'("FLIPS_",A,"/rmask.dat")') trim(id)

       ! Read maskvec
       open(666,file=rmaskfile,form="unformatted",access="direct",recl=r_len_i)

       do i = 1,ncols
          read(666,rec=i) aa

          if (aa==1) maskvec(i) = .TRUE.
       end do

       close(666)

    end if


    !write(*,*) nsize
    !write(*,*) maskvec
    !write(*,*) ndelvar

    ! Now the original FLIPS problem is up-to-date. It is time to
    ! resize.



    if (ndelvar>0) then
       call flips_resize(nfob=new_dd,ofob=dd,newsize=nsize,remove=maskvec,buffersize=bsize)
       deallocate(maskvec)
    else
       call flips_resize(nfob=new_dd,ofob=dd,newsize=nsize,buffersize=bsize)
    end if


    !call flips_solve(new_dd)
    !write(*,*) new_dd%solmat

    ! Finally, write new R and Y back to disk. 
    !Note, that they replace the old ones (if there were any).

    new_rsize=size(new_dd%rmat)

    !write(*,*) new_rsize, size(new_dd%rmat)

    call write_double(rfile,new_rsize,new_dd%rmat)
    call write_double(yfile,nrhs*nsize,new_dd%ymat)
    call write_double(tresidfile,nrhs,new_dd%tmpres)

    if (new_dd%common > 0) then
       call write_double(cvfile,size(new_dd%cvmat),new_dd%cvmat)
    end if
       
       

    ! Write bandwidth
    call write_bw(id,new_dd%bw)

    ! Deallocate FLIPS data types
    call flips_kill(dd)
    call flips_kill(new_dd)
 
  end subroutine f_resize_d





 subroutine f_resize_s(id,ncols,nrhs,crows,newrows,rexists,&
       naddvar,ndelvar,bsize,cbw,common)
    
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,&
         naddvar,ndelvar,bsize,cbw,common

    real(sp), dimension(:), allocatable :: Adata
    real(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata

    logical, dimension(:), allocatable :: maskvec
    integer :: aa, i, nsize, new_rsize
    character(len=40) :: rmaskfile, afile, mfile, efile,&
         sfile, rfile, covfile, residfile, irfile, yfile,tresidfile,cvfile

    real(sp), dimension(ncols) :: Rrow

    type(flips_s) :: new_ss

    nsize = ncols-ndelvar+naddvar


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)


    ! Initialize the original FLIPS problem
    call flips_init(ss,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Do we have R written in the disk?
    if (rexists==1) then
       ! Read R and Y
       call read_single(rfile,size(ss%rmat),ss%rmat)
       call read_single(yfile,ncols*nrhs,ss%ymat)

       if (common > 0) then
          call read_single(cvfile,size(ss%cvmat),ss%cvmat)
       end if

       call read_single(tresidfile,nrhs,ss%tmpres)
       if (crows > ncols) then
          ss%nrows = ncols
       else
          ss%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,ss%nrows
          Rrow = 0.0
          Rrow(i:min(i+ss%bw-1,ncols-common)) = ss%rmat(rind(i,i,ncols-common,ss%bw):rind(i,min(i+ss%bw-1,ncols-common),ncols-common,ss%bw))
          call find_first_and_len_s(ncols-common,1,ss%zeroth,Rrow(1:ncols-common),ss%rst(i),ss%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common+1,ncols
             ss%rst(i) = i
             ss%rle(i) = 0
          end do
       end if

    end if

    ! Are there unrotated data rows?
    if (newrows > 0) then

       ! Read data and feed it into FLIPS

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_single(afile,newrows*ncols,Adata)
       call read_single(mfile,newrows*nrhs,Mdata)
       call read_single(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(ss,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)

    end if
   

    ! Are we marginalizing unknowns?
    if (ndelvar > 0) then
       allocate(maskvec(ncols))
       maskvec = .FALSE.
       write(rmaskfile,'("FLIPS_",A,"/rmask.dat")') trim(id)

       ! Read maskvec
       open(666,file=rmaskfile,form="unformatted",access="direct",recl=r_len_i)

       do i = 1,ncols
          read(666,rec=i) aa
          if (aa==1) maskvec(i) = .TRUE.
       end do

       close(666)
    end if

    ! Now the original FLIPS problem is up-to-date. It is time to
    ! resize.
    if (ndelvar>0) then
       call flips_resize(new_ss,ss,newsize=nsize,remove=maskvec,buffersize=bsize)
       deallocate(maskvec)
    else
       call flips_resize(new_ss,ss,newsize=nsize,buffersize=bsize)
    end if

    ! Finally, write new R and Y back to disk. 
    !Note, that they replace the old ones (if there were any).
    
     new_rsize=size(new_ss%rmat)

    call write_single(rfile,new_rsize,new_ss%rmat)
    call write_single(yfile,nrhs*nsize,new_ss%ymat)

   if (common > 0) then
       call write_single(cvfile,size(new_ss%cvmat),new_ss%cvmat)
    end if

    call write_single(tresidfile,nrhs,new_ss%tmpres)

    ! Write bandwidth
    call write_bw(id,ss%bw)

    ! Deallocate FLIPS data types
    call flips_kill(ss)
    call flips_kill(new_ss)
 

  end subroutine f_resize_s







 subroutine f_resize_c(id,ncols,nrhs,crows,newrows,rexists,&
       naddvar,ndelvar,bsize,cbw,common)
    
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,&
         naddvar,ndelvar,bsize,cbw,common

    complex(sp), dimension(:), allocatable :: Adata
    complex(sp), dimension(:), allocatable :: Mdata
    real(sp), dimension(:), allocatable :: Edata

    logical, dimension(:), allocatable :: maskvec
    integer :: aa, i, nsize, new_rsize
    character(len=40) :: rmaskfile, afile, mfile, efile,&
         sfile, rfile, covfile, residfile, irfile, yfile,tresidfile,cvfile

    complex(sp), dimension(ncols) :: Rrow

    type(flips_c) :: new_cc

    nsize = ncols-ndelvar+naddvar


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)  



    ! Initialize the original FLIPS problem
    call flips_init(cc,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Do we have R written in the disk?
    if (rexists==1) then
       ! Read R and Y
       call read_complex(rfile,size(cc%rmat),cc%rmat)
       call read_complex(yfile,ncols*nrhs,cc%ymat)

       if (common > 0 ) then
          call read_complex(cvfile,size(cc%cvmat),cc%cvmat)
       end if

       call read_single(tresidfile,nrhs,cc%tmpres)
       if (crows > ncols) then
          cc%nrows = ncols
       else
          cc%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,cc%nrows
          Rrow = 0.0
          Rrow(i:min(i+cc%bw-1,ncols-common)) = cc%rmat(rind(i,i,ncols-common,cc%bw):rind(i,min(i+cc%bw-1,ncols-common),ncols-common,cc%bw))
          call find_first_and_len_c(ncols-common,1,cc%zeroth,Rrow(1:ncols-common),cc%rst(i),cc%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common+1,ncols
             cc%rst(i) = i
             cc%rle(i) = 0
          end do
       end if

    end if

    ! Are there unrotated data rows?
    if (newrows > 0) then

       ! Read data and feed it into FLIPS

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_complex(afile,newrows*ncols,Adata)
       call read_complex(mfile,newrows*nrhs,Mdata)
       call read_single(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(cc,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)

    end if
   

    ! Are we marginalizing unknowns?
    if (ndelvar > 0) then
       allocate(maskvec(ncols))
       maskvec = .FALSE.
       write(rmaskfile,'("FLIPS_",A,"/rmask.dat")') trim(id)

       ! Read maskvec
       open(666,file=rmaskfile,form="unformatted",access="direct",recl=r_len_i)

       do i = 1,ncols
          read(666,rec=i) aa
          if (aa==1) maskvec(i) = .TRUE.
       end do

       close(666)
    end if

    ! Now the original FLIPS problem is up-to-date. It is time to
    ! resize.
    if (ndelvar>0) then
       call flips_resize(new_cc,cc,newsize=nsize,remove=maskvec,buffersize=bsize)
       deallocate(maskvec)
    else
       call flips_resize(new_cc,cc,newsize=nsize,buffersize=bsize)
    end if

    ! Finally, write new R and Y back to disk. 
    !Note, that they replace the old ones (if there were any).
    
    new_rsize=size(new_cc%rmat)

    call write_complex(rfile,new_rsize,new_cc%rmat)
    call write_complex(yfile,nrhs*nsize,new_cc%ymat)

    if (common > 0) then
       call write_complex(cvfile,size(new_cc%cvmat),new_cc%cvmat)
    end if

    call write_single(tresidfile,nrhs,new_cc%tmpres)

    ! Write bandwidth
    call write_bw(id,cc%bw)


    ! Deallocate FLIPS data types
    call flips_kill(cc)
    call flips_kill(new_cc)
 

  end subroutine f_resize_c





subroutine f_resize_z(id,ncols,nrhs,crows,newrows,rexists,&
       naddvar,ndelvar,bsize,cbw,common)
    
    implicit none

	character(len=32) :: id
    integer, intent(in) :: ncols,nrhs,crows,newrows,rexists,&
         naddvar,ndelvar,bsize,cbw,common

    complex(dp), dimension(:), allocatable :: Adata
    complex(dp), dimension(:), allocatable :: Mdata
    real(dp), dimension(:), allocatable :: Edata

    logical, dimension(:), allocatable :: maskvec
    integer :: aa, i, nsize, new_rsize
    character(len=40) :: rmaskfile, afile, mfile, efile,&
         sfile, rfile, covfile, residfile, irfile, yfile,tresidfile,cvfile

    complex(dp), dimension(ncols) :: Rrow

    type(flips_z) :: new_zz

    nsize = ncols-ndelvar+naddvar


    ! Construct filenames
    write(afile,'("FLIPS_",A,"/A.dat")') trim(id)
    write(mfile,'("FLIPS_",A,"/M.dat")') trim(id)
    write(efile,'("FLIPS_",A,"/E.dat")') trim(id)
    write(sfile,'("FLIPS_",A,"/sol.dat")') trim(id)
    write(rfile,'("FLIPS_",A,"/R.dat")') trim(id)
    write(covfile,'("FLIPS_",A,"/cov.dat")') trim(id)
    write(residfile,'("FLIPS_",A,"/resid.dat")') trim(id)
    write(irfile,'("FLIPS_",A,"/Rinv.dat")') trim(id)
    write(yfile,'("FLIPS_",A,"/Y.dat")') trim(id)
    write(tresidfile,'("FLIPS_",A,"/tres.dat")') trim(id)   
    write(cvfile,'("FLIPS_",A,"/cv.dat")') trim(id)  




    ! Initialize the original FLIPS problem
    call flips_init(zz,ncols,nrhs,bandwidth=cbw,buffersize=bsize,common=common)

    ! Do we have R written in the disk?
    if (rexists==1) then
       ! Read R and Y
       call read_d_complex(rfile,size(zz%rmat),zz%rmat)
       call read_d_complex(yfile,ncols*nrhs,zz%ymat)

       if (common > 0 ) then
          call read_d_complex(cvfile,size(zz%cvmat),zz%cvmat)
       end if

       call read_double(tresidfile,nrhs,zz%tmpres)
       if (crows > ncols) then
          cc%nrows = ncols
       else
          cc%nrows = crows
       end if

       ! Find the R row lengths
       do i = 1,zz%nrows
          Rrow = 0.0
          Rrow(i:min(i+zz%bw-1,ncols-common)) = zz%rmat(rind(i,i,ncols-common,zz%bw):rind(i,min(i+zz%bw-1,ncols-common),ncols-common,zz%bw))
          call find_first_and_len_z(ncols-common,1,zz%zeroth,Rrow(1:ncols-common),zz%rst(i),zz%rle(i))
       end do

       if (common > 0 ) then
          do i = ncols-common+1,ncols
             zz%rst(i) = i
             zz%rle(i) = 0
          end do
       end if


    end if

    ! Are there unrotated data rows?
    if (newrows > 0) then

       ! Read data and feed it into FLIPS

       allocate(Adata(newrows*ncols),Mdata(newrows*nrhs),Edata(newrows))

       ! Read data
       call read_d_complex(afile,newrows*ncols,Adata)
       call read_d_complex(mfile,newrows*nrhs,Mdata)
       call read_double(efile,newrows,Edata)
       
       ! Feed FLIPS
       call flips_add(zz,newrows,Adata,Mdata,Edata,.TRUE.)
       
       deallocate(Adata,Mdata,Edata)

    end if
   

    ! Are we marginalizing unknowns?
    if (ndelvar > 0) then
       allocate(maskvec(ncols))
       maskvec = .FALSE.
       write(rmaskfile,'("FLIPS_",A,"/rmask.dat")') trim(id)

       ! Read maskvec
       open(666,file=rmaskfile,form="unformatted",access="direct",recl=r_len_i)

       do i = 1,ncols
          read(666,rec=i) aa
          if (aa==1) maskvec(i) = .TRUE.
       end do

       close(666)
    end if

    ! Now the original FLIPS problem is up-to-date. It is time to
    ! resize.
    if (ndelvar>0) then
       call flips_resize(new_zz,zz,newsize=nsize,remove=maskvec,buffersize=bsize)
       deallocate(maskvec)
    else
       call flips_resize(new_zz,zz,newsize=nsize,buffersize=bsize)
    end if

    ! Finally, write new R and Y back to disk. 
    !Note, that they replace the old ones (if there were any).
    
    new_rsize=size(new_zz%rmat)

    call write_d_complex(rfile,new_rsize,new_zz%rmat)
    call write_d_complex(yfile,nrhs*nsize,new_zz%ymat)

    if (common > 0) then
       call write_d_complex(cvfile,size(new_zz%cvmat),new_zz%cvmat)
    end if

    call write_double(tresidfile,nrhs,new_zz%tmpres)

    ! Write bandwidth
    call write_bw(id,zz%bw)

    ! Deallocate FLIPS data types
    call flips_kill(zz)
    call flips_kill(new_zz)
 

  end subroutine f_resize_z






end program rlipseng
