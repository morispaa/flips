!   FLIPS (Fortran Linear Inverse Problem Solver)
!   
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
  
  
  type flips_s
     logical :: cplx
     logical :: dbl
     logical :: use_files
     logical :: solexists
     logical :: rinverted
     logical :: covexists
     logical :: fullcov
     integer :: nrotbuf

     integer :: idnum
     character(len=32) :: rfile
     character(len=32) :: yfile
     character(len=32) :: cfile
     character(len=32) :: irfile
     character(len=32) :: solfile

     integer :: rfileunit
     integer :: yfileunit
     integer :: cfileunit
     integer :: irfileunit
     integer :: solfileunit

     integer :: ncols
     integer :: nrhs
     integer :: nbuf
     integer :: nrows
     integer :: bw
     integer :: bbw
     integer :: common

     integer(8) :: fc

     integer :: rl
    
     real(sp) :: zeroth

     integer, dimension(:), allocatable :: rst
     integer, dimension(:), allocatable :: rle
     integer, dimension(:), allocatable :: bst
     integer, dimension(:), allocatable :: ble

     real(sp), dimension(:), allocatable :: tmpres
     real(sp), dimension(:), allocatable :: residual
     
     real(sp), dimension(:), allocatable :: arotbuf   
     real(sp), dimension(:), allocatable :: cvbuf
     real(sp), dimension(:), allocatable :: mrotbuf
     real(sp), dimension(:), allocatable :: rmat
     real(sp), dimension(:), allocatable :: cvmat
     real(sp), dimension(:), allocatable :: ymat
     real(sp), dimension(:), allocatable :: solmat
     real(sp), dimension(:), allocatable :: invrmat
     real(sp), dimension(:), allocatable :: cmat


 
  end type flips_s
