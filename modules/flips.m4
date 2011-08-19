!-*-f90-*-
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

module flips


  ! This is the main module of flips.

  implicit none

  !*****************************************************************
  ! Interfaces
  !*****************************************************************

  interface print_info
     module procedure print_info_s, print_info_d,print_info_z
  end interface

  interface flips_init
     module procedure flipsinit_s, flipsinit_d,flipsinit_c,flipsinit_z
  end interface

  interface flips_kill
     module procedure flipskill_s, flipskill_d,flipskill_c,flipskill_z
  end interface

  interface flips_add
     module procedure flipsadd_s, flipsadd_d, flipsadd_scalar_s, &
          flipsadd_scalar_d, flipsadd_c,flipsadd_scalar_c,&
          flipsadd_z,flipsadd_scalar_z, flipsadd_onerow_s,&
          flipsadd_onerow_d,flipsadd_onerow_c,flipsadd_onerow_z
  end interface

  interface flips_add_fc
     module procedure flipsadd_full_cov_s,flipsadd_full_cov_d,&
          flipsadd_full_cov_c,flipsadd_full_cov_z
  end interface

  interface flips_solve
     module procedure flipssolve_s, flipssolve_d, flipssolve_c,&
          flipssolve_z
  end interface

  interface flips_calc_cov
     module procedure flipscalccov_s, flipscalccov_d, flipscalccov_c,&
          flipscalccov_z
  end interface

  interface flips_resize
     module procedure flipsresize_s, flipsresize_d, flipsresize_c, &
          flipsresize_z
  end interface

  interface flips_get
     module procedure get_matrix_s, get_matrix_d, get_matrix_c,&
          get_matrix_z, get_vector_s, get_vector_d, get_vector_c,&
          get_vector_z
  end interface

  interface flips_copy
     module procedure fobcopy_s, fobcopy_d, fobcopy_c, fobcopy_z
  end interface

  interface flips_delete
     module procedure flipsdelete_s,flipsdelete_d
  end interface

  interface flips_rotate
     module procedure flipsrotate_s, flipsrotate_d, flipsrotate_c, flipsrotate_z
  end interface


  !*****************************************************************
  ! FLIPS parameters
  !*****************************************************************

  integer, parameter :: sp = kind(1.0E0)
  integer, parameter :: dp = kind(1.0D0)

  ! Default rotation buffer size THIS NEEDS WORK!
  integer, parameter :: defbufsize = 100

  ! Global variables (evil, evil, evil!!!)
  ! Prints more information on screen.
  ! NB: Not fully implemented!
  logical :: verbose = .FALSE.
  ! Flag for flop counting
  ! NB: This is not accurate any more! 
  logical :: FLIPS_FLOPS = .TRUE.


ifdef(`F2003_STREAM',
  ` character(*), parameter :: flipsfileform = "unformatted"
  character(*), parameter :: flipsfileaccess = "stream"'
)



ifdef(`DIRECT',
  `character(*), parameter :: flipsfileform = "unformatted"
  character(*), parameter :: flipsfileaccess = "direct"'
)

  !*****************************************************************
  ! FLIPS data types
  !*****************************************************************

  ! Real-single
  include 'datatypes/flips_s.f90'

  ! Real-double
  include 'datatypes/flips_d.f90'

  ! Complex-single
  include 'datatypes/flips_c.f90'

  ! Complex-double
  include 'datatypes/flips_z.f90'

contains

  !*****************************************************************
  ! Auxillary routines
  !*****************************************************************

  include 'auxillary/rind.f90'
  include 'auxillary/frind.f90'
  include 'auxillary/yind.f90'
  include 'auxillary/irind.f90'

  include 'auxillary/print_info_s.f90'
  include 'auxillary/print_info_d.f90'
  include 'auxillary/print_info_c.f90'
  include 'auxillary/print_info_z.f90'

  include 'auxillary/find_first_and_len_s.f90'
  include 'auxillary/find_first_and_len_d.f90'
  include 'auxillary/find_first_and_len_c.f90'
  include 'auxillary/find_first_and_len_z.f90'

  include 'auxillary/modcholmat_s.f90'
  include 'auxillary/modcholmat_d.f90'
  include 'auxillary/modcholmat_c.f90'
  include 'auxillary/modcholmat_z.f90'

  include 'auxillary/modcholvec_s.f90'
  include 'auxillary/modcholvec_d.f90'
  include 'auxillary/modcholvec_c.f90'
  include 'auxillary/modcholvec_z.f90'

  !*****************************************************************
  ! Data management
  !*****************************************************************

  include 'datamanagement/copy_matrix_s.f90'
  include 'datamanagement/get_row_str_s.f90'
  include 'datamanagement/put_row_str_s.f90'
  include 'datamanagement/get_matrix_str_s.f90'
  include 'datamanagement/get_vector_str_s.f90'

  include 'datamanagement/add_to_buffer_s.f90'
  include 'datamanagement/get_ir_row_s.f90'
  include 'datamanagement/put_ir_col_s.f90'
  include 'datamanagement/open_cov_file_str_s.f90'
  include 'datamanagement/open_files_str_s.f90'
  include 'datamanagement/open_invr_file_str_s.f90'
  include 'datamanagement/open_invr_file_dir_s.f90'
  include 'datamanagement/write_solmat_file_str_s.f90'
  include 'datamanagement/rresize_s.f90'


  include 'datamanagement/copy_matrix_d.f90'
  include 'datamanagement/get_row_str_d.f90'
  include 'datamanagement/put_row_str_d.f90'
  include 'datamanagement/get_matrix_str_d.f90'
  include 'datamanagement/get_vector_str_d.f90'

  include 'datamanagement/add_to_buffer_d.f90'
  include 'datamanagement/get_ir_row_d.f90'
  include 'datamanagement/put_ir_col_d.f90'
  include 'datamanagement/open_cov_file_str_d.f90'
  include 'datamanagement/open_files_str_d.f90'
  include 'datamanagement/open_invr_file_str_d.f90'
  include 'datamanagement/open_invr_file_dir_d.f90'
  include 'datamanagement/write_solmat_file_str_d.f90'
  include 'datamanagement/rresize_d.f90'


  include 'datamanagement/copy_matrix_c.f90'
  include 'datamanagement/get_row_str_c.f90'
  include 'datamanagement/put_row_str_c.f90'
  include 'datamanagement/get_matrix_str_c.f90'
  include 'datamanagement/get_vector_str_c.f90'

  include 'datamanagement/add_to_buffer_c.f90'
  include 'datamanagement/get_ir_row_c.f90'
  include 'datamanagement/put_ir_col_c.f90'
  include 'datamanagement/open_cov_file_str_c.f90'
  include 'datamanagement/open_files_str_c.f90'
  include 'datamanagement/open_invr_file_str_c.f90'
  include 'datamanagement/open_invr_file_dir_c.f90'
  include 'datamanagement/write_solmat_file_str_c.f90'
  include 'datamanagement/rresize_c.f90'


  include 'datamanagement/copy_matrix_z.f90'
  include 'datamanagement/get_row_str_z.f90'
  include 'datamanagement/put_row_str_z.f90'
  include 'datamanagement/get_matrix_str_z.f90'
  include 'datamanagement/get_vector_str_z.f90'

  include 'datamanagement/add_to_buffer_z.f90'
  include 'datamanagement/get_ir_row_z.f90'
  include 'datamanagement/put_ir_col_z.f90'
  include 'datamanagement/open_cov_file_str_z.f90'
  include 'datamanagement/open_files_str_z.f90'
  include 'datamanagement/open_invr_file_str_z.f90'
  include 'datamanagement/open_invr_file_dir_z.f90'
  include 'datamanagement/write_solmat_file_str_z.f90'
  include 'datamanagement/rresize_z.f90'



  !*****************************************************************
  ! Rotations
  !*****************************************************************

  include 'rotations/make_rotations_s.f90'
  include 'rotations/rotate_partial_s.f90'
  include 'rotations/rotate_full_s.f90'
  include 'rotations/rot_coeff_s.f90'
  include 'rotations/rot_vec_s.f90'
 

  include 'rotations/make_rotations_d.f90'
  include 'rotations/rotate_partial_d.f90'
  include 'rotations/rotate_full_d.f90'
  include 'rotations/rot_coeff_d.f90'
  include 'rotations/rot_vec_d.f90'


  include 'rotations/make_rotations_c.f90'
  include 'rotations/rotate_partial_c.f90'
  include 'rotations/rotate_full_c.f90'
  include 'rotations/rot_coeff_c.f90'
  include 'rotations/rot_vec_c.f90'

  include 'rotations/make_rotations_z.f90'
  include 'rotations/rotate_partial_z.f90'
  include 'rotations/rotate_full_z.f90'
  include 'rotations/rot_coeff_z.f90'
  include 'rotations/rot_vec_z.f90'


  !*****************************************************************
  ! Solver routines
  !*****************************************************************

  include 'solver/solve_utriag_mem_s.f90'
  include 'solver/inv_col_s.f90'
  include 'solver/invert_r_s.f90'
  include 'solver/calculate_residual_s.f90'
  include 'solver/calculate_covariance_s.f90'


  include 'solver/solve_utriag_mem_d.f90'
  include 'solver/inv_col_d.f90'
  include 'solver/invert_r_d.f90'
  include 'solver/calculate_residual_d.f90'
  include 'solver/calculate_covariance_d.f90'

  include 'solver/solve_utriag_mem_c.f90'
  include 'solver/inv_col_c.f90'
  include 'solver/invert_r_c.f90'
  include 'solver/calculate_residual_c.f90'
  include 'solver/calculate_covariance_c.f90'

  include 'solver/solve_utriag_mem_z.f90'
  include 'solver/inv_col_z.f90'
  include 'solver/invert_r_z.f90'
  include 'solver/calculate_residual_z.f90'
  include 'solver/calculate_covariance_z.f90'


  !*****************************************************************
  ! Resizing routines
  !*****************************************************************

  include 'kalman/fobcopy_s.f90'
  include 'kalman/fobcopy_d.f90'
  include 'kalman/fobcopy_c.f90'
  include 'kalman/fobcopy_z.f90'

  include 'kalman/resize_s.f90'
  include 'kalman/resize_c.f90'
  include 'kalman/resize_d.f90'
  include 'kalman/resize_z.f90'


  !*****************************************************************
  ! User routines
  !*****************************************************************

  include 'main/flipsinit_s.f90'
  include 'main/flipskill_s.f90'
  include 'main/flipsadd_s.f90'
  include 'main/flipsadd_alt_s.f90'
  include 'main/flipsadd_onerow_s.f90'
  include 'main/flipsadd_full_cov_s.f90'
  include 'main/flipssolve_s.f90'
  include 'main/flipscalccov_s.f90'
  include 'main/flipsresize_s.f90'
  include 'main/flipsrotate_s.f90'

  include 'main/flipsinit_d.f90'
  include 'main/flipskill_d.f90'
  include 'main/flipsadd_d.f90'
  include 'main/flipsadd_alt_d.f90'
  include 'main/flipsadd_onerow_d.f90'
  include 'main/flipsadd_full_cov_d.f90'
  include 'main/flipssolve_d.f90'
  include 'main/flipscalccov_d.f90'
  include 'main/flipsresize_d.f90'
  include 'main/flipsrotate_d.f90'

  include 'main/flipsinit_c.f90'
  include 'main/flipskill_c.f90'
  include 'main/flipsadd_c.f90'
  include 'main/flipsadd_alt_c.f90'
  include 'main/flipsadd_onerow_c.f90'
  include 'main/flipsadd_full_cov_c.f90'
  include 'main/flipssolve_c.f90'
  include 'main/flipscalccov_c.f90'
  include 'main/flipsresize_c.f90'
  include 'main/flipsrotate_c.f90'

  include 'main/flipsinit_z.f90'
  include 'main/flipskill_z.f90'
  include 'main/flipsadd_z.f90'
  include 'main/flipsadd_alt_z.f90'
  include 'main/flipsadd_onerow_z.f90'
  include 'main/flipsadd_full_cov_z.f90'
  include 'main/flipssolve_z.f90'
  include 'main/flipscalccov_z.f90'
  include 'main/flipsresize_z.f90'
  include 'main/flipsrotate_z.f90'

!***************************************************
!* Antirotations
!***************************************************


  include 'main/flipsdelete_s.f90'
  include 'main/flipsdelete_d.f90'
  include 'main/flipsdelete_c.f90'
  include 'main/flipsdelete_z.f90'
  include 'rotations/arot_coeff_s.f90'
  include 'rotations/arot_coeff_d.f90'
  include 'rotations/arot_coeff_c.f90'
  include 'rotations/arot_coeff_z.f90'

end module flips


