# FLIPS makefile
# Copyright (C) 2006 University of Oulu
# Written by Mikko Orispaa <mikko.orispaa@oulu.fi>
# Licensed under GPL version 2

include Make.defs

LIB=libflips.a
MODDIR=modules

all: ppfiles flips.o $(LIB)

ppfiles:
	m4 -DPTYPE=s \
		modules/main/flipsinit.m4 > modules/main/flipsinit_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsinit.m4 > modules/main/flipsinit_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsinit.m4 > modules/main/flipsinit_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsinit.m4 > modules/main/flipsinit_z.f90

	m4 -DPTYPE=s \
		modules/main/flipskill.m4 > modules/main/flipskill_s.f90
	m4 -DPTYPE=d \
		modules/main/flipskill.m4 > modules/main/flipskill_d.f90
	m4 -DPTYPE=c \
		modules/main/flipskill.m4 > modules/main/flipskill_c.f90
	m4 -DPTYPE=z \
		modules/main/flipskill.m4 > modules/main/flipskill_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsresize.m4 > modules/main/flipsresize_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsresize.m4 > modules/main/flipsresize_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsresize.m4 > modules/main/flipsresize_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsresize.m4 > modules/main/flipsresize_z.f90

	m4 -DPTYPE=s \
		modules/main/flipssolve.m4 > modules/main/flipssolve_s.f90
	m4 -DPTYPE=d \
		modules/main/flipssolve.m4 > modules/main/flipssolve_d.f90
	m4 -DPTYPE=c \
		modules/main/flipssolve.m4 > modules/main/flipssolve_c.f90
	m4 -DPTYPE=z \
		modules/main/flipssolve.m4 > modules/main/flipssolve_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsdelete.m4 > modules/main/flipsdelete_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsdelete.m4 > modules/main/flipsdelete_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsdelete.m4 > modules/main/flipsdelete_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsdelete.m4 > modules/main/flipsdelete_z.f90


	m4 -DPTYPE=s \
		modules/main/flipsrotate.m4 > modules/main/flipsrotate_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsrotate.m4 > modules/main/flipsrotate_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsrotate.m4 > modules/main/flipsrotate_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsrotate.m4 > modules/main/flipsrotate_z.f90


	m4 -DPTYPE=s \
		modules/datamanagement/copy_matrix.m4 > modules/datamanagement/copy_matrix_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/copy_matrix.m4 > modules/datamanagement/copy_matrix_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/copy_matrix.m4 > modules/datamanagement/copy_matrix_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/copy_matrix.m4 > modules/datamanagement/copy_matrix_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/get_ir_row.m4 > modules/datamanagement/get_ir_row_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/get_ir_row.m4 > modules/datamanagement/get_ir_row_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/get_ir_row.m4 > modules/datamanagement/get_ir_row_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/get_ir_row.m4 > modules/datamanagement/get_ir_row_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/get_matrix_str.m4 > modules/datamanagement/get_matrix_str_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/get_matrix_str.m4 > modules/datamanagement/get_matrix_str_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/get_matrix_str.m4 > modules/datamanagement/get_matrix_str_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/get_matrix_str.m4 > modules/datamanagement/get_matrix_str_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/get_vector_str.m4 > modules/datamanagement/get_vector_str_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/get_vector_str.m4 > modules/datamanagement/get_vector_str_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/get_vector_str.m4 > modules/datamanagement/get_vector_str_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/get_vector_str.m4 > modules/datamanagement/get_vector_str_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/put_ir_col_rbr_dir.m4 > modules/datamanagement/put_ir_col_rbr_dir_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/put_ir_col_rbr_dir.m4 > modules/datamanagement/put_ir_col_rbr_dir_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/put_ir_col_rbr_dir.m4 > modules/datamanagement/put_ir_col_rbr_dir_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/put_ir_col_rbr_dir.m4 > modules/datamanagement/put_ir_col_rbr_dir_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/put_ir_col_str_rbr.m4 > modules/datamanagement/put_ir_col_str_rbr_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/put_ir_col_str_rbr.m4 > modules/datamanagement/put_ir_col_str_rbr_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/put_ir_col_str_rbr.m4 > modules/datamanagement/put_ir_col_str_rbr_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/put_ir_col_str_rbr.m4 > modules/datamanagement/put_ir_col_str_rbr_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/put_row_str.m4 > modules/datamanagement/put_row_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS)  \
		modules/datamanagement/put_row_str.m4 > modules/datamanagement/put_row_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS)  \
		modules/datamanagement/put_row_str.m4 > modules/datamanagement/put_row_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS)  \
		modules/datamanagement/put_row_str.m4 > modules/datamanagement/put_row_str_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/get_row_str.m4 > modules/datamanagement/get_row_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/get_row_str.m4 > modules/datamanagement/get_row_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/get_row_str.m4 > modules/datamanagement/get_row_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/get_row_str.m4 > modules/datamanagement/get_row_str_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/put_ir_col.m4 > modules/datamanagement/put_ir_col_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/put_ir_col.m4 > modules/datamanagement/put_ir_col_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/put_ir_col.m4 > modules/datamanagement/put_ir_col_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/put_ir_col.m4 > modules/datamanagement/put_ir_col_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/write_solmat_file.m4 > modules/datamanagement/write_solmat_file_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/write_solmat_file.m4 > modules/datamanagement/write_solmat_file_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/write_solmat_file.m4 > modules/datamanagement/write_solmat_file_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/write_solmat_file.m4 > modules/datamanagement/write_solmat_file_str_z.f90

	m4 -DPTYPE=s \
		modules/datamanagement/add_to_buffer.m4 > modules/datamanagement/add_to_buffer_s.f90
	m4 -DPTYPE=d \
		modules/datamanagement/add_to_buffer.m4 > modules/datamanagement/add_to_buffer_d.f90
	m4 -DPTYPE=c \
		modules/datamanagement/add_to_buffer.m4 > modules/datamanagement/add_to_buffer_c.f90
	m4 -DPTYPE=z \
		modules/datamanagement/add_to_buffer.m4 > modules/datamanagement/add_to_buffer_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/open_cov_file_str.m4 > modules/datamanagement/open_cov_file_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/open_cov_file_str.m4 > modules/datamanagement/open_cov_file_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/open_cov_file_str.m4 > modules/datamanagement/open_cov_file_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/open_cov_file_str.m4 > modules/datamanagement/open_cov_file_str_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/open_files_str.m4 > modules/datamanagement/open_files_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/open_files_str.m4 > modules/datamanagement/open_files_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/open_files_str.m4 > modules/datamanagement/open_files_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/open_files_str.m4 > modules/datamanagement/open_files_str_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/open_invr_file_str.m4 > modules/datamanagement/open_invr_file_str_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/open_invr_file_str.m4 > modules/datamanagement/open_invr_file_str_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/open_invr_file_str.m4 > modules/datamanagement/open_invr_file_str_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/open_invr_file_str.m4 > modules/datamanagement/open_invr_file_str_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/open_invr_file_dir.m4 > modules/datamanagement/open_invr_file_dir_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/open_invr_file_dir.m4 > modules/datamanagement/open_invr_file_dir_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/open_invr_file_dir.m4 > modules/datamanagement/open_invr_file_dir_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/open_invr_file_dir.m4 > modules/datamanagement/open_invr_file_dir_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/datamanagement/rresize.m4 > modules/datamanagement/rresize_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/datamanagement/rresize.m4 > modules/datamanagement/rresize_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/datamanagement/rresize.m4 > modules/datamanagement/rresize_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/datamanagement/rresize.m4 > modules/datamanagement/rresize_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/auxillary/modcholmat.m4 > modules/auxillary/modcholmat_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/auxillary/modcholmat.m4 > modules/auxillary/modcholmat_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/auxillary/modcholmat.m4 > modules/auxillary/modcholmat_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/auxillary/modcholmat.m4 > modules/auxillary/modcholmat_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/auxillary/modcholvec.m4 > modules/auxillary/modcholvec_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/auxillary/modcholvec.m4 > modules/auxillary/modcholvec_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/auxillary/modcholvec.m4 > modules/auxillary/modcholvec_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/auxillary/modcholvec.m4 > modules/auxillary/modcholvec_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/auxillary/find_first_and_len.m4 > modules/auxillary/find_first_and_len_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/auxillary/find_first_and_len.m4 > modules/auxillary/find_first_and_len_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/auxillary/find_first_and_len.m4 > modules/auxillary/find_first_and_len_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/auxillary/find_first_and_len.m4 > modules/auxillary/find_first_and_len_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/auxillary/print_info.m4 > modules/auxillary/print_info_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/auxillary/print_info.m4 > modules/auxillary/print_info_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/auxillary/print_info.m4 > modules/auxillary/print_info_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/auxillary/print_info.m4 > modules/auxillary/print_info_z.f90

	m4 -DPTYPE=s $(M4FLAGS) \
		modules/auxillary/relem.m4 > modules/auxillary/relem_s.f90
	m4 -DPTYPE=d $(M4FLAGS) \
		modules/auxillary/relem.m4 > modules/auxillary/relem_d.f90
	m4 -DPTYPE=c $(M4FLAGS) \
		modules/auxillary/relem.m4 > modules/auxillary/relem_c.f90
	m4 -DPTYPE=z $(M4FLAGS) \
		modules/auxillary/relem.m4 > modules/auxillary/relem_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsadd_alt.m4 > modules/main/flipsadd_alt_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsadd_alt.m4 > modules/main/flipsadd_alt_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsadd_alt.m4 > modules/main/flipsadd_alt_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsadd_alt.m4 > modules/main/flipsadd_alt_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsadd.m4 > modules/main/flipsadd_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsadd.m4 > modules/main/flipsadd_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsadd.m4 > modules/main/flipsadd_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsadd.m4 > modules/main/flipsadd_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsadd_onerow.m4 > modules/main/flipsadd_onerow_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsadd_onerow.m4 > modules/main/flipsadd_onerow_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsadd_onerow.m4 > modules/main/flipsadd_onerow_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsadd_onerow.m4 > modules/main/flipsadd_onerow_z.f90

	m4 -DPTYPE=s \
		modules/main/flipsadd_full_cov.m4 > modules/main/flipsadd_full_cov_s.f90
	m4 -DPTYPE=d \
		modules/main/flipsadd_full_cov.m4 > modules/main/flipsadd_full_cov_d.f90
	m4 -DPTYPE=c \
		modules/main/flipsadd_full_cov.m4 > modules/main/flipsadd_full_cov_c.f90
	m4 -DPTYPE=z \
		modules/main/flipsadd_full_cov.m4 > modules/main/flipsadd_full_cov_z.f90

	m4 -DPTYPE=s \
		modules/main/flipscalccov.m4 > modules/main/flipscalccov_s.f90
	m4 -DPTYPE=d \
		modules/main/flipscalccov.m4 > modules/main/flipscalccov_d.f90
	m4 -DPTYPE=c \
		modules/main/flipscalccov.m4 > modules/main/flipscalccov_c.f90
	m4 -DPTYPE=z \
		modules/main/flipscalccov.m4 > modules/main/flipscalccov_z.f90

	m4 -DPTYPE=s \
		modules/kalman/fobcopy.m4 > modules/kalman/fobcopy_s.f90
	m4 -DPTYPE=d \
		modules/kalman/fobcopy.m4 > modules/kalman/fobcopy_d.f90
	m4 -DPTYPE=c \
		modules/kalman/fobcopy.m4 > modules/kalman/fobcopy_c.f90
	m4 -DPTYPE=z \
		modules/kalman/fobcopy.m4 > modules/kalman/fobcopy_z.f90

	m4 -DPTYPE=s \
		modules/kalman/resize.m4 > modules/kalman/resize_s.f90
	m4 -DPTYPE=d \
		modules/kalman/resize.m4 > modules/kalman/resize_d.f90
	m4 -DPTYPE=c \
		modules/kalman/resize.m4 > modules/kalman/resize_c.f90
	m4 -DPTYPE=z \
		modules/kalman/resize.m4 > modules/kalman/resize_z.f90

	m4 -DPTYPE=s \
		modules/rotations/arot_coeff.m4 > modules/rotations/arot_coeff_s.f90
	m4 -DPTYPE=d \
		modules/rotations/arot_coeff.m4 > modules/rotations/arot_coeff_d.f90
	m4 -DPTYPE=c \
		modules/rotations/arot_coeff_complex.m4 > modules/rotations/arot_coeff_c.f90
	m4 -DPTYPE=z \
		modules/rotations/arot_coeff_complex.m4 > modules/rotations/arot_coeff_z.f90

	m4 -DPTYPE=s \
		modules/rotations/rot_coeff_real.m4 > modules/rotations/rot_coeff_s.f90
	m4 -DPTYPE=d \
		modules/rotations/rot_coeff_real.m4 > modules/rotations/rot_coeff_d.f90
	m4 -DPTYPE=c \
		modules/rotations/rot_coeff_complex.m4 > modules/rotations/rot_coeff_c.f90
	m4 -DPTYPE=z \
		modules/rotations/rot_coeff_complex.m4 > modules/rotations/rot_coeff_z.f90

	m4 -DPTYPE=s \
		modules/rotations/make_rotations.m4 > modules/rotations/make_rotations_s.f90
	m4 -DPTYPE=d \
		modules/rotations/make_rotations.m4 > modules/rotations/make_rotations_d.f90
	m4 -DPTYPE=c \
		modules/rotations/make_rotations.m4 > modules/rotations/make_rotations_c.f90
	m4 -DPTYPE=z \
		modules/rotations/make_rotations.m4 > modules/rotations/make_rotations_z.f90

	m4 -DPTYPE=s \
		modules/rotations/rot_vec.m4 > modules/rotations/rot_vec_s.f90
	m4 -DPTYPE=d \
		modules/rotations/rot_vec.m4 > modules/rotations/rot_vec_d.f90
	m4 -DPTYPE=c \
		modules/rotations/rot_vec.m4 > modules/rotations/rot_vec_c.f90
	m4 -DPTYPE=z \
		modules/rotations/rot_vec.m4 > modules/rotations/rot_vec_z.f90

	m4 -DPTYPE=s \
		modules/rotations/rotate_full.m4 > modules/rotations/rotate_full_s.f90
	m4 -DPTYPE=d \
		modules/rotations/rotate_full.m4 > modules/rotations/rotate_full_d.f90
	m4 -DPTYPE=c \
		modules/rotations/rotate_full.m4 > modules/rotations/rotate_full_c.f90
	m4 -DPTYPE=z \
		modules/rotations/rotate_full.m4 > modules/rotations/rotate_full_z.f90

	m4 -DPTYPE=s \
		modules/rotations/rotate_partial.m4 > modules/rotations/rotate_partial_s.f90
	m4 -DPTYPE=d \
		modules/rotations/rotate_partial.m4 > modules/rotations/rotate_partial_d.f90
	m4 -DPTYPE=c \
		modules/rotations/rotate_partial.m4 > modules/rotations/rotate_partial_c.f90
	m4 -DPTYPE=z \
		modules/rotations/rotate_partial.m4 > modules/rotations/rotate_partial_z.f90

	m4 -DPTYPE=s \
		modules/solver/calculate_covariance.m4 > modules/solver/calculate_covariance_s.f90
	m4 -DPTYPE=d \
		modules/solver/calculate_covariance.m4 > modules/solver/calculate_covariance_d.f90
	m4 -DPTYPE=c \
		modules/solver/calculate_covariance.m4 > modules/solver/calculate_covariance_c.f90
	m4 -DPTYPE=z \
		modules/solver/calculate_covariance.m4 > modules/solver/calculate_covariance_z.f90

	m4 -DPTYPE=s \
		modules/solver/calculate_residual.m4 > modules/solver/calculate_residual_s.f90
	m4 -DPTYPE=d \
		modules/solver/calculate_residual.m4 > modules/solver/calculate_residual_d.f90
	m4 -DPTYPE=c \
		modules/solver/calculate_residual.m4 > modules/solver/calculate_residual_c.f90
	m4 -DPTYPE=z \
		modules/solver/calculate_residual.m4 > modules/solver/calculate_residual_z.f90

	m4 -DPTYPE=s \
		modules/solver/inv_col.m4 > modules/solver/inv_col_s.f90
	m4 -DPTYPE=d \
		modules/solver/inv_col.m4 > modules/solver/inv_col_d.f90
	m4 -DPTYPE=c \
		modules/solver/inv_col.m4 > modules/solver/inv_col_c.f90
	m4 -DPTYPE=z \
		modules/solver/inv_col.m4 > modules/solver/inv_col_z.f90

	m4 -DPTYPE=s \
		modules/solver/invert_r.m4 > modules/solver/invert_r_s.f90
	m4 -DPTYPE=d \
		modules/solver/invert_r.m4 > modules/solver/invert_r_d.f90
	m4 -DPTYPE=c \
		modules/solver/invert_r.m4 > modules/solver/invert_r_c.f90
	m4 -DPTYPE=z \
		modules/solver/invert_r.m4 > modules/solver/invert_r_z.f90

	m4 -DPTYPE=s \
		modules/solver/solve_utriag_mem.m4 > modules/solver/solve_utriag_mem_s.f90
	m4 -DPTYPE=d \
		modules/solver/solve_utriag_mem.m4 > modules/solver/solve_utriag_mem_d.f90
	m4 -DPTYPE=c \
		modules/solver/solve_utriag_mem.m4 > modules/solver/solve_utriag_mem_c.f90
	m4 -DPTYPE=z \
		modules/solver/solve_utriag_mem.m4 > modules/solver/solve_utriag_mem_z.f90



flips.o: modules/flips.m4
	m4 $(M4FLAGS) modules/flips.m4 > modules/flips2.f90
	$(FC) -c $(FFLAGS) modules/flips2.f90 -o flips.o

#flips2.o: modules/flips2.f90
#	$(CPP) modules/flips.f90 > modules/flips2.f90
#	$(FC) -c $(FFLAGS)  $< -o $@

$(LIB): flips.o
	ar -r $@ $<
	ranlib $@

flipseng: interfaces/flipseng2.f90 flips.o
	$(FC) $(FFLAGS) interfaces/flipseng2.f90 flips.o -o flipseng2

flipseng22: interfaces/flipseng22.f90 flips.o
	$(FC) $(FFLAGS) interfaces/flipseng22.f90 flips.o -o flipseng22

flipseng24: interfaces/flipseng24.f90 flips.o
	$(FC) $(FFLAGS) interfaces/flipseng24.f90 flips.o -o flipseng24


clean:
	rm -f flipseng*
	rm -f config.log
	rm -f *.o
	rm -f *.mod
	rm -f $(LIB)
	rm -f modules/flips2.f90
	rm -f modules/main/flipsinit_[sdcz].f90
	rm -f modules/main/flipskill_[sdcz].f90
	rm -f modules/main/flipsadd_[sdcz].f90
	rm -f modules/main/flipsadd_onerow_[sdcz].f90
	rm -f modules/main/flipsadd_alt_[sdcz].f90
	rm -f modules/main/flipsadd_full_cov_[sdcz].f90
	rm -f modules/main/flipscalccov_[sdcz].f90
	rm -f modules/main/flipsresize_[sdcz].f90
	rm -f modules/main/flipssolve_[sdcz].f90
	rm -f modules/main/flipsdelete_[sdcz].f90
	rm -f modules/main/flipsrotate_[sdcz].f90
	rm -f modules/datamanagement/copy_matrix_[sdcz].f90
	rm -f modules/datamanagement/get_ir_row_[sdcz].f90
	rm -f modules/datamanagement/get_matrix_str_[sdcz].f90
	rm -f modules/datamanagement/get_vector_str_[sdcz].f90
	rm -f modules/datamanagement/put_ir_col_rbr_dir_[sdcz].f90
	rm -f modules/datamanagement/put_ir_col_str_rbr_[sdcz].f90
	rm -f modules/datamanagement/put_row_str_[sdcz].f90
	rm -f modules/datamanagement/get_row_str_[sdcz].f90
	rm -f modules/datamanagement/put_ir_col_[sdcz].f90
	rm -f modules/datamanagement/write_solmat_file_str_[sdcz].f90
	rm -f modules/datamanagement/add_to_buffer_[sdcz].f90
	rm -f modules/datamanagement/open_cov_file_str_[sdcz].f90
	rm -f modules/datamanagement/open_files_str_[sdcz].f90
	rm -f modules/datamanagement/open_invr_file_str_[sdcz].f90
	rm -f modules/datamanagement/open_invr_file_dir_[sdcz].f90
	rm -f modules/datamanagement/rresize_[sdcz].f90
	rm -f modules/auxillary/modcholmat_[sdcz].f90
	rm -f modules/auxillary/modcholvec_[sdcz].f90
	rm -f modules/auxillary/find_first_and_len_[sdcz].f90
	rm -f modules/auxillary/print_info_[sdcz].f90
	rm -f modules/auxillary/relem_[sdcz].f90
	rm -f modules/kalman/fobcopy_[sdcz].f90
	rm -f modules/kalman/resize_[sdcz].f90
	rm -f modules/rotations/arot_coeff_[sdcz].f90
	rm -f modules/rotations/make_rotations_[sdcz].f90
	rm -f modules/rotations/rot_coeff_[sdcz].f90
	rm -f modules/rotations/rot_vec_[sdcz].f90
	rm -f modules/rotations/rotate_full_[sdcz].f90
	rm -f modules/rotations/rotate_partial_[sdcz].f90
	rm -f modules/solver/calculate_covariance_[sdcz].f90
	rm -f modules/solver/calculate_residual_[sdcz].f90
	rm -f modules/solver/inv_col_[sdcz].f90
	rm -f modules/solver/invert_r_[sdcz].f90
	rm -f modules/solver/solve_utriag_mem_[sdcz].f90

cleanall: clean
	cd tests && make clean && cd ..
	cd interfaces && make clean && cd ..
