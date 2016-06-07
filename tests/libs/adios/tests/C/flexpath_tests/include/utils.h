/**
 * utils.h
 *
 *  Created on: Jul 5, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#ifndef UTILS_H_
#define UTILS_H_

extern diag_t usage(char *program_name, char *program_desc);
extern diag_t gen_1D_array(double *p_arr, int arr_len, int rank);
extern diag_t gen_1D_array2(double *p_arr, int arr_len, int rank);
extern diag_t gen_1D_array_int(int *p_arr, int arr_len, int rank);
extern diag_t get_options(struct adios_tsprt_opts * p_tsprt_opts, int argc, char ** argv, int *p_show_help);

extern diag_t set_value(double *p_arr, int arr_len, double value);
extern diag_t get_data_size(int *shape, int shape_elem_count, int* data_size);
extern diag_t get_maya_var_name(char *prefix, int number);
extern diag_t gen_maya_var_name(char *buf, int buf_size, char *maya_var_pfx, int number);



#endif /* UTILS_H_ */
