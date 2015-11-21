#ifndef _FLEXPATH_H
#define _FLEXPATH_H


#include "core/adios_logger.h"

#define CONTACT_LENGTH 1024

#define READER_CONTACT_FILE "reader_info.txt"
#define WRITER_CONTACT_FILE "writer_info.txt"
#define READER_READY_FILE "reader_ready.txt"
#define WRITER_READY_FILE "writer_ready.txt"
#define FP_RANK_ATTR_NAME "fp_rank_num"
#define FP_DST_ATTR_NAME "fp_dst_rank"
#define FP_DIM_ATTR_NAME "fp_dim"
#define FP_NDIMS_ATTR_NAME "fp_ndims"

#define CLOSE_MSG 0
#define OPEN_MSG 1
#define ACK_MSG 2
#define INIT_MSG 3
#define EOS_MSG 4

#define FP_FORTRAN_MODE 1
#define FP_C_MODE 0

#define perr(...) if(getenv("FP_DEBUG")) fprintf(stderr, __VA_ARGS__);

#define fp_log(LOG, ...)                             \
            if(getenv("FP_DEBUG")) {    \
                if(strcmp(getenv("FP_DEBUG"),"ALL")==0) {          \
                    fprintf(stderr, __VA_ARGS__);   \
                } else if(strcmp(getenv("FP_DEBUG"),LOG)==0) {     \
                    fprintf(stderr, __VA_ARGS__);   \
                }                                   \
            }

#define fp_write_log(LOG, ...)                                      \
            if(getenv("FP_DEBUG")) {                                \
                if(strcmp(getenv("FP_DEBUG"),"ALL")==0) {           \
                    fprintf(stderr, "%d %s:", flexpathWriteData.rank, LOG);   \
                    fprintf(stderr, __VA_ARGS__);                   \
                } else {                                            \
                    char* env_tok;                                  \
                    char* env = strdup(getenv("FP_DEBUG"));         \
                    env_tok = strtok(env, ",");                     \
                    while(env_tok) {                                \
                        if(strcmp(env_tok, LOG)==0) {               \
                    fprintf(stderr, "%d %s:", flexpathWriteData.rank, LOG);   \
                    fprintf(stderr, __VA_ARGS__);                   \
                        }                                           \
                        env_tok = strtok(NULL, ",");                \
                    }                                               \
                }                                                   \
            }
            

//adios_logger(4,1, __VA_ARGS__);

#define CONTACT_STR_LEN 50

typedef enum {FORMAT, DATA, EVGROUP, STEP } Flush_type;

typedef struct _update_step_msg{
    int step;
    int finalized;
    int condition;
}update_step_msg;

typedef struct _drop_evgroup{
    int step;
    int condition;
}drop_evgroup_msg;
/*
 * Contains the offset information for a variable for all writers.
 * offsets_per_rank is == ndims.
 */
typedef struct _offset_struct{
    int offsets_per_rank;
    int total_offsets;
    uint64_t *local_dimensions;
    uint64_t *local_offsets;
    uint64_t *global_dimensions;
} offset_struct;

typedef struct _var {
    char * name;
    int noffset_structs;
    offset_struct * offsets;    
} global_var, *global_var_ptr;

typedef struct _evgroup {    
    int condition;
    int num_vars;
    int step;
    int process_id;
    char *group_name;
    global_var* vars;
} evgroup, *evgroup_ptr;

typedef struct _op_msg
{
    int process_id;
    char *file_name;
    int type; //4 = end_of_stream, 3 = init, 2 = ack, 1 = open, 0 = close,
    int step;
    int condition;
} op_msg, *op_msg_ptr;
 
typedef struct flush_msg_ {
    Flush_type type;
    int process_id;
    int condition;
    int id;
} Flush_msg, *Flush_msg_ptr;

typedef struct var_msg_ {
    char* var_name;
    int process_id;
    int condition;
} Var_msg, *Var_msg_ptr;

typedef struct _complex_dummy
{
    float r;
    float i;
} complex_dummy;

typedef struct _double_complex_dummy
{
    double r;
    double i;
} double_complex_dummy;

static FMField complex_dummy_field_list[] =
{
    {"r", "float", sizeof(float), FMOffset(complex_dummy*, r)},
    {"i", "float", sizeof(float), FMOffset(complex_dummy*, i)},
    {NULL, NULL, 0, 0}
};

static FMField double_complex_dummy_field_list[] =
{
    {"r", "double", sizeof(double), FMOffset(double_complex_dummy*, r)},
    {"i", "double", sizeof(double), FMOffset(double_complex_dummy*, i)},
    {NULL, NULL, 0, 0}
};

static FMField update_step_msg_field_list[]=
{
    {"step", "integer", sizeof(int), FMOffset(update_step_msg*, step)},
    {"finalized", "integer", sizeof(int), FMOffset(update_step_msg*, finalized)},
    {"condition", "integer", sizeof(int), FMOffset(update_step_msg*, condition)},
    {NULL, NULL, 0, 0}
};

static FMField drop_evgroup_msg_field_list[]=
{
    {"step", "integer", sizeof(int), FMOffset(drop_evgroup_msg*, step)},
    {"condition", "integer", sizeof(int), FMOffset(drop_evgroup_msg*, condition)},
    {NULL, NULL, 0, 0}
};

static FMField offset_struct_field_list[]=
{
    {"offsets_per_rank", "integer", sizeof(int), FMOffset(offset_struct*, offsets_per_rank)},
    {"total_offsets", "integer", sizeof(int), FMOffset(offset_struct*, total_offsets)},
    {"local_dimensions", "integer[total_offsets]", sizeof(uint64_t), FMOffset(offset_struct*, local_dimensions)},
    {"local_offsets", "integer[total_offsets]", sizeof(uint64_t), FMOffset(offset_struct*, local_offsets)},
    {"global_dimensions", "integer[offsets_per_rank]", sizeof(uint64_t), FMOffset(offset_struct*, global_dimensions)},
    {NULL, NULL, 0, 0}
};

static FMField global_var_field_list[]=
{
    {"name", "string", sizeof(char*), FMOffset(global_var_ptr, name)},
    {"noffset_structs", "integer", sizeof(int), FMOffset(global_var_ptr, noffset_structs)},
    {"offsets", "offset_struct[noffset_structs]", sizeof(offset_struct), FMOffset(global_var_ptr, offsets)},
    {NULL, NULL, 0, 0}
};

static FMField evgroup_field_list[]=
{
    {"condition", "integer", sizeof(int), FMOffset(evgroup_ptr, condition)},
    {"num_vars", "integer", sizeof(int), FMOffset(evgroup_ptr, num_vars)},
    {"step", "integer", sizeof(int), FMOffset(evgroup_ptr, step)},
    {"process_id", "integer", sizeof(int), FMOffset(evgroup_ptr, process_id)},
    {"group_name", "string", sizeof(char*), FMOffset(evgroup_ptr, group_name)},
    {"vars", "global_var[num_vars]", sizeof(global_var), FMOffset(evgroup_ptr, vars)},
    {NULL, NULL, 0, 0}
};

static FMField flush_field_list[] =
{   
    {"type", "integer", sizeof(Flush_type), FMOffset(Flush_msg_ptr, type)},
    {"process_id", "integer", sizeof(int), FMOffset(Flush_msg_ptr, process_id)},
    {"condition", "integer", sizeof(int), FMOffset(Flush_msg_ptr, condition)},
    {"id", "integer", sizeof(int), FMOffset(Flush_msg_ptr, id)},
    {NULL, NULL, 0, 0}
};

static FMField var_field_list[] =
{
    {"var_name", "string", sizeof(char*), FMOffset(Var_msg_ptr, var_name)},
    {"process_id", "integer", sizeof(int), FMOffset(Var_msg_ptr, process_id)},
    {NULL, NULL, 0, 0}
};


static FMField op_file_field_list[] =
{
    {"process_id", "integer", sizeof(int), FMOffset(op_msg_ptr, process_id)},
    {"file_name", "string", sizeof(char*), FMOffset(op_msg_ptr, file_name)},
    {"type", "integer", sizeof(int), FMOffset(op_msg_ptr, type)},
    {"step", "integer", sizeof(int), FMOffset(op_msg_ptr, step)},
    {"condition", "integer", sizeof(int), FMOffset(op_msg_ptr, condition)},
    {NULL, NULL, 0, 0}
};

static FMStructDescRec update_step_msg_format_list[]=
{
    {"update_step_msg", update_step_msg_field_list, sizeof(update_step_msg), NULL},
    {NULL, NULL, 0, 0}
};

static FMStructDescRec drop_evgroup_msg_format_list[]=
{
    {"drop_evgroup_msg", drop_evgroup_msg_field_list, sizeof(drop_evgroup_msg), NULL},
    {NULL, NULL, 0, 0}
};

static FMStructDescRec offset_struct_format_list[] =
{
    {"offset_struct", offset_struct_field_list, sizeof(offset_struct), NULL},
    {NULL, NULL, 0, 0}
};


static FMStructDescRec evgroup_format_list[] =
{   
    {"evgroup", evgroup_field_list, sizeof(evgroup), NULL},
    {"offset_struct", offset_struct_field_list, sizeof(offset_struct), NULL},
    {"global_var", global_var_field_list, sizeof(global_var), NULL},
    {NULL,NULL,0,NULL}
};

static FMStructDescRec flush_format_list[] =
{   
    {"flush", flush_field_list, sizeof(Flush_msg), NULL},
    {NULL,NULL,0,NULL}
};
 
static FMStructDescRec var_format_list[] =
{
    {"varMsg", var_field_list, sizeof(Var_msg), NULL},
    {NULL, NULL, 0, NULL}
};

static FMStructDescRec data_format_list[] =
{
    {"anonymous", NULL, 0, NULL},
    {NULL, NULL, 0, NULL}
};

static FMStructDescRec op_format_list[] =
{
    {"op_msg", op_file_field_list, sizeof(op_msg), NULL},
    {NULL, NULL, 0, NULL}
};

static char *getFixedName(char *name);


#endif
