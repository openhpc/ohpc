#ifndef ADIOS_SCHEMA_H
#define ADIOS_SCHEMA_H

#ifdef __cplusplus
extern "C" {
#endif

enum ADIOS_MESH_TYPE
{
     ADIOS_MESH_UNIFORM      = 1
    ,ADIOS_MESH_STRUCTURED   = 2
    ,ADIOS_MESH_RECTILINEAR  = 3
    ,ADIOS_MESH_UNSTRUCTURED = 4
};

typedef struct
{
    int num_dimensions;
    uint64_t * dimensions;
    double * origins;
    double * spacings;
    double * maximums;
} MESH_UNIFORM;

typedef struct
{
    int use_single_var;        // 1 means coordinates-single-var,0 means coordinates-multi-var
    int num_dimensions;
    uint64_t * dimensions;
    char ** coordinates;       // name of the variable(s) containing the rectilinear spacing values
} MESH_RECTILINEAR;

typedef struct
{
    int use_single_var;        // 1 means points-single-var, 0 mean points-multi-var
    int num_dimensions;
    uint64_t * dimensions;
    int nspaces;
    char ** points;            // name of the variable(s) containing the point coordinates 
} MESH_STRUCTURED;

// ADIOS Schema: supported cell types
enum ADIOS_CELL_TYPE
{
     ADIOS_CELL_PT         = 1
    ,ADIOS_CELL_LINE       = 2
    ,ADIOS_CELL_TRI        = 3
    ,ADIOS_CELL_QUAD       = 4
    ,ADIOS_CELL_HEX        = 5
    ,ADIOS_CELL_PRI        = 6
    ,ADIOS_CELL_TET        = 7
    ,ADIOS_CELL_PYR        = 8
};

typedef struct
{
    int nspaces;
    uint64_t npoints;
    int nvar_points;           // how much vars for points-multi-var, 1 for points-single-var
    char ** points;
    int ncsets;
    uint64_t * ccounts;
    char ** cdata;
    enum ADIOS_CELL_TYPE * ctypes;
} MESH_UNSTRUCTURED;


typedef struct {   //type returned by adios_inq_mesh for read method
    int id;
    char * name;
    char * file_name; // 0 means mesh struct from the same file, otherwise mesh struct from externel file 
    int time_varying;           //0 means not time-varying, 1 means time-varying
    enum ADIOS_MESH_TYPE type;
    union
    {
        MESH_UNIFORM * uniform;
        MESH_RECTILINEAR * rectilinear;
        MESH_STRUCTURED * structured;
        MESH_UNSTRUCTURED * unstructured;
    };
} ADIOS_MESH;

#ifdef __cplusplus
}
#endif

#endif
