/**
 * config.h
 *
 *  Created on: Jul 17, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#ifndef CONFIG_H_
#define CONFIG_H_

//! the maya grid function prefix
#define MAYA_GF_VAR_PFX "maya_gf_var"

//! the number of grid functions; change this parameter
//! to see if test passes
//#define MAYA_GRID_FUNC_COUNT 5113
#define MAYA_GRID_FUNC_COUNT 3

//! the size of the buffer for the name of maya var
#define MAYA_VAR_BUF_SIZE 30

//! params for the method
#define TRANSPORT_PARAMS ""

//! adios buffer size in MB
#define ADS_BUFFER_SIZE 50

//! the number of patches I want to write (this is what I got from Maya)
#define GLOBAL_PATCH_COUNT 27

//! GLOBAL_DIMENSIONS should be in accordance with MAYA_SHAPE_MAX_XXX
//! this is responsible for shape or slice of the data
#define GLOBAL_DIMENSIONS "P,48,89,116"
#define MAYA_SHAPE_MAX_X 48
#define MAYA_SHAPE_MAX_Y 89
#define MAYA_SHAPE_MAX_Z 116

//! for defining a box; should be <= for corresponding MAYA_SHAPE_MAX_Xxx
//! actual "shape" or slice of the data (this is because we had to
//! something like this in Maya); but don't quote me on that
#define MAYA_SHAPE_X 38
#define MAYA_SHAPE_Y 69
#define MAYA_SHAPE_Z 38


#endif //CONFIG_H_

