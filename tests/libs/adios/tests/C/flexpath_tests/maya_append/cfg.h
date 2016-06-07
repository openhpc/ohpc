/**
 * cfg.h
 *
 *  Created on: Aug 23, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#ifndef CFG_H_
#define CFG_H_

//! params for the method
#define TRANSPORT_PARAMS ""

//! adios buffer size in MB
#define ADS_BUFFER_SIZE 50

//! the name of adios group in the file
#define GRP_NAME "carpet_var"

//! how many loop iterations is in the writer
#define TIMESTEP_COUNT 10

//! the maya grid function prefix
#define MAYA_GF_VAR_PFX "maya_gf_var"

#endif /* CFG_H_ */
