/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef _ADIOS_TIMING_H_
#define _ADIOS_TIMING_H_


#include <stdint.h>


#define ADIOS_TIMING_MAX_USER_TIMERS 16
#define ADIOS_TIMING_MAX_EVENTS 1024 


struct adios_timing_event_struct
{
    int type; // index of event type, reference timing_struct->names[type] for name of event type
    int is_start;
    double time; // time that the event occurred

};


struct adios_timing_struct
{
    int64_t internal_count;
    int64_t user_count;
    char ** names;
    double *times;
    
    // keep the last MAX_EVENTS events, older events
    // are overwritten in a circular fashion
    int64_t event_count;
    struct adios_timing_event_struct events[ADIOS_TIMING_MAX_EVENTS];

};

//int adios_get_timing_count (int64_t fd_p, int64_t * tc);
//int adios_get_timing_name (int64_t fd_p, int64_t index, char* name);
//int adios_get_timing_value (int64_t fd_p, int64_t index, double* value);


struct adios_timing_struct *  adios_timing_create (int timer_count, char** timer_names);
void adios_clear_timers (struct adios_timing_struct * ts);
void adios_timing_destroy (struct adios_timing_struct * timing_obj);

void adios_timing_go (struct adios_timing_struct * ts, int64_t index);
void adios_timing_stop (struct adios_timing_struct * ts, int64_t index);


void adios_timing_declare_user_timers (int64_t fd_p, int user_timer_count, char** user_timer_names);


/* Print out all gathered info */
void adios_timing_write_xml_common (int64_t fd_p, const char* filename);

#endif
