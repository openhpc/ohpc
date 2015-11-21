/*
 * Copyright (C) 2009-2012 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all
 * documents related to the source code ("Material") are owned by 
 * Intel Corporation or its suppliers or licensors. Title to the
 * Material remains with Intel Corporation or its suppliers and
 * licensors. The Material is protected by worldwide copyright
 * laws and treaty provisions.  No part of the Material may be
 * used, copied, reproduced, modified, published, uploaded,
 * posted, transmitted, distributed,  or disclosed in any way
 * except as expressly provided in the license provided with the
 * Materials.  No license under any patent, copyright, trade
 * secret or other intellectual property right is granted to or
 * conferred upon you by disclosure or delivery of the Materials,
 * either expressly, by implication, inducement, estoppel or
 * otherwise, except as expressly provided in the license
 * provided with the Materials. 
 *
 * Simple header file for abstracting machine-dependent timers
 *
 * This file declares the following functions:
 *
 * cilk_getticks() - Returns a timer value with a resolution of
 * milliseconds
 *
 * cilk_ticks_to_seconds - Converts the timer value to seconds.
 */

#ifdef _WIN32
#include <Windows.h>

static inline unsigned long long cilk_getticks()
{
    // Fetch number of milliseconds that have elapsed sin the system started
     return GetTickCount();
}

static inline double cilk_ticks_to_seconds(unsigned long long ticks)
{
     return ticks * 1.0e-3;
}
#endif

#if defined __unix__ || defined __APPLE__
#include <time.h>
#include <sys/time.h>

static inline unsigned long long cilk_getticks()
{
     struct timeval t;
     gettimeofday(&t, 0);
     return t.tv_sec * 1000000ULL + t.tv_usec;
}

static inline double cilk_ticks_to_seconds(unsigned long long ticks)
{
     return ticks * 1.0e-6;
}
#endif
