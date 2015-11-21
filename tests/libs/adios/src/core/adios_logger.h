#ifndef __ADIOS_LOGGER_H__
#define __ADIOS_LOGGER_H__

/* Logger functions */
#include <stdio.h>
#include <string.h>

extern FILE *adios_logf;
extern int adios_verbose_level; // default = 2 (print warnings)
extern int adios_abort_on_error; // default = 0 (don't abort)
extern char *adios_log_names[4];

/* Direct log into file(s).
   rank >=0: log file is <logpath>.<rank>
   rank <0:  log file is <logpath>
*/
void adios_logger_open (char *logpath, int rank);
void adios_logger_close();

#define  adios_logger(verbose_level, print_header, ...) { \
    if (adios_verbose_level >= verbose_level) { \
        if (!adios_logf) adios_logf=stderr; \
        if (print_header) \
            fprintf (adios_logf, "%s: ",adios_log_names[verbose_level-1]); \
        fprintf (adios_logf, __VA_ARGS__); \
        fflush(adios_logf);\
    }\
}


#define log_error(...) adios_logger(1, 1, __VA_ARGS__); if (adios_abort_on_error) abort();
#define log_warn(...) adios_logger(2, 1, __VA_ARGS__)
#define log_info(...) adios_logger(3, 1, __VA_ARGS__)
#define log_debug(...) adios_logger(4, 1, __VA_ARGS__)

#define log_error_cont(...) adios_logger(1, 0, __VA_ARGS__)
#define log_warn_cont(...) adios_logger(2, 0, __VA_ARGS__)
#define log_info_cont(...) adios_logger(3, 0, __VA_ARGS__)
#define log_debug_cont(...) adios_logger(4, 0, __VA_ARGS__)

#endif
