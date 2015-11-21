#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include "core/qhashtbl.h"

/* Test the hashtable in ADIOS. 
 * Create a hashtable of size HASHTABLE_SIZE 
 * Generate variable paths of NPATHS*NVARS times as P<nnn>/v<mmm>
 * Generate double type scalars for each path/var 
 * Add them to the table
 * Retrieve each of them and check that the value retrieved is the expected one
 */

const int HASHTABLE_SIZE = 100; // how many rows in hash table
const int NPATHS = 650; // number of different paths tried
const int NVARS = 100;  // number of vars per path

char ** varpaths;
char ** varnames;
double  *  data; // npaths * nvars doubles,   pathid.nameid
int range;
int npaths;
int nvars;

void alloc_vars()
{
    int p,v;

    varpaths = (char**) malloc (npaths * sizeof(char*));
    for (p=0; p<npaths; p++) {
        varpaths[p] = (char*) malloc (16);
    }

    varnames = (char**) malloc (nvars * sizeof(char*));
    for (v=0; v<nvars; v++) {
        varnames[v] = (char*) malloc (16);
    }

    /* make varpaths like p001,p002,.. */
    int digit=1, d=10;
    while (npaths/d > 0) {
        d *= 10;
        digit++;
    }

    char fmt[16];
    sprintf (fmt, "p%%%d.%dd",digit,digit);
    printf ("fmt=[%s]\n", fmt);
    for (p=0; p<npaths; p++) {
        sprintf(varpaths[p], fmt, p);
    }
    printf ("varpaths[0]=%s\n", varpaths[0]);
    printf ("varpaths[%d]=%s\n", npaths-1, varpaths[npaths-1]);

    /* make varnames like v001,v002,.. */
    digit=1; d=10;
    while (nvars/d > 0) {
        d *= 10;
        digit++;
    }

    sprintf (fmt, "v%%%d.%dd",digit,digit);
    printf ("fmt=[%s]\n", fmt);
    for (v=0; v<nvars; v++) {
        sprintf(varnames[v], fmt, v);
    }
    printf ("varnames[0]=%s\n", varnames[0]);
    printf ("varnames[%d]=%s\n", nvars-1, varnames[nvars-1]);

    // make data
    data = (double *) malloc (sizeof(double)*npaths*nvars);
    for (p=0; p<npaths; p++) {
        for (v=0; v<nvars; v++) {
            data[p*nvars+v] = p + ((double)v)/d;
            //printf ("p=%d v=%d data[%d]=%lf\n", p, v, p*nvars+v, data[p*nvars+v]);
        }
    }
    printf ("data[0]=%lf\n", data[0]);
    printf ("data[%d]=%lf\n", npaths*nvars-1, data[npaths*nvars-1]);
}

void free_vars()
{
    int p,v;

    for (p=0; p<npaths; p++) {
        free (varpaths[p]);
    }
    free (varpaths);

    for (v=0; v<nvars; v++) {
        free(varnames[v]);
    }
    free(varnames);

    free(data);
}


int dotest ()
{
    int v,p;
    double *d;
    time_t tbegin, tput, tget;
    alloc_vars();
    qhashtbl_t *tbl = qhashtbl(range);

    /* Put all data values with path/var into hashtable */
    printf("============== PUT DATA INTO HASHTABLE ============\n");
    time (&tbegin);
    for (p=0; p<npaths; p++) {
        for (v=0; v<nvars; v++) {
            tbl->put2(tbl, varpaths[p], varnames[v], &data[p*nvars+v]);
        }
    }
    time (&tput);
    tput = tput - tbegin;

    /* Get each element and check value */
    printf("============== GET DATA FROM HASHTABLE ============\n");
    time (&tbegin);
    for (p=npaths-1; p>=0; p--) {
        for (v=nvars-1; v>=0; v--) {
            d = tbl->get2(tbl, varpaths[p], varnames[v]);
            if (d == NULL) {
                printf ("ERROR: stored value for p=%d,v=%d not found in hash table\n",
                        p, v);
                return 1;
            } else if (*d != data[p*nvars+v]) {
                printf ("ERROR: for p=%d,v=%d, found value = %lf in hash table != "
                        "what was put in = %lf\n",
                        p, v, *d, data[p*nvars+v]);
                return 2;
            }

        }
    }
    time (&tget);
    tget = tget - tbegin;
    printf("Timing: put %d elements in %d seconds, got them back in %d seconds\n",
            npaths*nvars, tput, tget);

    /* Print hashtable */
    printf("============== PRINT HASHTABLE ============\n");
    tbl->debug(tbl,NULL,0);

    /* Free hashtable */
    tbl->free(tbl);

    free_vars();
    return 0;
}

int main (int argc, char ** argv)
{
    int retval;
    range = HASHTABLE_SIZE;
    npaths = NPATHS;
    nvars = NVARS;

    retval = dotest();

    return retval;
}

