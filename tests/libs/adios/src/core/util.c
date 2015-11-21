#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>

#include "config.h"
#include "core/util.h"
#include "core/bp_utils.h"
#include "core/adios_endianness.h"
#include "core/adios_logger.h"

/* Reverse the order in an array in place.
   use swapping from Fortran/column-major order to ADIOS-read-api/C/row-major order and back
*/
void swap_order(int n, uint64_t *array, int *timedim)
{
    int i;
    uint64_t tmp;
    for (i=0; i<n/2; i++) {
        tmp = array[i];
        array[i] = array[n-1-i];
        array[n-1-i] = tmp;
    }
    if (*timedim > -1)
        *timedim = (n-1) - *timedim; // swap the time dimension too
}

/* Change endianness of each element in an array */
/* input: array, size in bytes(!), size of one element */
void change_endianness( void *data, uint64_t slice_size, enum ADIOS_DATATYPES type)
{
    int size_of_type = bp_get_type_size(type, "");
    uint64_t n = slice_size / size_of_type;
    uint64_t i;
    char *ptr = (char *) data;

    if (slice_size % size_of_type != 0) {
       log_error ("Adios error in bp_utils.c:change_endianness(): "
                  "An array's endianness is to be converted but the size of array "
                  "is not dividable by the size of the elements: "
                  "size = %lld, element size = %d\n", slice_size, size_of_type);
    }

    switch (type)
    {
        case adios_byte:
        case adios_short:
        case adios_integer:
        case adios_long:
        case adios_unsigned_byte:
        case adios_unsigned_short:
        case adios_unsigned_integer:
        case adios_unsigned_long:
        case adios_real:
        case adios_double:
        case adios_long_double:
            switch (size_of_type) {
                /* case 1: nothing to do */
                case 2:
                    for (i=0; i < n; i++) {
                        swap_16_ptr(ptr);
                        ptr += size_of_type;
                    }
                    break;
                case 4:
                    for (i=0; i < n; i++) {
                        swap_32_ptr(ptr);
                        ptr += size_of_type;
                    }
                    break;
                case 8:
                    for (i=0; i < n; i++) {
                        swap_64_ptr(ptr);
                        ptr += size_of_type;
                    }
                    break;
                case 16:
                    for (i=0; i < n; i++) {
                        swap_128_ptr(ptr);
                        ptr += size_of_type;
                    }
                    break;
            }
            break;

        case adios_complex:
            for (i=0; i < n; i++) {
                swap_32_ptr(ptr);   // swap REAL part 4 bytes 
                swap_32_ptr(ptr+4); // swap IMG part 4 bytes
                ptr += size_of_type;
            }
            break;

        case adios_double_complex:
            for (i=0; i < n; i++) {
                swap_64_ptr(ptr);   // swap REAL part 8 bytes 
                swap_64_ptr(ptr+8); // swap IMG part 8 bytes
                ptr += size_of_type;
            }
            break;

        case adios_string:
        default:
            /* nothing to do */
            break;
    }
}

void copy_data (void *dst, void *src,
        int idim,
        int ndim,
        uint64_t* size_in_dset,
        uint64_t* ldims,
        const uint64_t * readsize,
        uint64_t dst_stride,
        uint64_t src_stride,
        uint64_t dst_offset,
        uint64_t src_offset,
        uint64_t ele_num,
        int      size_of_type,
        enum ADIOS_FLAG change_endiness,
        enum ADIOS_DATATYPES type
        )
{
    unsigned int i, j;
    uint64_t dst_offset_new=0;
    uint64_t src_offset_new=0;
    uint64_t src_step, dst_step;
    if (ndim-1==idim) {
        for (i=0;i<size_in_dset[idim];i++) {
            memcpy ((char *)dst + (i*dst_stride+dst_offset)*size_of_type,
                    (char *)src + (i*src_stride+src_offset)*size_of_type,
                    ele_num*size_of_type);
            if (change_endiness == adios_flag_yes) {
                change_endianness ((char *)dst + (i*dst_stride+dst_offset)*size_of_type, 
                                   ele_num*size_of_type, type);
            }
        }
        return;
    }

    for (i = 0; i<size_in_dset[idim];i++) {
        // get the different step granularity 
        // for each different reading pattern broke
        src_step = 1;
        dst_step = 1;
        for (j = idim+1; j <= ndim-1;j++) {
            src_step *= ldims[j];
            dst_step *= readsize[j];
        }
        src_offset_new =src_offset + i * src_stride * src_step;
        dst_offset_new = dst_offset + i * dst_stride * dst_step;
        copy_data ( dst, src, idim+1, ndim, size_in_dset,
                ldims,readsize,
                dst_stride, src_stride,
                dst_offset_new, src_offset_new,
                ele_num, size_of_type, change_endiness, type);
    }
}

void alloc_namelist (char ***namelist, int length)
{
        int j;

        *namelist = (char **) malloc(length*sizeof(char*));
        for (j=0;j<length;j++)
                (*namelist)[j] = (char *) malloc(255);

        return;
}

void free_namelist (char **namelist, int length)
{
        int i;
        if (namelist) {
                for (i=0;i<length;i++) {
                        if(namelist[i])
                                free(namelist[i]);
                }
                free(namelist);
        }
        return;
}

void list_insert_read_request_tail (read_request ** h, read_request * q)
{
    read_request * head;
    if (!h || !q)
    {
        printf ("Error: list_insert_read_request_tail cannot handle NULL parameters ()\n");
        return;
    }

    head = * h;
    if (!head)
    {
        * h = q;
        q->next = NULL;

        return;
    }

    while (head->next)
    {
        head = head->next;
    }

    head->next = q;
    q->next = NULL;

    return;
}

void list_append_read_request_list (read_request ** h, read_request * q)
{
    read_request * head;
    if (!h || !q)
    {
        printf ("Error: list_append_read_request_list: h: %d, q: %d\n", h == 0, q == 0);
        return;
    }

    head = * h;
    if (!head)
    {
        * h = q;
        return;
    }

    while (head->next)
    {
        head = head->next;
    }

    head->next = q;

    return;
}

void list_insert_read_request_next (read_request ** h, read_request * q)
{
    read_request * head;
    if (!h || !q)
    {
        printf ("Error: list_insert_read_request_next cannot handle NULL parameters ()\n");
        return;
    }

    head = * h;
    if (!head)
    {
        * h = q;
        q->next = NULL;
    }
    else
    {
        // NCSU ALACRITY-ADIOS: Fixed this prepend ordering bug. Previously, prepending A, B, C, D would produce
        //   [A, D, C, B], which causes poor seek performance for the Transforms layer versus raw Transport layer
        //   due to backwards seeks. The fixed code now properly produces [D, C, B, A]

        //q->next = head->next;
        //head->next = q;
        q->next = head;
        *h = q;
    }

    return;
}


void list_free_read_request (read_request * h)
{
    read_request * n;

    while (h)
    {
        n = h->next;

        free_selection (h->sel);
        if (h->priv)
        {
            free (h->priv);
            h->priv = 0;
        }
        free (h);
        h = n;
    }
}

int list_get_length (read_request * h)
{
    int l = 0;

    while (h)
    {
        h = h->next;
        l++;
    }

    return l;
}

read_request * copy_read_request (const read_request * r)
{
    read_request * newreq;

    newreq = (read_request *) malloc (sizeof (read_request));
    assert (newreq);

    newreq->sel = copy_selection (r->sel);
    newreq->varid = r->varid;
    newreq->from_steps = r->from_steps;
    newreq->nsteps = r->nsteps;
    newreq->data = r->data;
    newreq->datasize = r->datasize;
    newreq->priv = r->priv;
    newreq->next = 0;

    return newreq;
}

ADIOS_SELECTION * copy_selection (const ADIOS_SELECTION * sel)
{
    ADIOS_SELECTION * nsel;

    nsel = (ADIOS_SELECTION *) malloc (sizeof (ADIOS_SELECTION));
    assert (nsel);

    nsel->type = sel->type;

    if (sel->type == ADIOS_SELECTION_BOUNDINGBOX)
    {
        nsel->u.bb.ndim = sel->u.bb.ndim;
        nsel->u.bb.start = (uint64_t *) malloc (sel->u.bb.ndim * 8);
        nsel->u.bb.count = (uint64_t *) malloc (sel->u.bb.ndim * 8);
        assert (nsel->u.bb.start && nsel->u.bb.count);

        memcpy (nsel->u.bb.start, sel->u.bb.start, sel->u.bb.ndim * 8);
        memcpy (nsel->u.bb.count, sel->u.bb.count, sel->u.bb.ndim * 8);
    }
    else if (sel->type == ADIOS_SELECTION_POINTS)
    {
        nsel->u.points.ndim = sel->u.points.ndim;
        nsel->u.points.npoints = sel->u.points.npoints;
        nsel->u.points.points = (uint64_t *) malloc (nsel->u.points.npoints * nsel->u.points.ndim * 8);
        assert (nsel->u.points.points);

        memcpy (nsel->u.points.points, sel->u.points.points, sel->u.points.npoints * sel->u.points.ndim * 8);
    }
    else if (sel->type == ADIOS_SELECTION_WRITEBLOCK)
    {
        nsel->u.block.index = sel->u.block.index;
        // NCSU ALACRITY-ADIOS: Copy the new fields
        nsel->u.block.is_absolute_index = sel->u.block.is_absolute_index;
        nsel->u.block.is_sub_pg_selection = sel->u.block.is_sub_pg_selection;
        nsel->u.block.element_offset = sel->u.block.element_offset;
        nsel->u.block.nelements = sel->u.block.nelements;
    }
    else if (sel->type == ADIOS_SELECTION_AUTO)
    {
        //TODO
    }
    else
    {
        //adios_error (err_invalid_argument, "Wrong ADIOS selection type.\n");
    }

    return nsel;
}

void free_selection (ADIOS_SELECTION * sel)
{
    sel->type = sel->type;

    if (sel->type == ADIOS_SELECTION_BOUNDINGBOX)
    {
        free (sel->u.bb.start);
        free (sel->u.bb.count);
    }
    else if (sel->type == ADIOS_SELECTION_POINTS)
    {
        free (sel->u.points.points);
    }

    free (sel);
}

int unique (uint32_t * nids, int size)
{
    int i, j, k;
    uint32_t temp;

    // sort the nids first
    for (i = 1; i < size; i++)
    {
        for (j = 0; j < size - i; j++)
        {
            if (nids[j] > nids[j + 1])
            {
                temp = nids[j];
                nids[j] = nids[j + 1];
                nids[j + 1] = temp;
            }
        }
    }

    // remove duplicates
    i = 0;
    k = 0;
    while (i < size)
    {
        nids[k] = nids[i];

        j = i + 1;
        while (j < size && nids[i] == nids[j])
        {
            j++;
        }
   
        if (j < size)
        {
            k++;
            i = j;            
        }
        else
        {
            break;
        }
    }

    return k + 1;
}

uint32_t nid_atoi ()
{
    int name_len;
    char * nid_str, * str_buf = malloc (MPI_MAX_PROCESSOR_NAME);
    uint32_t nid;

    MPI_Get_processor_name (str_buf, &name_len);
    nid_str = str_buf;
    while (*nid_str != '\0' && (!isdigit (*nid_str) || *nid_str == '0'))
    {
        nid_str++;
    }

    if (*nid_str == '\0')
    {
        // report an error
    }

    nid = atoi (nid_str);
    free (str_buf);

    return nid;
}

// This helper routine returns a vector of unique NID's.
// It is caller's responsiblity to free nids afterwards.
int get_unique_nids (MPI_Comm comm, uint32_t ** nids)
{
    int size;
    uint32_t my_nid;

    my_nid = nid_atoi ();
    MPI_Comm_size (comm, &size);
    * nids = (uint32_t *) malloc (size * 4);
    assert (* nids);

    MPI_Allgather (&my_nid, 1, MPI_INT,
                   *nids, 1, MPI_INT,
                   comm);
    return unique (*nids, size);
}

/*******************************************************
   Processing parameter lists
**********************************************************/
static char * remove_whitespace (char *start, char *end) 
{
    char *s = start;
    char *e = end;
    //int orig_len = (int) (e-s);
    int final_len;
    char *res;
    // remove front whitespace (but do not go far beyond the end)
    while (s <= e && 
           (*s==' ' || *s=='\t' || *s=='\n')
          ) s++;
    if (s <= e) { // there is some text 
        // remove tail whitespace
        while (s <= e && 
               (*e==' ' || *e=='\t' || *e=='\n')
              ) e--;
        // create result 
        final_len = e - s + 1; //  length of result 
        if (final_len > 0) {
            res = (char *) malloc (final_len + 1); // allocate space s..e and \0
            memcpy(res, s, final_len);
            res[final_len] = 0;
        } else {
            // "   = something" patterns end here
            res = NULL;
        }
    } else {
        // no non-whitespace character found
        res = NULL;
    }
    return res;
}


/* Split a line at = sign into name and value pair
   Remove " ", TAB and Newline from around names and values
   Return NULL for name and value if there is no = sign in line
   Return newly allocated strings otherwise
   Used by: esimmon_internal_text_to_name_value_pairs
 */
static void splitnamevalue (const char * line, int linelen,  char **name, char **value)
{
    char *equal; // position of first = sign in line

    equal = strchr (line, '=');
    if (equal && equal != line) {
        /* 1. name */
        // from first char to before =
        *name = remove_whitespace ((char*)line, equal-1);
        //printf ("      --name=[%s]\n", *name);
        /* 2. value */
        // from after = to the last character of line
        *value = remove_whitespace (equal+1, (char*)line+linelen-1);
        //printf ("      --value=[%s]\n", *value);

    } else if (equal != line) {
        /* check if it as name without = value statement */
        *name = remove_whitespace ((char*)line, (char*)line+linelen-1);
        //printf ("      --name only=[%s]\n", *name);
        *value = NULL;
    } else { 
        // funny text starting with =. E.g. "=value" 
        *name = NULL;
        *value = NULL;
    }
}

PairStruct * text_to_name_value_pairs (const char * text)
{
    /* Process a multi-line and/or ;-separated text and create a list
       of name=value pairs from each line which has a 
           name = value 
       pattern. Whitespaces are removed. 
         "X = 1
          Y = 2"  
       is not valid because of missing ';', but
          "X=1; Y=5;
          Z=apple"  
       is valid
    */
    char *name, *value; 
    char *item, *delim;
    int len;
    char line[256];
    PairStruct *res = NULL, *last = NULL, *pair;

    if (!text) return res;

    item  = (char *)text; 
    while (item) {
        delim = strchr (item, ';');
        if (delim) 
            len = (int) (delim-item); 
        else 
            len = strlen (item);

        strncpy (line, item, len);
        line[len] = '\0';

        //printf ("    --Line=[%s]\n", line);
        splitnamevalue(line, len, &name, &value);
        if (name) {
            pair = (PairStruct *) malloc (sizeof(PairStruct));
            pair->name = name;
            pair->value = value;
            pair->next = NULL;
            if (last) {
                last->next = pair;
                last = pair;
            } else {
                res = pair; 
                last = pair;
            }
        }
        if (delim && delim+1 != 0)
            item = delim+1;
        else
            item = NULL;
    }
    return res;
}


void free_name_value_pairs (PairStruct * pairs)
{
    PairStruct *p;
    while (pairs) {
        free(pairs->name);
        free(pairs->value);
        p = pairs;
        pairs=pairs->next;
        free(p);
    }
}


/*******************************************************
   Timing
**********************************************************/
#include <time.h> // nanosleep 
void adios_nanosleep (int sec, int nanosec)
{
#if HAVE_NANOSLEEP
    struct timespec treq = {.tv_sec=sec, .tv_nsec=nanosec};
    struct timespec trem;
    int r;
    r = nanosleep(&treq, &trem);
    //log_debug("adios_nanosleep: Nanoslept for %d.%9.9d sec, r=%d, errno=%d\n",
    //          treq.tv_sec, treq.tv_nsec, r, errno);
    while (r == -1 && errno == EINTR) {
        treq.tv_sec = trem.tv_sec;
        treq.tv_nsec = trem.tv_nsec;
        r = nanosleep (&treq, &trem);
    }
#else
    if (sec>0) {
        //log_debug("adios_nanosleep: Slept for %d seconds\n");
        sleep(sec);
    } else {
        //log_debug("adios_nanosleep: Slept for 1 second\n");
        sleep(1);
    }

#endif
}   

#include <sys/time.h>
struct timeval adios_timer_tp;
double adios_gettime() 
{
    gettimeofday(&adios_timer_tp, NULL); \
        return  ((double)adios_timer_tp.tv_sec + ((double)adios_timer_tp.tv_usec)/1000000.0);
}

void * bufdup(const void *buf, uint64_t elem_size, uint64_t count) {
    const uint64_t len = elem_size * count;
    void *newbuf = malloc(len);
    memcpy(newbuf, buf, len);
    return newbuf;
}
