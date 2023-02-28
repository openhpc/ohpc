/************************************************************

  This example shows a way to recursively traverse the file
  using H5Giterate.  The method shown here guarantees that
  the recursion will not enter an infinite loop, but does
  not prevent objects from being visited more than once.
  The program prints the directory structure of the file
  specified in FILE.  The default file used by this example
  implements the structure described in the User's Guide,
  chapter 4, figure 26.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE       "h5ex_g_traverse.h5"

/*
 * Define operator data structure type for H5Giterate callback.
 * During recursive iteration, these structures will form a
 * linked list that can be searched for duplicate groups,
 * preventing infinite recursion.
 */
struct opdata {
    unsigned        recurs;         /* recursion level.  0=root */
    struct opdata   *prev;          /* pointer to previous opdata */
    unsigned long   groupno[2];     /* unique group number */
};

/*
 * Operator function to be called by H5Giterate.
 */
herr_t op_func (hid_t loc_id, const char *name, void *operator_data);

/*
 * Function to check for duplicate groups in a path.
 */
int group_check (struct opdata *od, unsigned long target_groupno[2]);

int
main (void)
{
    hid_t           file;           /* Handle */
    herr_t          status;
    H5G_stat_t      statbuf;
    struct opdata   od;

    /*
     * Open file and initialize the operator data structure.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    status = H5Gget_objinfo (file, "/", 0, &statbuf);
    od.recurs = 0;
    od.prev = NULL;
    od.groupno[0] = statbuf.objno[0];
    od.groupno[1] = statbuf.objno[1];

    /*
     * Print the root group and formatting, begin iteration.
     */
    printf ("/ {\n");
    status = H5Giterate (file, "/", NULL, op_func, (void *) &od);
    printf ("}\n");

    /*
     * Close and release resources.
     */
    status = H5Fclose (file);

    return 0;
}


/************************************************************

  Operator function.  This function prints the name and type
  of the object passed to it.  If the object is a group, it
  is first checked against other groups in its path using
  the group_check function, then if it is not a duplicate,
  H5Giterate is called for that group.  This guarantees that
  the program will not enter infinite recursion due to a
  circular path in the file.

 ************************************************************/
herr_t op_func (hid_t loc_id, const char *name, void *operator_data)
{
    herr_t          status, return_val = 0;
    H5G_stat_t      statbuf;
    struct opdata   *od = (struct opdata *) operator_data;
                                /* Type conversion */
    unsigned        spaces = 2*(od->recurs+1);
                                /* Number of whitespaces to prepend
                                   to output */

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Gget_objinfo (loc_id, name, 0, &statbuf);
    printf ("%*s", spaces, "");     /* Format output */
    switch (statbuf.type) {
        case H5G_GROUP:
            printf ("Group: %s {\n", name);

            /*
             * Check group objno against linked list of operator
             * data structures.  Only necessary if there is more
             * than 1 link to the group.
             */
            if ( (statbuf.nlink > 1) && group_check (od, statbuf.objno) ) {
                printf ("%*s  Warning: Loop detected!\n", spaces, "");
            }
            else {

                /*
                 * Initialize new operator data structure and
                 * begin recursive iteration on the discovered
                 * group.  The new opdata structure is given a
                 * pointer to the current one.
                 */
                struct opdata nextod;
                nextod.recurs = od->recurs + 1;
                nextod.prev = od;
                nextod.groupno[0] = statbuf.objno[0];
                nextod.groupno[1] = statbuf.objno[1];
                return_val = H5Giterate (loc_id, name, NULL, op_func,
                            (void *) &nextod);
            }
            printf ("%*s}\n", spaces, "");
            break;
        case H5G_DATASET:
            printf ("Dataset: %s\n", name);
            break;
        case H5G_TYPE:
            printf ("Datatype: %s\n", name);
            break;
        default:
            printf ( "Unknown: %s\n", name);
    }

    return return_val;
}


/************************************************************

  This function recursively searches the linked list of
  opdata structures for one whose groupno field matches
  target_groupno.  Returns 1 if a match is found, and 0
  otherwise.

 ************************************************************/
int group_check (struct opdata *od, unsigned long target_groupno[2])
{
    if ( (od->groupno[0] == target_groupno[0]) &&
                (od->groupno[1] == target_groupno[1]) )
        return 1;       /* Group numbers match */
    else if (!od->recurs)
        return 0;       /* Root group reached with no matches */
    else
        return group_check (od->prev, target_groupno);
                        /* Recursively examine the next node */
}
