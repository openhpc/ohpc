/*
 Copyright 2004-2006, UCAR/Unidata
 See COPYRIGHT file for copying and redistribution conditions.

 This program (quickly, but not throughly) tests the large file
 features. It turns off fill mode to quickly create an 8 gb file, and
 write one value is written, nothing is read.

 $Id: quick_large_files.c,v 1.18 2008/06/05 15:10:16 ed Exp $
*/
#include <config.h>
#include <nc_tests.h>
#include <netcdf.h>
#include <stdio.h>
#include <string.h>

#define NUMDIMS 1
#define NUMVARS 2

/* This is the magic number for classic format limits: 2 GiB - 4
   bytes. */
#define MAX_CLASSIC_BYTES 2147483644

/* This is the magic number for 64-bit offset format limits: 4 GiB - 4
   bytes. */
#define MAX_64OFFSET_BYTES 4294967292

/* Handy for constucting tests. */
#define QTR_CLASSIC_MAX (MAX_CLASSIC_BYTES/4)

/* We will create this file. */
#define FILE_NAME "quick_large_files.nc"

int
main(int argc, char **argv)
{
    int ncid, spockid, kirkid, dimids[NUMDIMS], recdimid, scottyid;
    int int_val_in, int_val_out = 99;
    double double_val_in, double_val_out = 1.79769313486230e+308; /* from ncx.h */
    size_t index[2] = {QTR_CLASSIC_MAX-1, 0};
    char file_name[NC_MAX_NAME + 1];

    /* These are for the revolutionary generals tests. */
    int cromwellid, collinsid, washingtonid;
    int napoleanid, dimids_gen[4], dimids_gen1[4];

    /* All create modes will be anded to this. All tests will be run
       twice, with and without NC_SHARE.*/
    int cmode_run;
    int cflag = NC_CLOBBER;

    int res; 

    printf("\n*** Testing large files, quickly.\n");

    for (cmode_run=0; cmode_run<2; cmode_run++)
    {
	size_t big_index = (size_t)MAX_CLASSIC_BYTES + 10;
 	/* On second pass, try using NC_SHARE. */
	if (cmode_run == 1) 
	{
	    cflag |= NC_SHARE;
	    printf("*** Turned on NC_SHARE for subsequent tests.\n");
	}

	/* Create a netCDF 64-bit offset format file. Write a value. */
	sprintf(file_name, "%s/%s", TEMP_LARGE, FILE_NAME); 
	printf("*** Creating %s for 64-bit offset large file test...", file_name);
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;

	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "longdim", QTR_CLASSIC_MAX, dimids)))
	    ERR;
	/* test bug fix writing record beyond 2**31 */
	if ((res = nc_def_dim(ncid, "longerdim", NC_UNLIMITED, &recdimid)))
	    ERR;
	if ((res = nc_def_var(ncid, "spock", NC_DOUBLE, NUMDIMS, 
			      dimids, &spockid)))
	    ERR;
	if ((res = nc_def_var(ncid, "kirk", NC_DOUBLE, NUMDIMS, 
			      dimids, &kirkid)))
	    ERR;
	if ((res = nc_def_var(ncid, "scotty", NC_BYTE, 1, 
			      &recdimid, &scottyid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	if ((res = nc_put_var1_double(ncid, kirkid, index, &double_val_out)))
	    ERR;
	if ((res = nc_put_var1_int(ncid, scottyid, &big_index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, scottyid, &big_index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out) 
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* How about a meteorological data file about the weather
	   experience by various generals of revolutionary armies? 

	   This has 3 dims, 4 vars. The dimensions are such that this will
	   (just barely) not fit in a classic format file. The first three
	   vars are cromwell, 536870911 bytes, washington, 2*536870911
	   bytes, and napolean, 536870911 bytes. That's a grand total of
	   2147483644 bytes. Recall our magic limit for the combined size
	   of all fixed vars: 2 GiB - 4 bytes, or 2147483644. So you would
	   think these would exactly fit, unless you realized that
	   everything is rounded to a 4 byte boundary, so you need to add
	   some bytes for that (how many?), and that pushes us over the
	   limit.
      
	   We will create this file twice, once to ensure it succeeds (with
	   64-bit offset format), and once to make sure it fails (with
	   classic format). Then some variations to check record var
	   boundaries. 
	*/
	printf("*** Now a 64-bit offset, large file, fixed var test..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      QTR_CLASSIC_MAX, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      QTR_CLASSIC_MAX, &dimids_gen[1])))
	    ERR;
	if ((res = nc_def_dim(ncid, "ruthlessness", 100, &dimids_gen[2])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_BYTE, 1, &dimids_gen[0],
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 1, &dimids_gen[1], 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 1, &dimids_gen[0], 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, &dimids_gen[2], 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	printf("ok\n"); 

	/* Write a value or two just for fun. */
	/*index[0] = QTR_CLASSIC_MAX - 296;
	if ((res = nc_put_var1_int(ncid, napoleanid, index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, napoleanid, index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out)
	BAIL2;*/
	printf("*** Now writing some values..."); 
	index[0] = QTR_CLASSIC_MAX - 295;
	if ((res = nc_put_var1_int(ncid, napoleanid, index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, napoleanid, index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out)
	   ERR;

	index[0] = QTR_CLASSIC_MAX - 1;
	if ((res = nc_put_var1_int(ncid, napoleanid, index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, napoleanid, index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out)
	    ERR;

	index[0] = QTR_CLASSIC_MAX - 1;
	if ((res = nc_put_var1_int(ncid, washingtonid, index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, washingtonid, index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out)
	    ERR;

	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* This time it should fail, because we're trying to cram this into
	   a classic format file. nc_enddef will detect our violations and
	   give an error. We've*/
	printf("*** Now a classic file which will fail..."); 
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      QTR_CLASSIC_MAX, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      QTR_CLASSIC_MAX, &dimids_gen[1])))
	    ERR;
	if ((res = nc_def_dim(ncid, "ruthlessness", 100, &dimids_gen[2])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_BYTE, 1, &dimids_gen[0],
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 1, &dimids_gen[1], 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 1, &dimids_gen[0], 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, &dimids_gen[2], 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)) != NC_EVARSIZE)
	    ERR;
	if ((res = nc_close(ncid)) != NC_EVARSIZE)
	    ERR;
	printf("ok\n"); 

	/* This will create some max sized 64-bit offset format fixed vars. */
	printf("*** Now a 64-bit offset, simple fixed var create test..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      MAX_CLASSIC_BYTES, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_SHORT, 1, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_SHORT, 1, dimids_gen, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, dimids_gen, 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* This will exceed the 64-bit offset format limits for one of the
	   fixed vars. */
	printf("*** Now a 64-bit offset, over-sized file that will fail..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	/* max dim size is MAX_CLASSIC_BYTES. */
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      MAX_CLASSIC_BYTES, dimids_gen)))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_DOUBLE, 1, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 1, dimids_gen, 
			      &washingtonid)))
	    if ((res = nc_enddef(ncid)) != NC_EVARSIZE)
		ERR;
	if ((res = nc_close(ncid)) != NC_EVARSIZE)
	    ERR;
	printf("ok\n"); 

	/* Now let's see about record vars. First create a 64-bit offset
	   file with three rec variables, each with the same numbers as
	   defined above for the fixed var tests. This should all work. */
	printf("*** Now a 64-bit offset, record var file..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      QTR_CLASSIC_MAX, &dimids_gen[1])))
	    ERR;
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      QTR_CLASSIC_MAX, &dimids_gen[2])))
	    ERR;
	if ((res = nc_def_dim(ncid, "ruthlessness", 100, &dimids_gen[3])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_BYTE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 2, dimids_gen, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, &dimids_gen[2], 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* Now try this record file in classic format. It should fail and
	   the enddef. Too many bytes in the first record.*/
	printf("*** Now a classic file that's too big and will fail..."); 
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      QTR_CLASSIC_MAX, &dimids_gen[1])))
	    ERR;
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      QTR_CLASSIC_MAX, &dimids_gen[2])))
	    ERR;
	if ((res = nc_def_dim(ncid, "ruthlessness", 100, &dimids_gen[3])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_BYTE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 2, dimids_gen, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, &dimids_gen[2], 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)) != NC_EVARSIZE)
	    ERR;
	if ((res = nc_close(ncid)) != NC_EVARSIZE)
	    ERR;
	printf("ok\n"); 

	/* Now try this record file in classic format. It just barely
	   passes at the enddef. Almost, but not quite, too many bytes in
	   the first record. Since I'm adding a fixed variable (Collins), 
	   I don't get the last record size exemption. */ 
	printf("*** Now a classic file with recs and one fixed will fail..."); 
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor", 
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))
	    ERR;
	if ((res = nc_def_dim(ncid, "ruthlessness", 100, &dimids_gen[2])))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_BYTE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 1, &dimids_gen[2], 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* Try a classic file with several records, and the last record var
	   with a record size greater than our magic number of 2 GiB - 4
	   bytes. We'll start with just one oversized record var. This
	   should work. Cromwell has been changed to NC_DOUBLE, and that
	   increases his size to 2147483644 (the max dimension size) times
	   8, or about 16 GB per record. Zowie! (Mind you, Cromwell
	   certainly had a great deal of revolutionary fervor.)
	*/ 
	printf("*** Now a classic file with one large rec var..."); 
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor",  
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))  
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_DOUBLE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	index[0] = 0;
	index[1] = MAX_CLASSIC_BYTES - 1;
	if ((res = nc_put_var1_double(ncid, cromwellid, index, &double_val_out)))
	    ERR;
	if ((res = nc_get_var1_double(ncid, cromwellid, index, &double_val_in)))
	    ERR;
	if (double_val_in != double_val_out)
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 
   
	/* This is a classic format file with an extra-large last record
	   var. */
	printf("*** Now a classic file with extra-large last record var...") ;
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor",  
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))  
	    ERR;
	dimids_gen1[0] = dimids_gen[0];
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      5368, &dimids_gen1[1])))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen1, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 2, dimids_gen1, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 2, dimids_gen1, 
			      &collinsid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_DOUBLE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	index[0] = 0;
	index[1] = MAX_CLASSIC_BYTES - 1;
	if ((res = nc_put_var1_double(ncid, cromwellid, index, &double_val_out)))
	    ERR;
	if ((res = nc_get_var1_double(ncid, cromwellid, index, &double_val_in)))
	    ERR;
	if (double_val_in != double_val_out)
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 

	/* This is a classic format file with an extra-large second to last
	   record var. But this time it won't work, because the size
	   exemption only applies to the last record var. Note that one
	   dimension is small (5000). */
	printf("*** Now a classic file xtra-large 2nd to last var that will fail..."); 
	if ((res = nc_create(file_name, cflag, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor",  
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))  
	    ERR;
	dimids_gen1[0] = dimids_gen[0];
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      5000, &dimids_gen1[1])))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen1, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 2, dimids_gen1, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_DOUBLE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 2, dimids_gen1, 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)) != NC_EVARSIZE)
	    ERR;
	if ((res = nc_close(ncid)) != NC_EVARSIZE)
	    ERR;
	printf("ok\n"); 

	/* Now try an extra large second to last ver with 64-bit
	   offset. This won't work either, because the cromwell var is so
	   large. It exceeds the 4GiB - 4 byte per record limit for record
	   vars. */
	printf("*** Now a 64-bit offset file with too-large rec var that will fail..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor",  
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))  
	    ERR;
	dimids_gen1[0] = dimids_gen[0];
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      5368, &dimids_gen1[1])))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen1, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_BYTE, 2, dimids_gen1, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_DOUBLE, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 2, dimids_gen1, 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)) != NC_EVARSIZE)
	    ERR;
	if ((res = nc_close(ncid)) != NC_EVARSIZE)
	    ERR;
	printf("ok\n"); 

	/* A 64-bit offset record file that just fits... */
	printf("*** Now a 64 bit-offset file that just fits..."); 
	if ((res = nc_create(file_name, cflag|NC_64BIT_OFFSET, &ncid)))
	    ERR;
	if ((res = nc_set_fill(ncid, NC_NOFILL, NULL)))
	    ERR;
	if ((res = nc_def_dim(ncid, "political_trouble", 
			      NC_UNLIMITED, &dimids_gen[0])))
	    ERR;
	if ((res = nc_def_dim(ncid, "revolutionary_fervor",  
			      MAX_CLASSIC_BYTES, &dimids_gen[1])))  
	    ERR;
	dimids_gen1[0] = dimids_gen[0];
	if ((res = nc_def_dim(ncid, "post_revoultionary_hangover", 
			      MAX_CLASSIC_BYTES, &dimids_gen1[1])))
	    ERR;
	if ((res = nc_def_var(ncid, "Washington", NC_SHORT, 2, dimids_gen1, 
			      &washingtonid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Napolean", NC_SHORT, 2, dimids_gen1, 
			      &napoleanid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Cromwell", NC_SHORT, 2, dimids_gen,
			      &cromwellid)))
	    ERR;
	if ((res = nc_def_var(ncid, "Collins", NC_DOUBLE, 2, dimids_gen1, 
			      &collinsid)))
	    ERR;
	if ((res = nc_enddef(ncid)))
	    ERR;
	index[0] = 0;
	index[1] = MAX_CLASSIC_BYTES - 1;
	if ((res = nc_put_var1_int(ncid, cromwellid, index, &int_val_out)))
	    ERR;
	if ((res = nc_get_var1_int(ncid, cromwellid, index, &int_val_in)))
	    ERR;
	if (int_val_in != int_val_out)
	    ERR;
	if ((res = nc_close(ncid)))
	    ERR;
	printf("ok\n"); 
    } /* end of cmode run */

    /* Wow! Everything worked! */
    printf("*** Tests successful!\n"); 

    /* Delete the huge data file we created. */
    (void) remove(file_name); 

    return 0;
}






