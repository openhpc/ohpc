/*********************************************************************
 *
 *  Copyright (C) 2014, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 *
 *********************************************************************/
/* $Id: nctst.cpp 2744 2016-12-28 16:25:22Z wkliao $ */

#include <stdio.h>
#include <stdlib.h>

#include <iostream>
using namespace std;

#include <string.h>
#include <libgen.h> /* basename() */
#include <pnetcdf>
#include <testutils.h>

using namespace PnetCDF;
using namespace PnetCDF::exceptions;

const char LAT[] = "lat";
const char LON[] = "lon";
const char FRTIME[] = "frtime";
const char TIMELEN1[] = "timelen";
const char P_NAME[] = "P";
const char PRES_MAX_WIND[] = "pressure at maximum wind";
const char LONG_NAME[] = "long_name";
const char UNITS[] = "units";
const char VALID_RANGE[] = "valid_range";
const char FILL_VALUE[] = "_FillValue";
const char DEGREES_NORTH[] = "degrees_north";
const char LONGITUDE[] = "longitude";
const char LATITUDE[] = "latitude";
const char HECTOPASCALS[] = "hectopascals";
const char DEGREES_EAST[] = "degrees_east";
const char HOURS[] = "hours";
const char FORECAST_TIME[] = "forecast time";
const char REFERENCE_TIME[] = "reference time";
const char REFTIME[] = "reftime";
const char TEXT_TIME[] = "text_time";
const char SCALARV[] = "scalarv";
const char SCALAR_ATT[] = "scalar_att";
const int SCALAR_VALUE = 1;
const char HISTORY[] = "history";
const char TITLE[] = "title";
const char HISTORY_STR[] = "created by Unidata LDM from NPS broadcast";
const char TITLE_STR[] = "NMC Global Product Set: Pressure at Maximum Wind";

const int NC_ERR = 2;
const int NLATS = 4;
const int NLONS = 3;
const int NFRTIMES = 2;
const int TIMESTRINGLEN = 20;
const MPI_Offset NRANGES = 2;

// These are data values written out by the gen() function, and read
// in again and checked by the read() function.
static float range[] = {0., 1500.};
static float lats[NLATS] = {-90, -87.5, -85, -82.5};
static float lons[NLONS] = {-180, -175, -170};
static int frtimes[NFRTIMES] = {12, 18};
static const char* s = "1992-3-21 12:00";
static float fill_value = -9999.0;
static float P_data[NFRTIMES][NLATS][NLONS] = {
   {{950, 951, 952}, {953, 954, 955}, {956, 957, 958}, {959, 960, 961}},
   {{962, 963, 964}, {965, 966, 967}, {968, 969, 970}, {971, 972, 973}}
};


// Check a string attribute to make sure it has the correct value.
int 
check_string_att(NcmpiAtt &att, const char *theName, const char *value)
{
   if (att.isNull() || att.getName().compare(theName) != 0 || 
       att.getType() != ncmpiChar || att.getAttLength() != (long)strlen(value))
      return NC_ERR;

   int err=0;
   char *value_in = (char*)malloc(strlen(value)+1);
   att.getValues(value_in);
   if (strncmp(value_in, value, strlen(value)))
      err = NC_ERR;
   free(value_in);

   return err;
}

// Check the units and long_name attributes of a var to make sure they
// are what is expected.
int
check_u_ln_atts(NcmpiVar &var, const char *units, const char *long_name)
{
   NcmpiVarAtt att = var.getAtt(UNITS);
   if (check_string_att(att, UNITS, units))
      return NC_ERR;

   att = var.getAtt(LONG_NAME);
   if (check_string_att(att, LONG_NAME, long_name))
      return NC_ERR;

   return 0;
}

// This reads the netCDF file created by gen() and ensures that
// everything is there as expected.
int read(const MPI_Comm        &comm,
         const char            *path,
         NcmpiFile::FileFormat  format)
{
    try {
        // open the file
        NcmpiFile nc(comm, path, NcmpiFile::read); 

        // Check the format.
        if (nc.getFormat() != format) {
            cout << "unexpected file format"<<endl;
            throw NcmpiException("read Error ",__FILE__,__LINE__);
        }

        // Check the number of dimensions.
        int ndims = nc.getDimCount();
        if (ndims != 4 && ndims != 6 && ndims != 2) {
            cout << "nc.getDimCount()="<<ndims<<" != 2, 4, 6"<<endl;
            throw NcmpiException("read Error ",__FILE__,__LINE__);
        }

        // Check the global attributes.
        NcmpiGroupAtt att = nc.getAtt(HISTORY);
        if (check_string_att(att, HISTORY, HISTORY_STR)) {
            cout << "global attribute "<<HISTORY_STR<<" != "<<HISTORY<<endl;
            throw NcmpiException("read Error ",__FILE__,__LINE__);
        }

        att = nc.getAtt(TITLE);
        if (check_string_att(att, TITLE, TITLE_STR)) {
            cout << "global attribute "<<TITLE_STR<<" != "<<TITLE<<endl;
            throw NcmpiException("read Error ",__FILE__,__LINE__);
        }
   
        // Check the dimensions.
        NcmpiDim latDim = nc.getDim(LAT);
        if (latDim.isNull() || latDim.getName().compare(LAT) != 0 || 
            latDim.getSize() != NLATS || latDim.isUnlimited())
            throw NcmpiException("read Error: dimension lat ",__FILE__,__LINE__);

        NcmpiDim lonDim = nc.getDim(LON);
        if (lonDim.isNull() || lonDim.getName().compare(LON) != 0 || 
            lonDim.getSize() != NLONS || lonDim.isUnlimited())
            throw NcmpiException("read Error: dimension lon ",__FILE__,__LINE__);

        NcmpiDim frtimeDim = nc.getDim(FRTIME);
        if (frtimeDim.isNull() || frtimeDim.getName().compare(FRTIME) != 0 || 
            frtimeDim.getSize() != NFRTIMES || !frtimeDim.isUnlimited())
            throw NcmpiException("read Error: unlimited dimension frtime ",__FILE__,__LINE__);

        NcmpiDim timeLenDim = nc.getDim(TIMELEN1);
        if (timeLenDim.isNull() || timeLenDim.getName().compare(TIMELEN1) != 0 || 
            timeLenDim.getSize() != TIMESTRINGLEN || timeLenDim.isUnlimited())
            throw NcmpiException("read Error: dimension timeLen ",__FILE__,__LINE__);

        // Check the coordinate variables.

        // Get the latitude.
        NcmpiVar latVar = nc.getVar(LAT);

        // Check units and long name.
        if (check_u_ln_atts(latVar, DEGREES_NORTH, LATITUDE))
            throw NcmpiException("read Error: check_u_ln_atts lat ",__FILE__,__LINE__);

        // Get the longitude.
        NcmpiVar lonVar = nc.getVar(LON);

        // Check units and long name.
        if (check_u_ln_atts(lonVar, DEGREES_EAST, LONGITUDE))
            throw NcmpiException("read Error: check_u_ln_atts lon ",__FILE__,__LINE__);

        // Get the forecast time coordinate variable.
        NcmpiVar frtimeVar = nc.getVar(FRTIME);

        // Check units and long name.
        if (check_u_ln_atts(frtimeVar, HOURS, FORECAST_TIME)) {
            cout << "check_u_ln_atts frtime  err"<<endl;
            throw NcmpiException("read Error ",__FILE__,__LINE__);
        }

        // Get the refTime coordinate variable.
        NcmpiVar refTimeVar = nc.getVar(REFTIME);

        // Check units and long name.
        if (check_u_ln_atts(refTimeVar, TEXT_TIME, REFERENCE_TIME))
            throw NcmpiException("read Error: check_u_ln_atts refTime ",__FILE__,__LINE__);

        // Check the data variables.
        NcmpiVar pVar = nc.getVar(P_NAME);

        // Check units and long name.
        if (check_u_ln_atts(pVar, HECTOPASCALS, PRES_MAX_WIND))
            throw NcmpiException("read Error: check_u_ln_atts HECTOPASCALS ",__FILE__,__LINE__);

        // Check the valid range, and check the values.
        NcmpiVarAtt vatt = pVar.getAtt(VALID_RANGE);
        if (vatt.isNull() || vatt.getName().compare(VALID_RANGE) != 0 || 
            vatt.getType() != ncmpiFloat || vatt.getAttLength() != NRANGES)
            throw NcmpiException("read Error: VALID_RANGE ",__FILE__,__LINE__);

        float range_in[NRANGES];
        vatt.getValues(range_in);
        if (range_in[0] != range[0] || range_in[1] != range[1])
            throw NcmpiException("read Error: range value ",__FILE__,__LINE__);

        // Check the fill value, and check the value.
        vatt = pVar.getAtt(FILL_VALUE);
        if (vatt.isNull() || vatt.getName().compare(FILL_VALUE) != 0 || 
            vatt.getType() != ncmpiFloat || vatt.getAttLength() != 1)
            throw NcmpiException("read Error: FILL_VALUE ",__FILE__,__LINE__);

        float fill_value_in;
        vatt.getValues(&fill_value_in);
        if (fill_value_in != fill_value)
            throw NcmpiException("Fill value mismatch Error ",__FILE__,__LINE__);

        // Check the data in the pressure variable.
        float P_data_in[NFRTIMES][NLATS][NLONS];
        // pVar.getVar(NFRTIMES, NLATS, NLONS, &P_data_in[0][0][0]);
        pVar.getVar_all(&P_data_in[0][0][0]);
        for (int f = 0; f < NFRTIMES; f++)
           for (int la = 0; la < NLATS; la++)
	      for (int lo = 0; lo < NLONS; lo++)
	         if (P_data_in[f][la][lo] != P_data[f][la][lo])
                     throw NcmpiException("read Error: unexpect variable value ",__FILE__,__LINE__);

        // Get the scalar variable.
        NcmpiVar scalarVar = nc.getVar(SCALARV);

        // Check for the scalar attribute of the scalar variable and check its value.
        vatt = scalarVar.getAtt(SCALAR_ATT);
        if (vatt.isNull() || vatt.getName().compare(SCALAR_ATT) != 0 || 
            vatt.getType() != ncmpiInt || vatt.getAttLength() != 1)
            throw NcmpiException("read Error: SCALAR_ATT ",__FILE__,__LINE__);

        int value_in;
        vatt.getValues(&value_in);
        if (value_in != SCALAR_VALUE)
            throw NcmpiException("read Error: SCALAR_VALUE ",__FILE__,__LINE__);

        // Check the value of the scalar variable.
    }
    catch(NcmpiException& e)
    {
      cout << e.what() << " error code=" << e.errorCode() << " Error!\n";
      return -1;
    }

   return 0;
}

// Generate a netCDF file
int gen(const MPI_Comm        &comm,
        const char            *path,
        NcmpiFile::FileFormat  format)
{
    try {
        // Create a new file, leave in define mode
        NcmpiFile nc(comm, path, NcmpiFile::replace, format);

        // Create dimensions
        NcmpiDim latd     = nc.addDim(LAT, NLATS);
        NcmpiDim lond     = nc.addDim(LON, NLONS);
        NcmpiDim frtimed  = nc.addDim(FRTIME); // unlimited dimension
        NcmpiDim timelend = nc.addDim(TIMELEN1, TIMESTRINGLEN); 

        vector<NcmpiDim> dim3D(3);
        dim3D[0]=frtimed;
        dim3D[1]=latd;
        dim3D[2]=lond;

        // Create variables and their attributes
        NcmpiVar P = nc.addVar(P_NAME, ncmpiFloat, dim3D);
        P.putAtt(LONG_NAME,   PRES_MAX_WIND);
        P.putAtt(UNITS,       HECTOPASCALS);
        P.putAtt(VALID_RANGE, ncmpiFloat, NRANGES, range);
        P.putAtt(FILL_VALUE,  ncmpiFloat, fill_value);

        NcmpiVar lat = nc.addVar(LAT, ncmpiFloat, latd);
        lat.putAtt(LONG_NAME, LATITUDE);
        lat.putAtt(UNITS, DEGREES_NORTH);

        NcmpiVar lon = nc.addVar(LON, ncmpiFloat, lond);
        lon.putAtt(LONG_NAME, LONGITUDE);
        lon.putAtt(UNITS, DEGREES_EAST);

        NcmpiVar frtime = nc.addVar(FRTIME, ncmpiInt, frtimed);
        frtime.putAtt(LONG_NAME, FORECAST_TIME);
        frtime.putAtt(UNITS, HOURS);

        NcmpiVar reftime = nc.addVar(REFTIME, ncmpiChar, timelend);
        reftime.putAtt(LONG_NAME, REFERENCE_TIME);
        reftime.putAtt(UNITS, TEXT_TIME);

        NcmpiVar scalar = nc.addVar(SCALARV, ncmpiInt);
        scalar.putAtt(SCALAR_ATT, ncmpiInt, SCALAR_VALUE);

        // Global attributes
        nc.putAtt(HISTORY, HISTORY_STR);
        nc.putAtt(TITLE, TITLE_STR);

        // Start writing data, implictly leaves define mode

        lat.putVar_all(lats);

        lon.putVar_all(lons);

        // make sure tailing characters are all '\0'
        char *str = (char*) calloc(TIMESTRINGLEN, 1);
        strcpy(str, s);
        reftime.putVar_all(str);
        free(str);

        // we write one record at a time
        vector<MPI_Offset> startp,countp;
        startp.push_back(0);
        startp.push_back(0);
        startp.push_back(0);
        countp.push_back(1);
        countp.push_back(NLATS);
        countp.push_back(NLONS);

        P.putVar_all(startp,countp,&P_data[0][0][0]); // write 1st record

        startp[0] = 1;
        P.putVar_all(startp,countp,&P_data[1][0][0]); // write 2nd record

        frtime.putVar_all(frtimes);

        // close of nc takes place in destructor
    }
    catch(NcmpiException& e)
    {
      cout << e.what() << " error code=" << e.errorCode() << " Error!\n";
      return -1;
    }

    return 0;
}

/*
 * Convert pathname of netcdf file into name for CDL, by taking last component
 * of path and stripping off any extension.  The returned string is in static
 * storage, so copy it if you need to keep it.
 */
static char* 
cdl_name(const char* path)
{
    const char* cp = path + strlen(path);
    while (*(cp-1) != '/' && cp != path) // assumes UNIX path separator
	cp--;

    static char np[NC_MAX_NAME];
    strncpy(&np[0], cp, NC_MAX_NAME);

    char* ep = np + strlen(np);
    while (*ep != '.' && ep != np)
	ep--;
    if (*ep == '.')
      *ep = '\0';
    return np;
}

// A derived class, just like NcmpiFile except knows how to "dump" its
// dimensions, variables, global attributes, and data in ASCII form.
class DumpableNcmpiFile : public NcmpiFile
{
  public:
    DumpableNcmpiFile(const MPI_Comm &comm,
                      const char* path, NcmpiFile::FileMode mode = read)
	: NcmpiFile(comm, path, mode) {} ;
    void dumpdims( void );
    void dumpvars( void );
    void dumpgatts( void );
    void dumpdata( void );
};

void DumpableNcmpiFile::dumpdims( void )
{
    multimap<string,NcmpiDim> dimMap = getDims();

    for (multimap<string,NcmpiDim>::iterator iter=dimMap.begin(); iter!=dimMap.end(); ++iter) {
        NcmpiDim *ptr = &(*iter).second;
	cout << "\t" << ptr->getName() << " = " ;
	if (ptr->isUnlimited())
	  cout << "UNLIMITED" << " ;\t " << "// " << ptr->getSize() <<
	    " currently\n";
	else
	  cout << ptr->getSize() << " ;\n";
    }
}

void dumpatts(NcmpiVar& var)
{
    std::map<std::string,NcmpiVarAtt> attMap = var.getAtts();

    for (std::map<std::string,NcmpiVarAtt>::iterator iter=attMap.begin(); iter!=attMap.end(); ++iter) {
        NcmpiAtt *ptr = &(*iter).second;
	cout << "\t\t" << var.getName() << ":" << ptr->getName() << " = " ;
        vector<double>  atest(ptr->getAttLength());
        ptr->getValues(&atest[0]);
	cout << &atest[0] << " ;" << endl ;
    }
}

void DumpableNcmpiFile::dumpvars( void )
{
    multimap<string,NcmpiVar> vMap = getVars();

    for (multimap<string,NcmpiVar>::iterator iter=vMap.begin(); iter!=vMap.end(); ++iter) {
        NcmpiVar *vp = &(*iter).second;
	cout << "\t" << vp->getType().getName() << " " << vp->getName() ;

	if (vp->getDimCount() > 0) {
	    cout << "(";
	    for (int d = 0; d < vp->getDimCount(); d++) {
		NcmpiDim dim = vp->getDim(d);
		cout << dim.getName();
		if (d < vp->getDimCount()-1)
		  cout << ", ";		  
	    }
	    cout << ")";
	}
	cout << " ;\n";
	// now dump each of this variable's attributes
	dumpatts(*vp);
    }
}

void DumpableNcmpiFile::dumpgatts( void )
{
    std::multimap<std::string,NcmpiGroupAtt> attMap = getAtts();

    for (std::multimap<std::string,NcmpiGroupAtt>::iterator iter=attMap.begin(); iter!=attMap.end(); ++iter) {
        NcmpiGroupAtt *ap = &(*iter).second;
        cout << "\t\t" << ":" << ap->getName() << " = " ;
        vector<double>  atest(ap->getAttLength());
        ap->getValues(&atest[0]);
	cout << &atest[0] << " ;" << endl ;
    }
}

void DumpableNcmpiFile::dumpdata( )
{
    std::multimap<std::string,NcmpiVar> vMap = getVars();

    for (std::multimap<std::string,NcmpiVar>::iterator iter=vMap.begin(); iter!=vMap.end(); ++iter) {
        NcmpiVar *vp = &(*iter).second;
	cout << " " << vp->getName() << " = ";

        MPI_Offset vSize = 1;
        std::vector<NcmpiDim> dimMap = vp->getDims();
        for (std::vector<NcmpiDim>::iterator iterD=dimMap.begin(); iterD!=dimMap.end(); ++iterD) {
            vSize *= iterD->getSize();
        }

        vector<double> vals(vSize);
        vp->getVar_all(&vals[0]);
	cout << &vals[0] << " ;" << endl ;
    }
}

void dump(const MPI_Comm &comm, const char* path)
{
    DumpableNcmpiFile nc(comm, path);	// default is open in read-only mode

    cout << "netcdf " << cdl_name(path) << " {" << endl <<
	    "dimensions:" << endl ;

    nc.dumpdims();

    cout << "variables:" << endl;

    nc.dumpvars();

    if (nc.getAttCount() > 0)
      cout << "// global attributes" << endl ;

    nc.dumpgatts();

    cout << "data:" << endl;

    nc.dumpdata();

    cout << "}" << endl;
}

/* Test everything for classic, 64-bit offset, an 64-bit data files. */
#define NUM_FORMATS (3)

int
main(int argc, char* argv[])	// test new netCDF interface
{
   char filename[256];
   int rank, nprocs;

   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

   if (argc > 2) {
       if (!rank) printf("Usage: %s [filename]\n",argv[0]);
       MPI_Finalize();
       return 0;
   }
   if (argc == 2) snprintf(filename, 256, "%s", argv[1]);
   else           strcpy(filename, "testfile.nc");

   if (rank == 0) {
       char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
       sprintf(cmd_str, "*** TESTING C++ %s for APIs with different netCDF formats ", basename(argv[0]));
       printf("%-66s ------ ", cmd_str);
       free(cmd_str);
   }

   // Set up the format constants.
   NcmpiFile::FileFormat format[NUM_FORMATS] =
              {NcmpiFile::classic, NcmpiFile::classic2, NcmpiFile::classic5};
#ifdef DEBUG
   char format_name[NUM_FORMATS][NC_MAX_NAME] = 
        {"classic", "classic2", "classic5"};
#endif

   int nerrs = 0;
   for (int i = 0; i < NUM_FORMATS; i++)
   {
      if (gen(MPI_COMM_WORLD, filename, format[i]) || 
	  read(MPI_COMM_WORLD, filename, format[i]))
      {
#ifdef DEBUG
	 cout << "*** FAILURE with format " << format_name[i] << "\n";
#endif
	 nerrs++;
      }
#ifdef DEBUG
      else
	 cout << "*** SUCCESS with format " << format_name[i] << "\n";
#endif
   }

#ifdef DEBUG
   cout << "\n*** Total number of failures: " << nerrs << "\n";
#endif

    MPI_Offset malloc_size, sum_size;
    int err = ncmpi_inq_malloc_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }

    if (rank == 0) {
        if (nerrs) printf(FAIL_STR,nerrs);
        else       printf(PASS_STR);
    }


   MPI_Finalize();
   return nerrs;
}
