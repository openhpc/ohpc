/* This is part of the netCDF package.
   Copyright 2006 University Corporation for Atmospheric Research/Unidata.
   See COPYRIGHT file for conditions of use.

   This is a very simple example which reads a 2D array of
   sample data produced by simple_xy_wr.cpp.

   This example is part of the netCDF tutorial:
   http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

   Full documentation of the netCDF C++ API can be found at:
   http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-cxx

   $Id: simple_xy_rd.cpp,v 1.5 2010/02/11 22:36:43 russ Exp $
*/

#include <iostream>
#include <netcdf>
using namespace std;
using namespace netCDF;
using namespace netCDF::exceptions;

// We are reading 2D data, a 6 x 12 grid. 
static const int NX = 6;
static const int NY = 12;

// Return this in event of a problem.
static const int NC_ERR = 2;

int main()
{
   try
   {
   // This is the array we will read.
   int dataIn[NX][NY]; 

   // Open the file for read access
   NcFile dataFile("simple_xy.nc", NcFile::read);

   // Retrieve the variable named "data"
   NcVar data=dataFile.getVar("data");
   if(data.isNull()) return NC_ERR;
   data.getVar(dataIn);

   // Check the values. 
   for (int i = 0; i < NX; i++)
      for (int j = 0; j < NY; j++)
	 if (dataIn[i][j] != i * NY + j)
	   return NC_ERR;
   
   // The netCDF file is automatically closed by the NcFile destructor
   //cout << "*** SUCCESS reading example file simple_xy.nc!" << endl;

   return 0;
   }catch(NcException& e)
     {
       e.what();
       cout<<"FAILURE*************************************"<<endl;
       return NC_ERR;
     }
}
