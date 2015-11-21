/*
Copyright (c) 2013, Intel Corporation
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
* Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
* Neither the name of Intel Corporation nor the names of its
      contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/
/*******************************************************************
NAME:    Stencil
PURPOSE: This program tests the efficiency with which a space-invariant,
         linear, symmetric filter (stencil) can be applied to a square
         grid or image.
 
USAGE:   The program takes as input the linear dimension of the grid,
         and the number of iterations on the grid
               <progname> <# iterations> <grid size>
 
         The output consists of diagnostics to make sure the
         algorithm worked, and of timing statistics.
FUNCTIONS CALLED:
         Other than MPI or standard C functions, the following
         functions are used in this program:
         wtime()
         bail_out()
HISTORY: - Written by Rob Van der Wijngaart, November 2006.
         - RvdW, August 2013: Removed unrolling pragmas for clarity;
           fixed bug in compuation of width of strip assigned to
           each rank;
         - RvdW, August 2013: added constant to array "in" at end of
           each iteration to force refreshing of neighbor data in
           parallel versions
         - RvdW, October 2014: introduced 2D domain decomposition
         - RvdW, October 2014: removed barrier at start of each iteration
         - RvdW, October 2014: replaced single rank/single iteration timing
           with global timing of all iterations across all ranks
 
*********************************************************************************/
#include <par-res-kern_general.h>
#include <par-res-kern_mpi.h>
#include <Grappa.hpp>
#include <FullEmpty.hpp>
using namespace Grappa;
#ifndef RADIUS
  #define RADIUS 2
#endif
#define DOUBLE
#define STAR
#ifdef DOUBLE
  #define DTYPE     double
  #define MPI_DTYPE MPI_DOUBLE
  #define EPSILON   1.e-8
  #define COEFX     1.0
  #define COEFY     1.0
  #define FSTR      "%lf"
#else
  #define DTYPE     float
  #define MPI_DTYPE MPI_FLOAT
  #define EPSILON   0.0001f
  #define COEFX     1.0f
  #define COEFY     1.0f
  #define FSTR      "%f"
#endif
#define root 0

#define FOCUS 1
 
// temporary hack to allocate symmetric data with lower overhead
#define symmetric static
 
// define shorthand for indexing multi-dimensional arrays with offsets          
#define INDEXIN(i,j)  (i+RADIUS+(j+RADIUS)*(width+2*RADIUS))
// need to add offset of RADIUS to j to account for ghost points                
#define IN(i,j)       in[INDEXIN(i-istart,j-jstart)]
#define INDEXOUT(i,j) (i+(j)*(width))
#define OUT(i,j)      out[INDEXOUT(i-istart,j-jstart)]
#define WEIGHT(ii,jj) weight[ii+RADIUS][jj+RADIUS]
 
 
int main(int argc, char * argv[]) {
  //    Num_procs        = number of Grappa cores                             
  //    Num_procsx/y     = number of Grappa cores in respective coordinate directions
  //    my_ID            = Grappa core ID                                      
  //    my_IDx, my_IDy   = coordinates of Grappa core in core grid            
  //    right_nbr        = global ID of right neighboring core                
  //    left_nbr         = global ID of left neighboring core                  
  //    top_nbr          = global ID of top neighboring core                  
  //    bottom_nbr       = global ID of bottom neighboring core               
  //    root = 0
  //    n, width, height = linear global and local grid dimension              
  //    nsquare          = total number of grid points                        
  //    i, j, ii, jj, kk, it, jt, iter, leftover  = dummies                  
  //    istart, iend     = bounds of grid tile assigned to calling core    
  //    jstart, jend     = bounds of grid tile assigned to calling core    
  //    norm             = L1 norm of solution                                
  //    local_norm       = contribution of calling core to L1 norm         
  //    reference_norm
  //    f_active_points  = interior of grid with respect to stencil           
  //    flops            = floating point ops per iteration                   
  //    iterations;      = number of times to run the algorithm               
  //    local_stencil_time = timing parameters                                
  //    stencil_time,
  //    avgtime
  //    stencil_size     = number of points in stencil                        
  //    in               = input grid values                                   
  //    out              = output grid values                                 
  //    total_length_in  = total required length to store input array         
  //    total_length_out = total required length to store output array        
  //    error            = error flag                                         
  //    weight           = weights of points in the stencil
  Grappa::init( &argc, &argv );
  symmetric int my_ID = Grappa::mycore();
 
  /*******************************************************************************
  ** process and test input parameters   
  ********************************************************************************/
#ifndef STAR
  if (my_ID == root)
    std::cout <<"ERROR: Compact stencil not supported"<<std::endl;
  exit(1);     
#endif
   
  if (argc != 3){
    if (my_ID == root)
      std::cout<<"Usage:"<<argv[0]<<" <# iterations> <array dimension>"<<std::endl;
    exit(1);
  }
  int iterations  = atoi(argv[1]);
  if (iterations < 1){
    if (my_ID == root)
      std::cout<<"ERROR: iterations must be >= 1 :"<<iterations<<std::endl;
    exit(1);
  }
  int n       = atoi(argv[2]);
  long nsquare = n * n;
  if (nsquare < Grappa::cores()){
    if (my_ID == root)
      std::cout<<"ERROR: grid size "<<nsquare<<" must be at least # cores "<<
        Grappa::cores()<<std::endl;
    exit(1);
  }
  Grappa::run([iterations,n]{
    int Num_procsx, Num_procsy;
    int Num_procs=Grappa::cores();
    if (RADIUS < 0) {
      std::cout<<"ERROR: Stencil radius "<<RADIUS<<" should be non-negative"<<std::endl;
      exit(1);
    }
    if (2*RADIUS +1 > n) {
      std::cout<<"ERROR: Stencil radius "<<RADIUS<<" exceeds grid size "<<n<<std::endl;
      exit(1);
    }
    // determine best way to create a 2D grid of ranks (closest to square, for
    // best surface/volume ratio); we do this brute force for now
    for (Num_procsx=(int) (sqrt(Num_procs+1)); Num_procsx>0; Num_procsx--) {
      if (!(Num_procs%Num_procsx)) {
        Num_procsy = Num_procs/Num_procsx;
        break;
      }
    }     
    symmetric int my_IDx;
    symmetric int my_IDy;
    on_all_cores( [Num_procsx] {
        my_IDx = my_ID%Num_procsx;
        my_IDy = my_ID/Num_procsx; }
      );
 
    std::cout<<"Grappa stencil execution on 2D grid"<<std::endl;
    std::cout<<"Number of cores        = "<<Num_procs<<std::endl;
    std::cout<<"Grid size              = "<<n<<std::endl;
    std::cout<<"Radius of stencil      = "<<RADIUS<<std::endl;
    std::cout<<"Tiles in x/y-direction = "<<Num_procsx<<"/"<<Num_procsy<<std::endl;
    std::cout<<"Type of stencil        = star"<<std::endl;
#ifdef DOUBLE
      std::cout<<"Data type              = double precision"<<std::endl;
#else
      std::cout<<"Data type              = single precision"<<std::endl;
#endif
    std::cout<<"Number of iterations   = "<<iterations<<std::endl;

    symmetric double start;
    symmetric double total;
    symmetric int istart;
    symmetric int iend;
    symmetric int jstart;
    symmetric int jend;
    symmetric int width;
    symmetric int height;
 
    symmetric FullEmpty<DTYPE> * left_halo;
    symmetric FullEmpty<DTYPE> * right_halo;
    symmetric FullEmpty<DTYPE> * top_halo;
    symmetric FullEmpty<DTYPE> * bottom_halo;
 
    symmetric DTYPE * in;
    symmetric DTYPE * out;
 
    symmetric DTYPE weight[2*RADIUS+1][2*RADIUS+1];
 
    Grappa::on_all_cores( [n,Num_procs,Num_procsx,Num_procsy]{
      // compute amount of space required for input and solution arrays              
      width = n/Num_procsx;
      int leftover = n%Num_procsx;
      if (my_IDx<leftover) {
        istart = (width+1) * my_IDx;
        iend = istart + width;
      }
      else {
        istart = (width+1) * leftover + width * (my_IDx-leftover);
        iend = istart + width - 1;
      }
 
      width = iend - istart + 1;
      if (width == 0) {
        std::cout<<"ERROR: core "<<my_ID<<" has no work to do"<<std::endl;
        exit(1);
      }
      height = n/Num_procsy;
      leftover = n%Num_procsy;
     if (my_IDy<leftover) {
        jstart = (height+1) * my_IDy;
        jend = jstart + height;
      }
      else {
        jstart = (height+1) * leftover + height * (my_IDy-leftover);
        jend = jstart + height - 1;
      }
 
      height = jend - jstart + 1;
      if (height == 0) {
        printf("ERROR: core %d has no work to do\n", my_ID);
        exit(1);
      }
      if (width < RADIUS || height < RADIUS) {
        std::cout<<"ERROR: core "<<my_ID<<" has no work to do"<<std::endl;
        exit(1);
      }
      long total_length_in = (width+2*RADIUS)*(height+2*RADIUS);
      if (total_length_in/(height+2*RADIUS) != (width+2*RADIUS)) {
        std::cout<<"ERROR: Space for "<<width+2*RADIUS<<" x "<<height+2*RADIUS<<
          " input array cannot be represented"<<std::endl;
        exit(1);
      }
      long total_length_out = width*height;
      in  = Grappa::locale_new_array<DTYPE>(total_length_in);
      out = Grappa::locale_new_array<DTYPE>(total_length_out);
      if (!in || !out) {
        std::cout<<"ERROR: core "<<my_ID<<
          " could not allocate space for input/output array"<<std::endl;
        exit(1);
      }
      // fill the stencil weights to reflect a discrete divergence operator       
      for (int jj=-RADIUS; jj<=RADIUS; jj++) for (int ii=-RADIUS; ii<=RADIUS; ii++)
        WEIGHT(ii,jj) = (DTYPE) 0.0;
      for (int ii=1; ii<=RADIUS; ii++) {
        WEIGHT(0, ii) = WEIGHT( ii,0) =  (DTYPE) (1.0/(2.0*ii*RADIUS));
        WEIGHT(0,-ii) = WEIGHT(-ii,0) = -(DTYPE) (1.0/(2.0*ii*RADIUS));
      }
      // intialize the input and output arrays                                  
      for (int j=jstart; j<=jend; j++) for (int i=istart; i<=iend; i++) {
          IN(i,j)  = COEFX*i+COEFY*j;
          OUT(i,j) = (DTYPE)0.0;
      }
      // allocate communication buffers for halo values                         
      top_halo    = Grappa::locale_new_array<Grappa::FullEmpty<DTYPE>>(RADIUS*width);
      bottom_halo = Grappa::locale_new_array<Grappa::FullEmpty<DTYPE>>(RADIUS*width);
      right_halo  = Grappa::locale_new_array<Grappa::FullEmpty<DTYPE>>(RADIUS*height);
      left_halo   = Grappa::locale_new_array<Grappa::FullEmpty<DTYPE>>(RADIUS*height);
      if (!top_halo || !bottom_halo || !right_halo || !left_halo) {
        std::cout<<"ERROR: Rank "<<my_ID<<" could not allocate communication buffers"<<std::endl;
        exit(1);
      }
      // initialize the halos
      for (int i=0; i<RADIUS*width; i++) {
        top_halo[i].reset();
        bottom_halo[i].reset();
      }
      for (int i=0; i<RADIUS*height; i++) {
        right_halo[i].reset();
        left_halo[i].reset();
      }
    } );
  
    for (int iter = 0; iter<=iterations; iter++){
      Grappa::on_all_cores( [iter,n] {
        if (iter==1) start = Grappa::walltime();
      } );
       
      // execute kernel
      Grappa::finish( [n,Num_procsx,Num_procsy] {
        Grappa::on_all_cores( [n,Num_procsx,Num_procsy] {
          int i, j, ii, jj, kk;

          // compute neighbors; don't worry about dropping off the edges of the grid
          int right_nbr  = my_ID+1;
          int left_nbr   = my_ID-1;
          int top_nbr    = my_ID+Num_procsx;
          int bottom_nbr = my_ID-Num_procsx;
          // plop ghost point values in neighbors' halos
          if (my_IDy < Num_procsy-1)
            for (kk=0,j=jend-RADIUS+1; j<=jend; j++)
            for (i=istart; i<=iend; i++,kk++) {
              auto val = IN(i,j);
              Grappa::delegate::call<async>( top_nbr, [=] () {
                writeXF( &bottom_halo[kk], val);
              } );
            }
          if (my_IDy > 0)
            for (kk=0,j=jstart; j<=jstart+RADIUS-1; j++)
            for (i=istart; i<=iend; i++,kk++) {
              auto val = IN(i,j);
              Grappa::delegate::call<async>( bottom_nbr, [=] () {
                writeXF( &top_halo[kk], val);
              } );
            }
          if (my_IDx < Num_procsx-1)
            for (kk=0,j=jstart; j<=jend; j++)
              for (i=iend-RADIUS+1; i<=iend; i++,kk++) {
              auto val = IN(i,j);
              Grappa::delegate::call<async>( right_nbr, [=] () {
                writeXF( &left_halo[kk], val);
                } );
              }
          if (my_IDx > 0)
            for (kk=0,j=jstart; j<=jend; j++)
              for (i=istart; i<=istart+RADIUS-1; i++,kk++) {
              auto val = IN(i,j);
              Grappa::delegate::call<async>( left_nbr, [=] () {
                writeXF( &right_halo[kk], val);
                } );
              }
          //Now put the halos into the regular array tile
          if (my_IDy < Num_procsy-1)
            for (kk=0,j=jend+1; j<=jend+RADIUS; j++)
            for (i=istart; i<=iend; i++,kk++) {
              IN(i,j) = readFE( &top_halo[kk]);
            }
          if (my_IDy > 0)
            for (kk=0,j=jstart-RADIUS; j<=jstart-1; j++)
            for (i=istart; i<=iend; i++,kk++) {
              IN(i,j) = readFE( &bottom_halo[kk]);
            }
          if (my_IDx < Num_procsx-1)
            for (kk=0,j=jstart; j<=jend; j++)
            for (i=iend+1; i<=iend+RADIUS; i++,kk++)
              IN(i,j) = readFE( &right_halo[kk]);
          if (my_IDx > 0)
            for (kk=0,j=jstart; j<=jend; j++) for (i=istart-RADIUS; i<=istart-1; i++,kk++)
              IN(i,j) = readFE( &left_halo[kk]);
 
          // Apply the stencil operator
          for (j=MAX(jstart,RADIUS); j<=MIN(n-RADIUS-1,jend); j++) {
            for (i=MAX(istart,RADIUS); i<=MIN(n-RADIUS-1,iend); i++) {
              for (jj=-RADIUS; jj<=RADIUS; jj++) {
                OUT(i,j) += WEIGHT(0,jj)*IN(i,j+jj);
              }
              for (ii=-RADIUS; ii<0; ii++) {
                OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
              }
              for (ii=1; ii<=RADIUS; ii++) {
                OUT(i,j) += WEIGHT(ii,0)*IN(i+ii,j);
              }
            }
          }
          // add constant to solution to force refresh of neighbor data, if any
          for (j=jstart; j<=jend; j++) for (i=istart; i<=iend; i++) IN(i,j)+= 1.0;
        } );
      } );       
 
    } // end of iterations                                                   */
 
    symmetric DTYPE local_norm;
    
    Grappa::on_all_cores ( [n] {
      int my_ID=Grappa::mycore();
      total = Grappa::walltime() - start;
      local_norm = (DTYPE) 0.0;
      // compute L1 norm in parallel                                                */
      for (int j=MAX(jstart,RADIUS); j<=MIN(n-RADIUS-1,jend); j++) {
        for (int i=MAX(istart,RADIUS); i<=MIN(n-RADIUS-1,iend); i++) {
          local_norm += (DTYPE)ABS(OUT(i,j));
        }
      }
    });
    // verify result
    double reference_norm = (DTYPE) (iterations+1) * (COEFX + COEFY);
    double actual_norm = Grappa::reduce<DTYPE,collective_sum<DTYPE>>(&local_norm );
    double f_active_points = (DTYPE) (n-2*RADIUS)*(DTYPE) (n-2*RADIUS);
    actual_norm /= f_active_points;
    if (ABS(reference_norm-actual_norm) >= EPSILON) {
      std::cout<<"ERROR: checksum "<<actual_norm<<
        "  does not match verification value "<<reference_norm<<std::endl;
    }
    else {
      double iter_time = Grappa::reduce<double,collective_max<double>>( &total );
      // flops/stencil: 2 flops (fma) for each point in the stencil,
      // plus one flop for the update of the input of the array       
      int stencil_size = 4*RADIUS+1;
      double flops = (DTYPE) (2*stencil_size+1) * f_active_points;
      double avgtime = iter_time/iterations;
      std::cout << "Solution validates"<<std::endl;
      std::cout << "Rate (MFlops/s): " << 1.0E-06*flops/avgtime<<
        "  Avg time (s): "<<avgtime<<std::endl;
    }
  });

  Grappa::finalize();
  return 0;
}
