////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013, Intel Corporation
// Copyright (c) 2014, Jacob Nelson
//
// Redistribution and use in source and binary forms, with or without 
// modification, are permitted provided that the following conditions 
// are met:
//
//     * Redistributions of source code must retain the above copyright 
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above 
//       copyright notice, this list of conditions and the following 
//       disclaimer in the documentation and/or other materials provided 
//       with the distribution.
//     * Neither the name of Intel Corporation nor the names of its 
//       contributors may be used to endorse or promote products 
//       derived from this software without specific prior written 
//       permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
// POSSIBILITY OF SUCH DAMAGE.
///////////////////////////////////////////////////////////////////////
 
#include <Grappa.hpp>
#include <FullEmpty.hpp>
 
using namespace Grappa;
 
#define ARRAY(i,j) (local[(i)+((j)*segment_size)])
#define ABS(x) ((x)>0 ? (x) : (-(x)))
#define root 0
 
double *local;
int start, end, segment_size;
FullEmpty<double> *lefts;
 
struct Timer {
  double start;
  double total;
} GRAPPA_BLOCK_ALIGNED;
 
int main( int argc, char * argv[] ) {
 
  int iterations;
  int m, n;
 
  Grappa::init( &argc, &argv );
 
  if( argc != 4 && argc !=5 ) {
    if( Grappa::mycore() == root ) 
      std::cout <<"Usage: " << argv[0] << 
                " <#iterations> <1st array dimension> <2nd array dimension>" << std::endl;
    exit(1);
  }
 
  iterations = atoi(argv[1]);
  if (iterations < 1){
    if( Grappa::mycore() == root ) 
      printf("ERROR: iterations must be >= 1 : %d \n",iterations);
    exit(1);
  } 
 
  m = atoi(argv[2]);
  n = atoi(argv[3]);
  if (m < 1 || n < 1){
    if( Grappa::mycore() == root )
      std::cout <<"ERROR: grid dimensions must be positive: "<<m<<","<<n<< std::endl;
    exit(1);
  }
 
  Grappa::run([iterations,m,n]{
 
    double avgtime, iter_time;
    int Num_procs = Grappa::cores();
    double epsilon = 1.e-8;
 
    if (m<Num_procs+1) {
      std::cout <<"ERROR: First grid dimension "<<m<<" smaller than #cores+1 "<<std::endl;
      exit(1);
    }
    std::cout<<"Grappa pipeline execution on 2D grid"<<std::endl;
    std::cout<<"Number of processes            = "<<Num_procs<<std::endl;
    std::cout<<"Grid sizes                     = "<<m<<"x"<<n<<std::endl;
    std::cout<<"Number of iterations           = "<<iterations<<std::endl;
    
    Grappa::on_all_cores( [m,n,Num_procs] {

      int my_ID = Grappa::mycore(), leftover;
      long total_length;
 
      lefts = new Grappa::FullEmpty<double>[n];
      if (!lefts) {
        std::cout<<"ERROR: core "<<my_ID<<" could not allocate flags"<<std::endl;
      }
 
      segment_size = m/Num_procs;
      leftover     = m%Num_procs;
      if (my_ID < leftover) {
        start = (segment_size+1)* my_ID;
        end   = start + segment_size;
      }
      else {
        start = (segment_size+1) * leftover + segment_size * (my_ID-leftover);
        end   = start + segment_size -1;
      }
 
      // now set segment_size to the value needed by the calling core
      segment_size = end - start + 1;
 
      total_length = segment_size*n;
      if (total_length/segment_size != n) {
        if (my_ID == root) {
          std::cout<<"ERROR: Grid of "<<m<<" by "<<n<<" points too large"<<std::endl;
        }
        exit(1);
      }
      local = new double[total_length];
      if (!local) {
        std::cout<<"ERROR: core "<<my_ID<<" could not allocate "
                                 <<total_length<<" words"<<std::endl;
        exit(1);
      }
 
      // clear the array                                                           
      for (int j=0; j<n; j++) for (int i=start; i<=end; i++) {
        ARRAY(i-start,j) = 0.0;
      }
      // set boundary values (bottom and left side of grid 
      if (my_ID==root) for (int j=0; j<n; j++) ARRAY(0,j) = (double) j;
      for (int i=start; i<=end; i++)      ARRAY(i-start,0) = (double) i;
 
      if (my_ID == root) {
        for( int j = 0; j < n; j++ ) {
          lefts[j].writeXF(j);
        } 
      }
      else {
        lefts[0].writeXF(start - 1); // one to the left of our first element
        for (int j = 1; j < n; j++ ) {
          lefts[j].reset();
        }
      }
 
      // redefine start and end for calling process to reflect local indices          
      if (my_ID==root) start = 1; 
      else             start = 0;
      end = segment_size-1;
 
    } );
      
    GlobalAddress<Timer> timer = Grappa::symmetric_global_alloc<Timer>();
 
    for (int iter = 0; iter <= iterations; ++iter ) {
 
      Grappa::on_all_cores( [timer,iter,n] {
        for( int j = 1; j < n; j++ ) {
          if( Grappa::mycore() != root) {
                lefts[j].reset();
          }
        }
        if (iter==1) timer->start = Grappa::walltime();
      } );
        
      // execute kernel
 
      Grappa::finish( [m,n] {
        Grappa::on_all_cores( [m,n] {
          double left, diag, up, current;
          int my_ID = Grappa::mycore();
 
          for(int j=1; j<n; j++) {
 
            // prepare to iterate over this segment      
            left = readFF( &lefts[j] );
            diag = readFF( &lefts[j-1] );
 
            for (int i=start; i<= end; i++) {
              // compute this cell's value
              up = ARRAY(i,j-1);
              current = up + left - diag;
              diag = up;
              left = current;
              ARRAY(i,j) = current;
            }
 
            // if we're at the end of a segment, write to corresponding full bit
            if(my_ID < Grappa::cores()-1 ) {
              Grappa::delegate::call<async>( my_ID+1, [=] () {
                                       writeXF( &lefts[j], current );
              } );
            }
          }
 
          // store top right corner value in a location to be read by the root core
          if (my_ID==Grappa::cores()-1) {
            Grappa::delegate::call<async>(root, [=] () {
                writeXF(&lefts[0], -1.0*current);
            } );
          }
        } );
      } );
 
    } // done with all iterations 
 
 
    Grappa::on_all_cores ( [timer] {
      Grappa::barrier();      
      timer->total = Grappa::walltime() - timer->start;
    });
 
    // verify result
    double expected_corner_val = (double) (iterations+1) * ( m + n - 2 );
    double actual_corner_val = -1.0*readFF(&lefts[0]);
    if (ABS(expected_corner_val-actual_corner_val) >= epsilon) {
      std::cout<<"ERROR: checksum "<<actual_corner_val<<
        "  does not match verification value "<<expected_corner_val<<std::endl;
    }
    else {
      iter_time = Grappa::reduce<double,collective_max<double>>( &timer->total );
      avgtime = iter_time/iterations;
      std::cout << "Solution validates"<<std::endl;
      std::cout << "Rate (MFlops/s): " << 1.0E-06*2*((double)(m-1)*(n-1))/avgtime<<
	"  Avg time (s): "<<avgtime<<std::endl;
    }
 
  });
  Grappa::finalize();
  return 0;
}
