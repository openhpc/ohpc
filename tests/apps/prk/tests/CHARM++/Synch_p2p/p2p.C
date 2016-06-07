#include "p2p.decl.h"
#include <par-res-kern_general.h>

#define EPSILON    1.e-8
#define ARRAY(i,j) vector[i+1+(j)*(width+1)]

/*readonly*/ CProxy_Main mainProxy;
/*readonly*/ int n; // array size
/*readonly*/ int m; // array size
/*readonly*/ int overdecomposition; 
/*readonly*/ int maxiterations;
/*readonly*/ int grp;

// specify the number of worker chares in each dimension
/*readonly*/ int num_chares;
/*readonly*/ double startTime, endTime;

class ghostMsg : public CMessage_ghostMsg {
public:
  double *gp;
  ghostMsg(){
  }
};

class cornerMsg : public CMessage_cornerMsg {
public:
  double *gp;
  cornerMsg(){
  }
};

class Main : public CBase_Main
{

public:
    CProxy_P2p array;

    Main(CkArgMsg* cmdlinearg) {
        if (cmdlinearg->argc != 5 && cmdlinearg->argc != 6 ) {
          CkPrintf("%s <#iterations> <grid_size x> <grid_size y> <overdecomposition factor> ",
          cmdlinearg->argv[0]);
          CkPrintf("[group factor]\n");
          CkExit();
        }

        // store the main proxy
        mainProxy = thisProxy;

        maxiterations = atoi(cmdlinearg->argv[1]);
        if (maxiterations < 1) {
          CkPrintf("ERROR: #iterations must be positive: %d", maxiterations);
          CkExit();
        }
        m = atoi(cmdlinearg->argv[2]);
        if (m < CkNumPes()) {
          CkPrintf("ERROR: Horizontal grid size %d smaller than #PEs %d\n", m, CkNumPes());
          CkExit();
        }

        n = atoi(cmdlinearg->argv[3]);
        if (n < 1) {
          CkPrintf("ERROR: Vertical grid size must be positive: %d\n", n);
          CkExit();
        }

        overdecomposition = atoi(cmdlinearg->argv[4]);
        if (overdecomposition<1) {
          CkPrintf("ERROR: Overdecomposition factor must be positive: %d\n", overdecomposition);
          CkExit();
        }

        if (cmdlinearg->argc==6) {
          grp = atoi(cmdlinearg->argv[5]);
          if (grp < 1) grp = 1;
          else if (grp >= n) grp = n-1;
        }
        else grp = 1;

        num_chares = CkNumPes()*overdecomposition;

        if ((m-1)< num_chares) {
          CkPrintf("ERROR: Interior horizontal grid size %d smaller than #chares %d\n",
		   m-1, num_chares);
          CkExit();
        }

        // print info
        CkPrintf("Charm++ pipeline execution on 2D grid\n");
        CkPrintf("Number of Charm++ PEs = %d\n", CkNumPes());
        CkPrintf("Overdecomposition     = %d\n", overdecomposition);
        CkPrintf("Grid sizes            = %d,%d\n", m, n);
        CkPrintf("Number of iterations  = %d\n", maxiterations);
        if (grp > 1)
        CkPrintf("Group factor          = %d (cheating!)\n", grp);

        // Create new array of worker chares
        array = CProxy_P2p::ckNew(num_chares);

        //Start the computation
	array.run();
    }

    // One worker reports back to here when it completes the workload
    void report(double result) {
      double totalTime, flops, diff;
      double corner_val = (double) ((maxiterations+1)*(m+n-2));
      totalTime = endTime - startTime;
      // flip sign of time if grouping is applied (cheating)                       
      if (grp>1) totalTime *= -1.0;
      flops = (double) (2*(n-1)) * (double) (m-1)*maxiterations;
      diff = ABS(result-corner_val);
      if (diff < EPSILON) {
        CkPrintf("Solution validates\n");
        CkPrintf("Rate (MFlops): %lf Avg time (s) %lf\n", flops/totalTime/1.e6, totalTime/maxiterations);
      }
      else {
        CkPrintf("Solution does not validate\n");
      }
      CkPrintf("Reference corner value: %lf, corner value: %lf, |diff|: %e \n", 
               corner_val, result, diff);
      CkExit();
    }

};

class P2p: public CBase_P2p {
  P2p_SDAG_CODE

public:
  int iterations;
  double result;
  int    offset, istart, iend, j; // global grid indices of strip
  int    width; 
  double *vector;

  // Constructor, initialize values
  P2p() {
    int i, iloc, leftover;
      
    /* compute amount of space required for input and solution arrays             */
    width = m/num_chares;
    leftover = m%num_chares;
    if (thisIndex < leftover) {
      istart = (width+1) * thisIndex; 
      iend = istart + width;
    }
    else {
      istart = (width+1) * leftover + width * (thisIndex-leftover);
      iend = istart + width - 1;
    }
    width = iend - istart + 1;

    // allocate two dimensional array
    vector = new double[n*(width+1)];
    if (!vector) {
      CkPrintf("ERROR: Char %d could not allocate array of size %d\n", thisIndex, n*(width+1));
      CkExit();
    }

    // initialize
    if (thisIndex == 0) for (j=0; j<n; j++) ARRAY(0,j) = (double) j;
    for(i=istart-1;i<=iend;i++) ARRAY(i-istart,0) = (double) i;
    if (thisIndex == 0) offset=1; else offset=0;
  }

  // a necessary function, which we ignore now. If we were to use load balancing 
  // and migration this function might become useful
    P2p(CkMigrateMessage* m) {}

    ~P2p() { 
      delete [] vector;
    }

    // Perform one or more grid lines worth of work
    // The first step is to receive data from a left neighbor, if any
    void processGhost(ghostMsg *msg) {
      int jj, jjsize;

      jjsize = MIN(grp, n-j);
      for (jj=0; jj<jjsize; jj++) ARRAY(-1,j+jj) = msg->gp[jj];
      delete msg;
    }

    // do the actual work
    void compute() {
      int iloc, jj, jjsize;
      jjsize = MIN(grp, n-j);

      for (jj=j; jj<j+jjsize; jj++) 
      for (int i=istart+offset,iloc=offset; i<=iend; i++,iloc++) 
        ARRAY(iloc,jj) = ARRAY(iloc-1,jj) + ARRAY(iloc,jj-1) - ARRAY(iloc-1,jj-1);
    }

    // The final step is to send the local state to the neighbors
    void pass_baton(void) {
      int jj, jjsize;

      // Send my right edge
      if (thisIndex < num_chares-1) {
	jjsize = MIN(grp, n-j);
        ghostMsg *msg = new (jjsize) ghostMsg();
        CkSetRefNum(msg, j+iterations*(n-1));
        for (jj=0; jj<jjsize; jj++) {
          msg->gp[jj] = ARRAY(iend-istart,j+jj);
	  //          CkPrintf("Chare %d, send_msg->[%d]=%lf\n", thisIndex, jj, msg->gp[jj]);
        }
        thisProxy(thisIndex+1).receiveGhost(msg);
      }
    }

    // Receive top right grid value and plop in 0,0 position
    void processCorner(cornerMsg *msg) {

      ARRAY(0,0) = msg->gp[0];
      delete msg;
    }

    // send the top right grid value to chare zero
    void sendCorner(void) {

      cornerMsg *msg = new (1) cornerMsg();
      CkSetRefNum(msg, iterations);
      msg->gp[0]   = -ARRAY(iend-istart,n-1);
      thisProxy(0).receiveCorner(msg);
    }

};

#include "p2p.def.h"
