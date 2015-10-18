#include "transpose.decl.h"
#include <par-res-kern_general.h>

#define A(i,j)        A_p[(i+istart)+order*(j)]
#define B(i,j)        B_p[(i+istart)+order*(j)]
#define Work_in(i,j)  Work_in_p[i+Block_order*(j)]
#define Work_out(i,j) Work_out_p[i+Block_order*(j)]

/*readonly*/ CProxy_Main mainProxy;
/*readonly*/ int order; // array size
/*readonly*/ int num_chares;
/*readonly*/ int overdecomposition; 
/*readonly*/ int maxiterations;
/*readonly*/ int Block_order;
/*readonly*/ int Tile_order;
/*readonly*/ int tiling;
/*readonly*/ int Colblock_size;
/*readonly*/ int Block_size;
/*readonly*/ double startTime;
/*readonly*/ double endTime;
/*readonly*/ long bytes;

class blockMsg : public CMessage_blockMsg {
public:
  int blockID;
  double *blockData;
  blockMsg(int _t) : blockID(_t) {
  }
};

class Main : public CBase_Main
{

public:
    CProxy_Transpose array;

    Main(CkArgMsg* cmdlinearg) {
        if (cmdlinearg->argc != 5) {
          CkPrintf("%s <#iterations> <matrix order> <tile size><overdecomposition factor>\n",
          cmdlinearg->argv[0]); CkExit();
        }

        // store the main proxy
        mainProxy = thisProxy;

        maxiterations = atoi(cmdlinearg->argv[1]);
        if (maxiterations < 1) {
          CkPrintf("ERROR: #iterations must be positive: %d\n", maxiterations);
          CkExit();
        }
        order = atoi(cmdlinearg->argv[2]);
        if (order < CkNumPes()) {
          CkPrintf("ERROR: Matrix order %d smaller than #PEs %d\n", order, CkNumPes());
          CkExit();
        }

        Tile_order = atoi(cmdlinearg->argv[3]);
        if (Tile_order < 1) {
          CkPrintf("ERROR: Tile size must be positive: %d \n", Tile_order);
          CkExit();
        }

        overdecomposition = atoi(cmdlinearg->argv[4]);
        if (overdecomposition<1) {
          CkPrintf("ERROR: Overdecomposition factor must be positive: %d\n", overdecomposition);
          CkExit();
        }

        num_chares = CkNumPes()*overdecomposition;
        if (!(order/num_chares)) {
          CkPrintf("ERROR: Matrix order %d smaller than #chares %d\n", order, num_chares);
          CkExit();
        }
        if (order%num_chares) {
          CkPrintf("ERROR: Matrix order %d not multiple of #chares $d\n", order, num_chares);
          CkExit();
        }

        Block_order = order/num_chares;
        Colblock_size = order * Block_order;
        Block_size  = Block_order * Block_order;

        tiling = (Tile_order > 0) && (Tile_order < order);
        bytes = 2 * sizeof(double) * order * order;

        // print info
        CkPrintf("Charm++ transpose execution\n");
        CkPrintf("Number of Charm++ PEs = %d\n", CkNumPes());
        CkPrintf("Overdecomposition     = %d\n", overdecomposition);
        CkPrintf("Matrix order          = %d\n", order);
        CkPrintf("Tile size             = %d\n", Tile_order);
        CkPrintf("Number of iterations  = %d\n", maxiterations);

        // Create new array of worker chares
        array = CProxy_Transpose::ckNew(num_chares);

        //Start the computation
	array.run();
    }

    // One worker reports back to here when it completes the workload
    void report(double result) {

      double epsilon = 1.e-8, avgtime;
      if (result < epsilon) {
        CkPrintf("Solution validates\n");
        avgtime = (endTime-startTime)/(double)maxiterations;
        CkPrintf("Rate (MB/s): %lf Avg time (s): %lf\n",1.0E-06*bytes/avgtime, avgtime);
      }
      else                  
	CkPrintf("Solutions does not validate; diff = %e, threshold = %e\n",
                 result, epsilon);
      CkExit();
    }

};

class Transpose: public CBase_Transpose {
  Transpose_SDAG_CODE

public:
  int iterations, phase, colstart;
  double result, local_error;
  int send_to, recv_from;
  double *A_p, *B_p, *Work_in_p, *Work_out_p;

  // Constructor, initialize values
  Transpose() {
      
    // allocate two dimensional array
    A_p        = new double[Colblock_size];
    B_p        = new double[Colblock_size];
    Work_in_p  = new double[Block_size];
    Work_out_p = new double[Block_size];
    if (! A_p || !B_p || !Work_in_p || !Work_out_p) { 
      CkPrintf("Could not allocate memory for matrix blocks\n");
      CkExit();
    }

    /* set value of starting column for this chare                      */
    colstart = thisIndex*Block_order;

    /* Fill the original column matrix in A.                            */
    int istart = 0;  
    for (int j=0;j<Block_order;j++) for (int i=0;i<order; i++) {
      A(i,j) = (double) (order*(j+colstart) + i);
    }

    /*  Set the transpose matrix to a known garbage value.              */
    for (int i=0;i<Colblock_size; i++) B_p[i] = -1.0;
  }

  // a necessary function, which we ignore now. If we were to use load balancing 
  // and migration this function might become useful
  Transpose(CkMigrateMessage* m) {}

  ~Transpose() { 
    delete [] A_p;
    delete [] B_p;
    delete [] Work_in_p;
    delete [] Work_out_p;
  }

  // Perform one matrix block worth of work
  void diagonal_transpose() {

    int istart = colstart; 
    if (!tiling) {
      for (int i=0; i<Block_order; i++) 
        for (int j=0; j<Block_order; j++) {
          B(j,i) = A(i,j);
	}
    }
    else {
      for (int i=0; i<Block_order; i+=Tile_order) 
        for (int j=0; j<Block_order; j+=Tile_order) 
          for (int it=i; it<MIN(Block_order,i+Tile_order); it++)
            for (int jt=j; jt<MIN(Block_order,j+Tile_order);jt++)
              B(jt,it) = A(it,jt); 
    }
  }

  void nondiagonal_transpose(int send_to) {

      int istart = send_to*Block_order; 
      if (!tiling) {
        for (int i=0; i<Block_order; i++) 
          for (int j=0; j<Block_order; j++){
	    Work_out(j,i) = A(i,j);
	  }
      }
      else {
        for (int i=0; i<Block_order; i+=Tile_order) 
          for (int j=0; j<Block_order; j+=Tile_order) 
            for (int it=i; it<MIN(Block_order,i+Tile_order); it++)
              for (int jt=j; jt<MIN(Block_order,j+Tile_order);jt++) {
                Work_out(it,jt) = A(jt,it); 
	      }
      }
  }

  // The next step is to send the block to the proper destination
  void sendBlock(int send_to) {

    blockMsg *msg = new (Block_size) blockMsg(thisIndex);
    if (!msg) {
      CkPrintf("Could not allocate space for message\n");
      CkExit();
    }
    CkSetRefNum(msg, phase+iterations*num_chares);
    memcpy(msg->blockData, Work_out_p, Block_size*sizeof(double));
    thisProxy(send_to).receiveBlock(msg);
  }

  // The final step is to receive the transposed block and store it in the proper place    
  void processBlock(blockMsg *msg) {

    memcpy(Work_in_p,msg->blockData,Block_size*sizeof(double));
    int istart = msg->blockID*Block_order; 
    /* scatter received block to transposed matrix; no need to tile */
    for (int j=0; j<Block_order; j++)
      for (int i=0; i<Block_order; i++) 
        B(i,j) = Work_in(i,j);
  
    delete msg;
  }

  void compute_local_error() {

    local_error = 0.0;
    int istart = 0;
    for (int j=0;j<Block_order;j++) for (int i=0;i<order; i++) {
        local_error += ABS(B(i,j) - (double)(order*i + j+colstart));
    }
  }

};

#include "transpose.def.h"
