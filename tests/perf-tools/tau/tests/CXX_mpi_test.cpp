#include "mpi.h"
#include <cmath>
#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <ctime>

#define LIN(x, y, dimX) ((x) + (y)*(dimX))
#define LIN2(x,y,dimX) (LIN(x+1,y+1,dimX+2))
#define CYCLIN(x, y, dimX, dimY) LIN((x+dimX) % (dimX), (y+dimY) % (dimY), (dimX))

static double ks = 1.0;

class Grid {
  private:
    double *myGridPrev_, *myGridCurr_, *myGridNext_;
    double *leftEdgeIn_, *leftEdgeOut_, *rightEdgeIn_, *rightEdgeOut_;
    int numGridsX_, numGridsY_, dimX_, dimY_;
    int myGridX_, myGridY_;

  public:
    Grid(int dimX, int dimY, int numGridsX, int numGridsY, int numInitialPerturbations)
        : dimX_(dimX), dimY_(dimY), numGridsX_(numGridsX), numGridsY_(numGridsY) {
        myGridPrev_ = new double[(dimX+2)*(2+dimY)]();
        myGridCurr_ = new double[(dimX+2)*(2+dimY)]();
        myGridNext_ = new double[(dimX+2)*(2+dimY)]();
        leftEdgeIn_   = new double[dimY];
        rightEdgeIn_  = new double[dimY];
        leftEdgeOut_  = new double[dimY];
        rightEdgeOut_ = new double[dimY];

        int myRank;
        MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
        myGridX_ = myRank % numGridsX_;
        myGridY_ = myRank / numGridsX_;

        initGrid(numInitialPerturbations);
    }

    ~Grid() {
        delete[] myGridPrev_;
        delete[] myGridCurr_;
        delete[] myGridNext_;
        delete[] leftEdgeIn_;
        delete[] rightEdgeIn_;
        delete[] leftEdgeOut_;
        delete[] rightEdgeOut_;
    }

    void doIterations(int numIterations) {
        for (int i = 0; i < numIterations; ++i) {
            exchangeEdges();
            doOneIteration();

            double *tmp = myGridPrev_;
            myGridPrev_ = myGridCurr_;
            myGridCurr_ = myGridNext_;
            myGridNext_ = tmp;
        }
    }

  private:
    void initGrid(int numInitialPerturbations) {
        for(int s = 0; s < numInitialPerturbations; s++){    
            // Determine where to place a circle within the interior of the 2-d domain
            int radius = 20+rand() % 30;
            int xcenter = radius + rand() % (dimX_*numGridsX_ - 2*radius);
            int ycenter = radius + rand() % (dimY_*numGridsY_ - 2*radius);

            // Draw the circle
            for(int x = 1; x < dimX_; x++){
                for(int y = 1; y < dimY_; y++){
                    // The coordinate in the global data array (not just in this rank's portion)
                    double globalx = myGridX_*dimX_ + x;
                    double globaly = myGridY_*dimY_ + y;

                    double distanceToCenter = sqrt((globalx-xcenter)*(globalx-xcenter)
                                                 + (globaly-ycenter)*(globaly-ycenter));

                    if (distanceToCenter < radius) {
                        // ranges from 0 to 3pi/2 
                        double rscaled = (distanceToCenter/radius) * 3.0 * 3.14159/2.0;

                        // Range won't exceed -700 to 700
                        double t = 700.0 * cos(rscaled);

                        myGridCurr_[LIN2(x,y,dimX_)] = myGridPrev_[LIN2(x,y,dimX_)] = t;
                    }
                }                       
            }
        }
    }

    void doOneIteration() {
        for (int x = 0; x < dimX_; ++x) {
            for (int y = 0; y < dimY_; ++y) {
                myGridNext_[LIN2(x,y,dimX_)] = 
                    ks * (myGridCurr_[LIN2(x+1,y,dimX_)] + myGridCurr_[LIN2(x-1,y,dimX_)]
                        + myGridCurr_[LIN2(x,y+1,dimX_)] + myGridCurr_[LIN2(x,y-1,dimX_)]
                        - myGridCurr_[LIN2(x,y,dimX_)] * 4)
                  - myGridPrev_[LIN2(x,y,dimX_)] + 2*myGridCurr_[LIN2(x,y,dimX_)];
            }
        }
    }

    void exchangeEdges() {
        MPI_Request reqs[4];
        MPI_Status stats[4];

        int topRank = CYCLIN(myGridX_, myGridY_+1, numGridsX_, numGridsY_);
        int leftRank = CYCLIN(myGridX_-1, myGridY_, numGridsX_, numGridsY_);
        int rightRank = CYCLIN(myGridX_+1, myGridY_, numGridsX_, numGridsY_);
        int bottomRank = CYCLIN(myGridX_, myGridY_-1, numGridsX_, numGridsY_);


        /*************** Recv ***************/
        // Top.
        MPI_Irecv(&(myGridCurr_[LIN(1,0,dimX_+2)]), dimX_, MPI_DOUBLE, topRank, 0, MPI_COMM_WORLD, &reqs[0]);

        // Bottom.
        MPI_Irecv(&(myGridCurr_[LIN(1,dimY_+1,dimX_+2)]), dimX_, MPI_DOUBLE,
                  bottomRank, 0, MPI_COMM_WORLD, &reqs[1]);

        // Left.
        MPI_Irecv(leftEdgeIn_, dimY_, MPI_DOUBLE, leftRank, 0, MPI_COMM_WORLD, &reqs[2]);

        // Right.
        MPI_Irecv(rightEdgeIn_, dimY_, MPI_DOUBLE, rightRank, 0, MPI_COMM_WORLD, &reqs[3]);


        /*************** Send ***************/
        // Top.
        MPI_Send(&(myGridCurr_[LIN2(0,0,dimX_)]), dimX_, MPI_DOUBLE, topRank, 0, MPI_COMM_WORLD);

        // Bottom.
        MPI_Send(&(myGridCurr_[LIN2(0,dimY_-1,dimX_)]), dimX_, MPI_DOUBLE, bottomRank, 0, MPI_COMM_WORLD);

        // Left.
        // A bit more annoying, since the data is not stored contiguously.
        for (int i = 0; i < dimY_; ++i) leftEdgeOut_[i] = myGridCurr_[LIN2(0, i, dimX_)];
        MPI_Send(leftEdgeOut_, dimY_, MPI_DOUBLE, leftRank, 0, MPI_COMM_WORLD);

        // Right.
        for (int i = 0; i < dimY_; ++i) rightEdgeOut_[i] = myGridCurr_[LIN2(dimX_-1, i, dimX_)];
        MPI_Send(rightEdgeOut_, dimY_, MPI_DOUBLE, rightRank, 0, MPI_COMM_WORLD);


        /*************** Update ***************/
        // Wait for all messages to be sent/received.
        MPI_Waitall(4, reqs, stats);

        for (int y = 0; y < dimY_; ++y) {
            myGridCurr_[LIN(0, y+1, dimX_+2)] = leftEdgeIn_[y];
            myGridCurr_[LIN(dimX_+1, y+1, dimX_+2)] = rightEdgeIn_[y];
        }
    }
};

int main(int argc, char *argv[])
{
    // Initialize MPI stuff.
    int nprocs, myRank;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

    // Seed the RNG.
    int seed;
    if (myRank == 0) seed = time(NULL);
    MPI_Bcast(&seed, 1, MPI_INT, 0, MPI_COMM_WORLD);
    srand(seed);

    // Complain if we're missing arguments.
    assert(argc >= 6);

    // Get dimensionality information from the command line.
    // FIXME?: Number of ranks cannot be prime.
    int myDimX = atoi(argv[1]), myDimY = atoi(argv[2]);
    int numGridsX = atoi(argv[3]), numGridsY = atoi(argv[4]);
    int numInitPerturbations = atoi(argv[5]);

    // Make sure those dimensions are right before moving on.
    assert(numGridsX * numGridsY == nprocs);

    Grid grid(myDimX, myDimY, numGridsX, numGridsY, numInitPerturbations);
    grid.doIterations(100);

    MPI_Finalize();
    return 0;
}
