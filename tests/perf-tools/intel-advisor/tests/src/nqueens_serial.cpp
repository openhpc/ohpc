//=======================================================================
//
// SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF END-USER LICENSE AGREEMENT FOR
// INTEL(R) ADVISOR XE 2013.
//
// Copyright (C) 2009-2014 Intel Corporation. All rights reserved
//
// THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY
// RIGHTS.
//
// ========================================================================

// [DESCRIPTION]
// Solve the nqueens problem  - how many positions of queens can fit on a chess
// board of a given size without attacking each other.
//
// [RUN]
// To set the board size in Visual Studio, right click on the project,
// select Properies > Configuration Properties > General > Debugging.  Set
// Command Arguments to the desired value.  14 has been set as the default.
//
// [EXPECTED OUTPUT]
// Depends upon the board size.
//
// Board Size   Number of Solutions
//     4                2
//     5               10
//     6                4
//     7               40
//     8               92
//     9              352
//    10              724
//    11             2680
//    12            14200
//    13            73712
//    14           365596
//    15          2279184

#include <iostream>
#include <cstdlib>

#ifdef _WIN32
#include <windows.h>
#include <mmsystem.h>
#define TimeType        DWORD
#define GET_TIME(t)     t = timeGetTime()
#define TIME_IN_MS(t)   (t)
#else
#include <sys/time.h>
#define TimeType        struct timeval
#define GET_TIME(t)     gettimeofday((&t), NULL)
#define TIME_IN_MS(t)   (((t).tv_sec * 1000000 + (t).tv_usec) / 1000)
#endif

//ADVISOR SUITABILITY EDIT: Uncomment the #include <advisor-annotate.h> line to
//                          use Advisor annotations.
//#include <advisor-annotate.h>

using namespace std;

int nrOfSolutions = 0;     // Counts the number of solutions.
int size = 0;              // The board-size; read from command-line.

// The number of correct solutions for each board size.
const int correctSolution[16] = {     0,     1,      0,       0, //  0 -  3
                                      2,    10,      4,      40, //  4 -  7
                                     92,   352,    724,    2680, //  8 - 11
                                  14200, 73712, 365596, 2279184  // 12 - 15
};


/*
 * Recursive function to find all solutions on a board, represented by the
 * argument "queens", when we place the next queen at location (row, col).
 *
 * On Return: nrOfSolutions has been increased by the number of solutions for
 *            this board.
 */
void setQueen(int queens[], int row, int col) {
    //ADVISOR COMMENT: The accesses to the "queens" array in this function
    //                 create an incidental sharing correctness issue.
    //ADVISOR COMMENT: Each task should have its own copy of the queens array.
    //ADVISOR COMMENT: Look at the solve() function to see how to fix this.

    // Check all previously placed rows for attacks.
    for (int i=0; i < row; i++) {
        // Check vertical attacks.
        if (queens[i] == col) {
            return;
        }
        // Check diagonal attacks.
        if (abs(queens[i] - col) == (row - i) ) {
            return;
        }
    }

    // Column is ok, set the queen.
    //ADVISOR COMMENT: See comment at top of function.
    queens[row]=col;

    if (row == (size - 1)) {
        //ADVISOR CORRECTNESS EDIT: Uncomment the following two LOCK
        //         annotations to lock the access to nrOfSolutions and
        //         eliminate the race condition.
        //ANNOTATE_LOCK_ACQUIRE(0);

        //ADVISOR COMMENT: This is a race condition because multiple tasks may
        //                 try and increment nrOfSolutions at the same time.
        nrOfSolutions++;  // Placed final queen, found a solution!

        //ANNOTATE_LOCK_RELEASE(0);
    } else {
        // Try to fill next row.
        for (int i=0; i < size; i++) {
            setQueen(queens, row+1, i);
        }
    }
}


/*
 * Find all solutions for nQueens problem on size x size chessboard.
 *
 * On Return: nrOfSolutions = number of solutions for size x size chessboard.
 */
void solve() {

    //ADVISOR COMMENT: When surveying, this is the top function below main.
    //                 This for() loop is a candidate for parallelization.

    //ADVISOR CORRECTNESS EDIT: Comment out the following declaration of the
    //                          queens array.
    int *queens = new int[size]; // Array of queens on the board.

    //ADVISOR SUITABILITY EDIT: Uncomment the three annotations below to model
    //                          parallelizing the body of this for() loop.
    //ANNOTATE_SITE_BEGIN(solve);
    for (int i=0; i < size; i++) {
        //ANNOTATE_ITERATION_TASK(setQueen);

        //ADVISOR CORRECTNESS EDIT: Uncomment the declaration of queens.  This
        //                          creates a separate array for each recursion
        //                          eliminating the incidental sharing.
        //int * queens = new int[size]; // Array of queens on the chess board.

        //ADVISOR COMMENT: The call below exhibits incidental sharing when all
        //                 of the tasks use the same copy of "queens".
        // Try all positions in first row.
        setQueen(queens, 0, i);

        //ADVISOR CORRECTNESS EDIT: Uncomment the deletion of the queens array.
        //delete [] queens;
    }
    //ANNOTATE_SITE_END();

    //ADVISOR CORRECTNESS EDIT: Comment out the deletion of the queens array.
    delete [] queens;
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " boardSize [default is 14].\n";
        size = 14;
    } else {
        size = atoi(argv[1]);
        // Limit the range of inputs we accept.  If the board is not large
        // enough, the program may finish before suitability and other analyses
        // can produce an accurate result.  If it is too large, the program
        // may appear to hang (even though it is just taking a lot of time.)
        if ((size < 4) || (size > 15)) {
            cerr << "Boardsize should be between 4 and 15; "
                "setting it to 14. \n" << endl;
            size = 14;
        }
    }

    cout << "Starting nqueens (" << argv[0] << ") solver for size " << size
         << "...\n";

    TimeType startTime, endTime;
    GET_TIME(startTime);
    solve();
    GET_TIME(endTime);

    cout << "Number of solutions: " << nrOfSolutions << endl;
    if (nrOfSolutions != correctSolution[size])
        cout << "!!Incorrect result!! Number of solutions should be " <<
            correctSolution[size] << endl << endl;
    else
        cout << "Correct result!" << endl;

    cout << endl << "Calculations took " <<
        TIME_IN_MS(endTime) - TIME_IN_MS(startTime) << "ms." << endl;
    return 0;
}
