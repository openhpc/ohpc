#ifndef __ADIOS_QUERY_H__
#define __ADIOS_QUERY_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "adios_read.h"

//#define ADIOS_QUERY_METHOD_COUNT  2

//int gCurrentTimeStep;

enum ADIOS_QUERY_METHOD 
{
    ADIOS_QUERY_METHOD_FASTBIT = 0,
    ADIOS_QUERY_METHOD_ALACRITY = 1,
    ADIOS_QUERY_METHOD_UNKNOWN = 2,
    ADIOS_QUERY_METHOD_COUNT = ADIOS_QUERY_METHOD_UNKNOWN
};
    

enum ADIOS_PREDICATE_MODE 
{
    ADIOS_LT = 0,
    ADIOS_LTEQ = 1,
    ADIOS_GT = 2,
    ADIOS_GTEQ = 3,
    ADIOS_EQ = 4,
    ADIOS_NE = 5
};

enum ADIOS_CLAUSE_OP_MODE 
{
    ADIOS_QUERY_OP_AND = 0,
    ADIOS_QUERY_OP_OR  = 1
};

typedef struct {
    char* condition;
    void* queryInternal;

    // keeping start/count to map 1d results from fastbit to N-d

    ADIOS_SELECTION* sel;
    void* dataSlice;

    ADIOS_VARINFO* varinfo;
    char* varName;

    ADIOS_FILE* file;
    enum ADIOS_QUERY_METHOD method;

    enum ADIOS_PREDICATE_MODE predicateOp;
    char* predicateValue;
    uint64_t rawDataSize; // this is the result of dim/start+count

    void* left;
    void* right;
    enum ADIOS_CLAUSE_OP_MODE combineOp;

    int onTimeStep; // dataSlice is obtained with this timeStep 

    uint64_t maxResultsDesired;
    uint64_t resultsReadSoFar;

    int hasParent;
    int deleteSelectionWhenFreed;
} ADIOS_QUERY;
   

#ifndef __INCLUDED_FROM_FORTRAN_API__

/* functions */

int adios_query_is_method_available(enum ADIOS_QUERY_METHOD method);

ADIOS_QUERY* adios_query_create (ADIOS_FILE* f, 
                                 ADIOS_SELECTION* queryBoundary,
                                 const char* varName,
                                 enum ADIOS_PREDICATE_MODE op,
                                 const char* value); 


ADIOS_QUERY* adios_query_combine (ADIOS_QUERY* q1, 
                                  enum ADIOS_CLAUSE_OP_MODE operator,		    
                                  ADIOS_QUERY* q2);

/* 
 *  Select a query method manually for a query evaluation. 
 *  If not set by the user, a suitable query method is chosen at evaluation
*/
void adios_query_set_method (ADIOS_QUERY* q, enum ADIOS_QUERY_METHOD method);


/*
 * Estimate the number of hits of the query at "timestep"
 * 
 * 
 * IN:  q               query
 *      timestep        timestep of interest
 *
 * RETURN:  -1 : error
 *          >=0: estimated hits
 *
 */

int64_t adios_query_estimate (ADIOS_QUERY* q, int timeStep);

// obsolete. time_steps for non-streaming files should show up in estimate/evalute
//void adios_query_set_timestep (int timeStep);

/*
 * Evaluate and return result of the query at the "timestep"
 * result will be limited to "batchSize". May need to call Multiple times
 * to get all the result.
 * 
 * IN:  q               query
 *      timestep        timestep of interest
 *	batchSize       max size of results to return of this call
 *      outputBoundary  query results will be mapped to this selection
 *	                outputBoundary must match the selections used when construct the query
 *			if NULL, then will use the first selection used in query
 * OUT: queryResult     list of points
 *                      NULL if no result      
 * RETURN:  -1: error
 *           1: if more results to follow, keep calling evaluate() to find out
 *           0: of no more results to fetch 
 *
 */

int  adios_query_evaluate (ADIOS_QUERY* q, 
			   ADIOS_SELECTION* outputBoundary,// must supply to get results
			   int timestep,
			   uint64_t batchSize, // limited by maxResult
			   ADIOS_SELECTION** queryResult);


void adios_query_free(ADIOS_QUERY* q);

#endif /* __INCLUDED_FROM_FORTRAN_API__ */

#ifdef __cplusplus
}
#endif

#endif /* __ADIOS_QUERY_H__ */
