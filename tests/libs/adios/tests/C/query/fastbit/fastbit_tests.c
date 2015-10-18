#include <string.h>
#include <inttypes.h>

#include <mxml.h>
#include <adios_read.h>
#include <adios_query.h>

#include <stdio.h>
#include <stdlib.h>
/*
static int mybmreader(void *ctx, uint64_t start,uint64_t count, uint32_t *buf) {
  const uint32_t *bms = (uint32_t*)ctx + start;
  unsigned j;
  for (j = 0; j < count; ++ j) {
    buf[j] = bms[j];
  }
  return 0;
} // mybmreader  
*/

char _gTagQuery[20] = "query";
char _gTagSel[20] = "selection";
char _gTagEntry[20] = "entry";
char _gTagOutput[20] = "output";

char _gAttrAction[20] = "action";
char _gAttrBPFile[20] = "bpFile";
char _gAttrBatchSize[20] = "batchSize";
char _gAttrQueryName[20] = "name";
char _gAttrVarName[20] = "varName";
char _gAttrVarValue[20] = "value";
char _gAttrOperand[20] = "op";
char _gAttrSelType[20] = "type";
char _gAttrSelDim[20] = "dim";
char _gAttrSelID[20] = "id";
char _gAttrSelStart[20] = "start";
char _gAttrSelCount[20] = "count";
char _gAttrNode[20] = "node";


long _stageRefreshMillis = 0;
long _lastMeasuredMillis = 0;
long _queryStartMillis = 0;

void logReport(int stage) {
  long ms = fastbit_adios_getCurrentTimeMillis();

  if (stage == -1) { // init                                                                                                                                             
      _lastMeasuredMillis = ms;
      _stageRefreshMillis = ms;
      _queryStartMillis   = ms;
  } else if (stage == 0) { // query                                                                                                               
      printf("\n==> Total time spent to process this query: %ld millis.\n", ms - _queryStartMillis);
  }
}


void logTimeMillis(const char* notes)
{
  long ms = fastbit_adios_getCurrentTimeMillis();

  if (notes == NULL) {
    printf("\n");
    _stageRefreshMillis = ms;
  } else {
    long d = ms - _lastMeasuredMillis;
    printf("   ELAPSED millis: %ld \t%s\n", d, notes);
  }
  _lastMeasuredMillis = ms;
}


static int getTotalByteSize (ADIOS_FILE* f, ADIOS_VARINFO* v, ADIOS_SELECTION* sel,
                             uint64_t* total_byte_size, uint64_t* dataSize, int timestep)
{
  *total_byte_size = common_read_type_size (v->type, v->value);
  *dataSize = 1;

  if (sel == 0) {
    uint64_t s = 0;
    for (s=0; s<v->ndim; s++) {
      *total_byte_size *=v->dims[s];
      *dataSize *= v->dims[s];
      //     log_debug(" dim %" PRIu64 "default count %" PRIu64 "\n", s, v->dims[s]);                                                                       
    }
    return 0;
  }

  switch (sel->type) {
  case  ADIOS_SELECTION_BOUNDINGBOX:
    {
      const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb = &(sel->u.bb);
      uint64_t* count = bb->count;
      uint64_t* start = bb->start;

      int s=0;

      for (s=0; s<v->ndim; s++) {
	if (start[s]+count[s] > v->dims[s]) {
	  printf(" Invalid bounding box at %dth dim: start %" PRIu64 " + count %" PRIu64 " exceeds dim size: %" PRIu64 "\n", 
		 s, start[s], count[s], v->dims[s]);		 
	  return -1;
	}
	*total_byte_size *=count[s];
	*dataSize *= count[s];
      }

      break;
    }
  case ADIOS_SELECTION_POINTS:
    {
      const ADIOS_SELECTION_POINTS_STRUCT *pts = &(sel->u.points);
      *total_byte_size *= pts->npoints;
      *dataSize = pts->npoints;
      break;
    }
  case ADIOS_SELECTION_WRITEBLOCK:
    {
      const ADIOS_SELECTION_WRITEBLOCK_STRUCT *wb = &(sel->u.block);

      common_read_inq_var_blockinfo(f, v);
      int i=0;
      int min = v->nblocks[0];
      int absBlockCounter = wb->index;

      if (v->nsteps > 1) {      // all timesteps are known, can get abs                                                                                        
        for (i=0; i<v->nsteps; i++)
          {
            int nBlocksAtStep = v->nblocks[i];
            if (nBlocksAtStep < min) {
              min = nBlocksAtStep;
            }
            printf("\t\t   currstep=%d nblocks=%d\n", i, nBlocksAtStep);
            if (i < timestep) {
              absBlockCounter += nBlocksAtStep;
            }
          }
      }

      if (wb->index > min) {
	printf("Error: Unable to handle this block index %d over all the timesteps. Stop.\n", wb->index);
	return -1;
      }
      int j=0;
      for (j=0; j<v->ndim; j++)
        {
          *total_byte_size *= v->blockinfo[absBlockCounter].count[j];
          *dataSize *= v->blockinfo[absBlockCounter].count[j];
        }

      printf("\t\t   block %d, abs id:%d, bytes: %" PRIu64 ", size =  %" PRIu64 " \n", wb->index, absBlockCounter, *total_byte_size, *dataSize);

      break;
    }
  default:
    break;
  }
  return 0;
}

void recursive_free(ADIOS_QUERY* q) {
  ADIOS_QUERY* left = q->left;
  ADIOS_QUERY* right = q->right;

  if (q->sel != NULL) {
    if (q->sel->type == ADIOS_SELECTION_BOUNDINGBOX) {    
        const ADIOS_SELECTION_BOUNDINGBOX_STRUCT *bb = &(q->sel->u.bb);
	free(bb->start);
	free(bb->count);
    }
    adios_selection_delete(q->sel);
  }

  if (left != NULL) {
    recursive_free(left);
    recursive_free(right);
  }

  adios_query_free(q); 
}

int getInput(char* input, const char* delim, uint64_t** result, int dim)
{
  int i=0;
  char* token = strtok(input, delim);

  while (token) {
    (*result)[i] = atol(token);
    i++;
    if (i>=dim) {
      break;
    }
    token = strtok(0, delim);
  }

  if (i == dim) {    
    return 0;
  }
  return -1;
}


ADIOS_SELECTION* getSelFromSelectionNode(mxml_node_t* selNode) {
    const char* type = mxmlElementGetAttr(selNode, _gAttrSelType);
    if (type == NULL) {
      printf("No type specified, treat as NULL\n");
      return NULL;
    }

    if (strstr(type, "BOX") != NULL) {
      const char* dimStr = mxmlElementGetAttr(selNode, _gAttrSelDim);
      char* startStr = mxmlElementGetAttr(selNode, _gAttrSelStart);
      char* countStr = mxmlElementGetAttr(selNode, _gAttrSelCount);
      if ((dimStr == NULL) || ( startStr == NULL) || (countStr == NULL)) {
	printf("One of dim/count/start is not specified in BOUNDINGBOX, treat as NULL\n");
	return NULL;
      }
      
      int dim = atoi(dimStr);
      if (dim == 0) {
	printf("Invalid dim str %s: expecting to be > 0 \n", dimStr);
	exit(EXIT_FAILURE);
      }
      
      uint64_t* count = malloc(dim*sizeof(uint64_t));
      uint64_t* start = malloc(dim*sizeof(uint64_t));

      if (getInput(startStr, ",", &start, dim) != 0) {
	free(start);
	return NULL;
      }
      if (getInput(countStr, ",", &count, dim) != 0) {
	free(count);
	return NULL;
      }
      return adios_selection_boundingbox(dim, start, count);
    } else if (strstr(type, "BLOCK") != NULL) {
      const char* idStr = mxmlElementGetAttr(selNode, _gAttrSelID);
      if (idStr ==  NULL) {
	printf("block id is expected but not defined. ");
	return NULL;
      }
      int blockID = atoi(idStr);
      return adios_selection_writeblock(blockID);
    }

    return NULL;
}

ADIOS_SELECTION* getSel(mxml_node_t* entryNode) {
    mxml_node_t* selNode = mxmlFindElement(entryNode, entryNode, _gTagSel, NULL, NULL, MXML_DESCEND);
    if (selNode == NULL) {
      return NULL;
    }

    return getSelFromSelectionNode(selNode);
}


ADIOS_QUERY* getEntryQuery(mxml_node_t* queryNode, const char* entryName, ADIOS_FILE* f) 
{
  mxml_node_t* entryNode = mxmlFindElement(queryNode, queryNode, _gTagEntry, _gAttrQueryName, entryName, MXML_DESCEND);
  if (entryNode == NULL) {
    printf("No such subquery: %s\n", entryName);
    return NULL;
  }

  const char* actionAttr = mxmlElementGetAttr(entryNode,_gAttrAction);
  
  if (actionAttr != NULL) { // composite query    
    enum ADIOS_CLAUSE_OP_MODE op = ADIOS_QUERY_OP_AND;
    if (strcasecmp(actionAttr, "or") == 0) {
      op = ADIOS_QUERY_OP_OR;
    } 
    //continue here.. 
    mxml_node_t* leftNode  = mxmlFindElement(entryNode, entryNode, _gAttrNode,NULL, NULL, MXML_DESCEND);
    mxml_node_t* rightNode = mxmlFindElement(leftNode, entryNode, _gAttrNode, NULL, NULL, MXML_DESCEND);

    if ((leftNode == NULL) || (rightNode == NULL)) {
      return NULL;
    }

    const char* leftName = leftNode->child->value.text.string;
    const char* rightName = rightNode->child->value.text.string;

    ADIOS_QUERY* left = getEntryQuery(queryNode, leftName, f);
    ADIOS_QUERY* right = getEntryQuery(queryNode, rightName, f);

    mxmlDelete(entryNode);
    return adios_query_combine(left, op, right);

  } else {
    // a leaf;
    const char* varName = mxmlElementGetAttr(entryNode, _gAttrVarName);
    const char* value = mxmlElementGetAttr(entryNode, _gAttrVarValue);
    const char* opStr = mxmlElementGetAttr(entryNode, _gAttrOperand);

    if ((varName == NULL) || (value == NULL) || (opStr == NULL)) {
      return NULL;
    }    
    enum ADIOS_PREDICATE_MODE op = getOp(opStr);
    
    ADIOS_SELECTION* sel = getSel(entryNode);

    ADIOS_QUERY* q = adios_query_create(f, sel, varName, op, value);
    return q;
  }
      
  mxmlDelete(entryNode);
}

ADIOS_QUERY* constructQuery(mxml_node_t* queryNode, ADIOS_FILE* f, const char* queryName, uint64_t batchSize)
{
  mxml_node_t* opNode = mxmlFindElement(queryNode, queryNode, _gAttrOperand, NULL, NULL, MXML_DESCEND);
  if (opNode == NULL) 
  {
    return getEntryQuery(queryNode, NULL, f);
  } else {
    mxml_node_t*  leftNode = mxmlFindElement(opNode, opNode, _gAttrNode, NULL, NULL, MXML_DESCEND);    
    mxml_node_t*  rightNode = mxmlFindElement(leftNode, opNode, _gAttrNode, NULL, NULL, MXML_DESCEND);

    const char* leftName = leftNode->child->value.text.string;
    const char* rightName = rightNode->child->value.text.string;

    if ((leftName == NULL) || (rightName == NULL)) {
      printf("Unable to get entry for combination op. \n");
      return NULL;
    }

    ADIOS_QUERY* left = getEntryQuery(queryNode, leftName, f);
    ADIOS_QUERY* right = getEntryQuery(queryNode, rightName, f);
    
    enum ADIOS_CLAUSE_OP_MODE op = ADIOS_QUERY_OP_AND;
    if (opNode->value.element.num_attrs == 1) {
      if (strcasecmp(opNode->value.element.attrs[0].value, "or") == 0) {
	op = ADIOS_QUERY_OP_OR;
      }
    }
    mxmlDelete(opNode);
    return adios_query_combine(left, op, right);
  }
}

double getCurrentValue(void* data, uint64_t idx, enum ADIOS_DATATYPES type) 
{
  switch (type)
    {
    case adios_unsigned_byte:
      return ((uint8_t *) data)[idx];
    case adios_byte:
      return ((int8_t *) data)[idx];
    case adios_short:
      return ((int16_t *) data)[idx];
    case adios_unsigned_short:
      return ((uint16_t *) data)[idx];
    case adios_integer:
      return ((int32_t *) data)[idx];
    case adios_unsigned_integer:
      return ((uint32_t *) data)[idx];
    case adios_long:
      return  ((int64_t *) data)[idx];
    case adios_unsigned_long:
      return ((uint64_t *) data)[idx];
    case adios_real:
      return ((float *) data)[idx];
    case adios_double:
      return ((double *) data)[idx];
    case adios_long_double:
      return ((long double *) data)[idx];
    case adios_string:
      break;
    case adios_complex:
      break;
    case adios_double_complex:
      break;
    }
  return 0;
}

void manualCheck(ADIOS_QUERY* q, int timestep) {
  if ((q->left == NULL) && (q->right == NULL)) {    
      printf ("... manual check: \n");
      // proceed
      uint64_t totalByteSize;
      uint64_t totalSize;
      
      getTotalByteSize(q->file, q->varinfo, q->sel, &totalByteSize, &totalSize, timestep);
      
      void* output = malloc (totalByteSize);

      adios_schedule_read (q->file, q->sel, q->varName, timestep, 1, output);
      adios_perform_reads (q->file, 1);

      uint64_t hits=0;
      uint64_t k=0; 

      char* endptr;
      double vv = strtod(q->predicateValue, &endptr);

      for (k=0; k<totalSize; k++) {
	double curr = getCurrentValue(output, k, q->varinfo->type);
	//printf ("curr=%lg, vv=%lg\n", curr, vv);
	if (curr == vv) {
	  if ((q->predicateOp == ADIOS_EQ) || (q->predicateOp == ADIOS_LTEQ) || (q->predicateOp == ADIOS_GTEQ)) {
	      hits ++;
	  }
	} else if (curr < vv) {
	  if ((q->predicateOp == ADIOS_LT) || (q->predicateOp == ADIOS_LTEQ)) {
	    hits ++;
	  }
	} else { // >
	  if ((q->predicateOp == ADIOS_GT) || (q->predicateOp == ADIOS_GTEQ)) {
	    hits ++;
	  }
	}	  
      }
      free(output);
      printf("... double check found %d hits\n", hits);
      return;
  }
  printf("Skip manual check on composite query\n");
  return;
}

ADIOS_SELECTION* getOutputSelection(mxml_node_t* queryNode) 
{
  mxml_node_t* outputSelNode = mxmlFindElement(queryNode, queryNode, _gTagOutput, NULL, NULL, MXML_DESCEND);

  if (outputSelNode == NULL) {
    return NULL;
  } else {
    return getSelFromSelectionNode(outputSelNode);
  }
}

int parseQueryXml(const char* xmlQueryFileName) 
{
  int rank;
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);

  //adios_query_init(ADIOS_QUERY_METHOD_FASTBIT);


  FILE * fp = fopen (xmlQueryFileName,"r");
  if (fp == NULL){
    printf("No xml query file %s\n", xmlQueryFileName);    
    return -1;
  }

  fseek(fp, 0, SEEK_END);  
  long len = ftell(fp);
  rewind(fp);

  if (len == 0) {
    printf("No content in file %s.\n", xmlQueryFileName);
    return -1;
  }

  mxml_node_t* tree = mxmlLoadFile(NULL, fp, MXML_NO_CALLBACK);
  fclose(fp);

  if (tree == NULL) {
    printf("Invalid xml file: %d.\n", xmlQueryFileName);
    return -1;
  }

  ADIOS_FILE * f;
  MPI_Comm    comm_dummy = 0;  // MPI_Comm is defined through adios_read.h 

  adios_read_init_method(ADIOS_READ_METHOD_BP, comm_dummy, "verbose=2");

  mxml_node_t* testsNode = mxmlFindElement(tree, tree, "tests", NULL, NULL, MXML_DESCEND);

  mxml_node_t* queryNode; // mxmlFindElement(testsNode, testsNode, _gTagQuery, NULL, NULL, MXML_DESCEND);
  

  for (queryNode = mxmlFindElement(tree, tree, _gTagQuery, NULL, NULL, MXML_DESCEND);
       queryNode != NULL;
       queryNode = mxmlFindElement(queryNode, tree, _gTagQuery, NULL, NULL, MXML_DESCEND))
				   
    {
      //printf("query: %s\n", queryNode->value.element.attrs[0].value);
      mxml_value_t value = queryNode->value;
      int i=0; 
      const char* bpFileName = NULL;
      const char* queryName = NULL;
      uint64_t batchSize = 0;
      

      for (i = 0; i < value.element.num_attrs; i++) {	
	mxml_attr_t currAttr = value.element.attrs[i];
	if (strcasecmp(currAttr.name, _gAttrBPFile) == 0) {
	  bpFileName = currAttr.value;
	} else if (strcasecmp(currAttr.name, _gAttrQueryName) == 0) {
	  queryName = currAttr.value;
	} else if (strcasecmp(currAttr.name, _gAttrBatchSize) == 0) {
	  batchSize = atol(currAttr.value);
	}
      }

      if (bpFileName == 0) {
	printf("missing data file in query.\n");
	return -1;
      }

      f = adios_read_open_file (bpFileName, ADIOS_READ_METHOD_BP, comm_dummy);
      if (f == NULL) {
	printf("::%s\n", adios_errmsg());
	return -1;
      }

      ADIOS_QUERY* q = constructQuery(queryNode, f, queryName, batchSize);

      ADIOS_SELECTION* outputBox = getOutputSelection(queryNode);
      logReport(-1); // init timer

      //adios_query_set_method(q, ADIOS_QUERY_METHOD_FASTBIT);
      int timestep = 0;
      //ADIOS_SELECTION* noBox = 0;
      while (timestep <= f->last_step) {
	printf("\n ...... query=%s, %s, [TimeStep=%d of %d]\n",queryName, q->condition, timestep, f->last_step);
	int64_t est = adios_query_estimate(q, timestep);
	logTimeMillis(" estimated.");
	printf("\n=> query %s: %s, \n\t estimated  %ld hits on timestep: %d\n", queryName, q->condition, est, timestep);
	ADIOS_SELECTION* currBatch = NULL;
	int hasMore = 1; 
	while (hasMore > 0) {
	  hasMore = adios_query_evaluate(q, outputBox, timestep, batchSize, &currBatch);
	  logTimeMillis(" evaluated one batch.");
	  if (currBatch != NULL) {
	    printf("\n=> evaluated: %ld hits for %s\n", currBatch->u.points.npoints, q->condition);
	  }
	  if (currBatch != NULL) {
	    free (currBatch->u.points.points);
	    adios_selection_delete(currBatch);
	  }
	}
	logReport(0);
	manualCheck(q, timestep);
	logTimeMillis(" manual check done.");
	timestep ++;
      }
      
      recursive_free(q);
      adios_read_close(f);
      
      //mxmlDelete(queryNode);
   }
  
  mxmlDelete(testsNode);
  mxmlDelete(tree);    

  adios_read_finalize_method(ADIOS_READ_METHOD_BP);
  adios_finalize(rank);
}

/*
void testDefaultBoundBox(ADIOS_FILE* f, const char* varName1, const char* varName2, int timestep, const char* value1, const char* value2) 
{
  printf("\n=============== testing default bound box (no box specified) for all ===========\n");
  ADIOS_SELECTION* noBox = 0;

  //const char* varName1 = "/Timestep_0/cells/X";
  enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  //const char* value1 = "0.96874";

  //const char* varName2 = "/Timestep_0/cells/Y";
  enum ADIOS_PREDICATE_MODE op2 = ADIOS_GT;
  //const char* value2 = "0.96874";

  ADIOS_QUERY* q1 = adios_query_create(f, noBox, varName1, op1, value1);
  ADIOS_QUERY* q2 = adios_query_create(f, noBox, varName2, op2, value2);

  ADIOS_QUERY* q = adios_query_combine(q1, ADIOS_QUERY_OP_AND, q2);

  if (q != NULL) {
    //int timestep = 0;
    uint64_t max = 10000;
    //int64_t hitSize = adios_query_evaluate(q, timestep, max);

    int64_t batchSize = 50;
    while (1) {
      ADIOS_SELECTION* currBatch = NULL;
      int hasMore =  adios_query_evaluate(q, noBox, timestep, batchSize, &currBatch);
      adios_selection_delete(currBatch);
      
      if (hasMore <= 0) {
	break;
      }
    }

    //fastbit_selection_free(q->_queryInternal);
    adios_query_free(q);
  }

  adios_query_free(q1);
  adios_query_free(q2);

}

void testMixedIdxNoIdxCase()
{
  printf(" \n =========== testng on cases with mixed index/in-memory usage =========\n");

  //here you go
}

void testNoBoxOnSelection(ADIOS_FILE* f, const char* varName1, const char* lessThanValue, int timestep, enum ADIOS_PREDICATE_MODE op1) 
{
  printf("\n=============== testing box in query but no bound box for selection  ===========\n");

  //uint64_t start1[] = {0, 0, 0};
  //uint64_t count1[] = {256, 1,32};
  //uint64_t count1[] = {56, 1,32};

  ADIOS_SELECTION* noBox = NULL;
  //ADIOS_SELECTION* box = adios_selection_boundingbox(3, start1, count1);


  //enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  //const char* value1 = "0.96874";

  ADIOS_QUERY* q = adios_query_create(f, noBox, varName1, op1, lessThanValue);

  if (q != NULL) {
    //int timestep = 0;
    uint64_t max = 10000;
    //int64_t hitSize = adios_query_evaluate(q, timestep, max);

    //int64_t estimated = adios_query_estimate(q);
    //printf("estimated query result = %lld \n", estimated);
    
    int64_t batchSize = 50;
    while (1) {
      ADIOS_SELECTION* currBatch = NULL;
      int hasMore =  adios_query_evaluate(q, noBox, timestep, batchSize, &currBatch);
      adios_selection_delete(currBatch);
      
      if (hasMore <= 0) {
	break;
      }
    }

    adios_query_free(q);
  }

}

void testOneBoundBox(ADIOS_FILE* f, const char* varName1, const char* value1, int timestep) 
{
  printf("\n=============== testing one box specified) for all ===========\n");
  uint64_t start1[] = {0, 0, 0};
  uint64_t count1[] = {258, 34,34};
  ADIOS_SELECTION* box = adios_selection_boundingbox(3, start1, count1);


  //const char* varName1 = "/Timestep_0/cells/X";
  enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  //const char* value1 = "0.96874";

  //const char* varName2 = "/Timestep_0/cells/Y";
  const char* varName2 = varName1;
  enum ADIOS_PREDICATE_MODE op2 = ADIOS_GT;
  const char* value2 = "0.96874";

  ADIOS_QUERY* q1 = adios_query_create(f, box, varName1, op1, value1);
  //ADIOS_QUERY* q2 = adios_query_create(f, varName2, box, op2, value2);

  //ADIOS_QUERY* q = adios_query_combine(q1, ADIOS_QUERY_OP_AND, q2);

  if (q1 != NULL) {
    uint64_t max = 10000;

    //int64_t estimated = adios_query_estimate(q1);
    //printf("estimated query result = %lld \n", estimated);

    int64_t batchSize = 50;
    while (1) {
      ADIOS_SELECTION* currBatch = NULL;
      int hasMore =  adios_query_evaluate(q1, box, timestep, batchSize, &currBatch);
      adios_selection_delete(currBatch);
      
      if (hasMore <= 0) {
	break;
      }
    }

    //fastbit_selection_free(q->_queryInternal);
    adios_query_free(q1);
  }

  adios_selection_delete(box);

}

void testTwoBoundBoxes(ADIOS_FILE* f, const char* varName1, const char* value1, const char* value2, int timestep) 
{
  printf("\n=============== testing two box specified) for all ===========\n");


  uint64_t start1[] = {33, 0, 0};
  uint64_t count1[] = {32, 34,34};
  ADIOS_SELECTION* box1 = adios_selection_boundingbox(3, start1, count1);

  uint64_t start2[] = {65, 0, 0};
  uint64_t count2[] = {32, 34,34};
  ADIOS_SELECTION* box2 = adios_selection_boundingbox(3, start2, count2);


  //const char* varName1 = "/Timestep_0/cells/X";
  enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  //const char* value1 = "0.96874";

  //const char* varName2 = "/Timestep_0/cells/Y";
  //const char* varName2 = varName1;
  enum ADIOS_PREDICATE_MODE op2 = ADIOS_GT;
  //const char* value2 = "0.96874";

  ADIOS_QUERY* q1 = adios_query_create(f, box1, varName1, op1, value1);
  ADIOS_QUERY* q2 = adios_query_create(f, box2, varName1, op2, value2);

  ADIOS_QUERY* q = adios_query_combine(q1, ADIOS_QUERY_OP_AND, q2);

  if (q != NULL) {
    uint64_t max = 10000;

    //int64_t estimated = adios_query_estimate(q);
    //printf("estimated query result = %lld \n", estimated);

    int64_t batchSize = 50;
    while (1) {
      ADIOS_SELECTION* currBatch = NULL;
      int hasMore =  adios_query_evaluate(q, box1, timestep, batchSize, &currBatch);
      adios_selection_delete(currBatch);
      
      if (hasMore <= 0) {
	break;
      }
    }

    //fastbit_selection_free(q->_queryInternal);
    adios_query_free(q);
  }
  adios_query_free(q1);
  adios_query_free(q2);

  adios_selection_delete(box1);
  adios_selection_delete(box2);

}

void testUseOneWriteBlockSimpleLessThan(ADIOS_FILE* f, int blockNum, const char* varName1, const char* lessThanValue1, int timestep)
{
  printf("\n=============== testing one block for simple query: %s < %s at timestep: %d ===========\n", varName1, lessThanValue1, timestep);

  ADIOS_SELECTION* box = adios_selection_writeblock(blockNum);

  //const char* varName1 = "/Timestep_0/cells/X";
  enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  // const char* value1 = "0.96874";

  ADIOS_QUERY* q = adios_query_create(f, box, varName1, op1, lessThanValue1);

  if (q!= NULL) {
    //int timestep = 0;
    uint64_t max = 10000;
    //int64_t hitSize = adios_query_evaluate(q, timestep, max);
    
    int64_t batchSize = 50;
    
    //printf("time steps for variable is: %d \n",q->_var->nsteps);
    //int i=0;
    //for (i=0; i<q->_var->nsteps; i++) {
    // adios_query_set_timestep(i);

    //int64_t estimated = adios_query_estimate(q);
    //printf("estimated query result = %lld \n", estimated);

      while (1) {
	ADIOS_SELECTION* currBatch = NULL;
	int hasMore =  adios_query_evaluate(q, box, timestep, batchSize, &currBatch);
	adios_selection_delete(currBatch);
	
	if (hasMore <= 0) {
	  break;
	}
      }      
      //}

    //fastbit_selection_free(q->_queryInternal);
  }

  adios_query_free(q);
  adios_selection_delete(box);
}

void testUseOneWriteBlock(ADIOS_FILE* f, int blockNum, const char* varName1, const char* varName2)
{
  printf("\n=============== testing one block for all ===========\n");

  ADIOS_SELECTION* box = adios_selection_writeblock(blockNum);

  //const char* varName1 = "/Timestep_0/cells/X";
  enum ADIOS_PREDICATE_MODE op1 = ADIOS_LT;
  const char* value1 = "0.96874";

  //const char* varName2 = "/Timestep_0/cells/Y";
  enum ADIOS_PREDICATE_MODE op2 = ADIOS_GT;
  const char* value2 = "0.96874";

  ADIOS_QUERY* q1 = adios_query_create(f, box, varName1, op1, value2);
  ADIOS_QUERY* q2 = adios_query_create(f, box, varName2, op2, value2);

  ADIOS_QUERY* q = adios_query_combine(q1, ADIOS_QUERY_OP_AND, q2);

  if (q!= NULL) {
    int timestep = 0;
    uint64_t max = 10000;
    //int64_t hitSize = adios_query_evaluate(q, timestep, max);
    
    int64_t batchSize = 50;
    
    int i = 0;
    printf("time steps for variable is: %d \n",q1->varinfo->nsteps);
    for (i=0; i<q1->varinfo->nsteps; i++) {
      
      while (1) {
	ADIOS_SELECTION* currBatch = NULL;
	int hasMore =  adios_query_evaluate(q, box, timestep, batchSize, &currBatch);
	adios_selection_delete(currBatch);
	
	if (hasMore <= 0) {
	  break;
	}
      }
      
    }

    //fastbit_selection_free(q->_queryInternal);
    adios_query_free(q);
  }
  adios_query_free(q1);
  adios_query_free(q2);

  adios_selection_delete(box);
}
*/
/*
void doubleCheckWithIdxOnBlock(ADIOS_FILE* dataFile, const char* basefileName, int blockNum, ADIOS_VARINFO* v, int timestep, double lessThanVal) 
{
  ADIOS_FILE* idxFile = fastbit_adios_util_getFastbitIndexFileToRead(basefileName,  0);

  if (idxFile == 0) {
    printf("No such file.\n");
    return;
  }

  char bmsVarName[100];
  char keyVarName[100];
  char offsetName[100];

  sprintf(bmsVarName, "bms-%d-%d-%d", v->varid, timestep, blockNum);
  sprintf(keyVarName, "key-%d-%d-%d", v->varid, timestep, blockNum);
  sprintf(offsetName, "offset-%d-%d-%d", v->varid, timestep, blockNum);

  ADIOS_VARINFO * bmsV = adios_inq_var (idxFile, bmsVarName); 
  ADIOS_VARINFO * keyV = adios_inq_var (idxFile, keyVarName); 
  ADIOS_VARINFO * offsetV = adios_inq_var (idxFile, offsetName); 

  int64_t bms_byte_size = adios_type_size (bmsV->type, bmsV->value);
  int64_t key_byte_size = adios_type_size (keyV->type, keyV->value);
  int64_t offset_byte_size = adios_type_size (offsetV->type, offsetV->value);

  void *bms = malloc((bmsV->dims[0])*bms_byte_size);
  void *key = malloc((keyV->dims[0])*key_byte_size);
  void *offset = malloc(offsetV->dims[0]*offset_byte_size);

  uint64_t start[] = {0};
  uint64_t count_bms[] = {bmsV->dims[0]};
  uint64_t count_key[] = {keyV->dims[0]};
  uint64_t count_offset[] = {offsetV->dims[0]};

  ADIOS_SELECTION* bmsSel = adios_selection_boundingbox(bmsV->ndim, start, count_bms);
  ADIOS_SELECTION* keySel = adios_selection_boundingbox(keyV->ndim, start, count_key);
  ADIOS_SELECTION* offsetSel = adios_selection_boundingbox(offsetV->ndim, start, count_offset);

  // has one timestep in idx file
  adios_schedule_read(idxFile, bmsSel, bmsVarName, 0, 1, bms);
  adios_schedule_read(idxFile, keySel, keyVarName, 0, 1, key);
  adios_schedule_read(idxFile, offsetSel, offsetName, 0, 1, offset);

  adios_perform_reads(idxFile,1);

  uint64_t nk = keyV->dims[0];
  uint64_t no = offsetV->dims[0];
  printf(" bms/key/offset data: length=%lld/%lld/%lld\n", bmsV->dims[0], keyV->dims[0], offsetV->dims[0]);

  printData(bms, bmsV->type, bmsV->dims[0]);

  adios_selection_delete(bmsSel);
  adios_free_varinfo(bmsV);
  adios_read_close(idxFile);

  ADIOS_SELECTION* box = adios_selection_writeblock(blockNum);

  adios_inq_var_blockinfo(dataFile, v);
  uint64_t blockSize = getBlockSize(v, blockNum);
  void* data = malloc(adios_type_size(v->type, v->value)*blockSize);
  adios_schedule_read_byid(dataFile, box, v->varid, timestep, 1, data);
  adios_perform_reads(dataFile,1);

  fastbit_iapi_register_array("testme", getFastbitDataType(v->type), data, blockSize);
  
  int ierr = fastbit_iapi_attach_index ("testme", key, nk, offset, no, bms, mybmreader);
  if (ierr < 0) {
    printf(" reattaching index failed ierr = %ld\n", ierr);
  } else {
    FastBitSelectionHandle  h = fastbit_selection_osr("testme", FastBitCompareLess, lessThanVal);
    int64_t hits =  fastbit_selection_evaluate(h);
    printf(" double check, hits = %ld\n", hits);
  }
  free(bms);
  free(key);
  free(offset);

}
*/

void usage(char* prog)
{
  printf("Usage: %s <BP-file> [query]\n e.g. ./test_v1 my.bp \"x1 > 10\" \n", prog);
}

int main (int argc, char ** argv) 
{

  parseQueryXml("query.xml");
  return 0;


#if 0
  
    if (argc < 2) {
        usage(argv[0]);
	return 1;
    }

    ADIOS_FILE * f;
    MPI_Comm    comm_dummy = 0;  /* MPI_Comm is defined through adios_read.h */

    f = adios_read_open_file (argv[1], ADIOS_READ_METHOD_BP, comm_dummy);
    if (f == NULL) {
        printf ("::%s\n", adios_errmsg());
	return -1;
    }

    const char* varName1 = "/Timestep_0/cells/X";
    const char* varName2 = "/Timestep_0/cells/Y";

    if (argc > 2) {
      varName1 = argv[2];
    } 


    if (argc > 3) {
      int blockNum = 0; // relative to timestep
      int timestep = 1;
      const char* lessThanVal = argv[3];
      if (argc > 4) { // e.g.  ./query_fastbit data/record20110203.bp "/var/v1" 1.000 2
	timestep = atoi(argv[4]);
      }

      /*    
      if (argc > 5) {	
	printf("arg: dataFile var1 value timestep var2\n");
	varName2 = argv[5];
	const char* greaterThanVal = argv[6];

	testDefaultBoundBox(f, varName1, varName2,  timestep, lessThanVal, greaterThanVal);
      }
      */
      
	//testUseOneWriteBlockSimpleLessThan(f, blockNum, varName1, lessThanVal, timestep); 
	enum ADIOS_PREDICATE_MODE lessT = ADIOS_LT;
	testNoBoxOnSelection(f, varName1, lessThanVal, timestep, lessT);
	//testNoBoxOnSelection(f, varName1, lessThanVal, timestep, lessT);
	enum ADIOS_PREDICATE_MODE greaterT = ADIOS_GT;
	testNoBoxOnSelection(f, varName1, lessThanVal, timestep, greaterT);
	//testNoBoxOnSelection(f, varName1, lessThanVal, timestep, greaterT);
	//testOneBoundBox(f, varName1, lessThanVal, timestep);

	//const char* greaterThanVal = argv[5];
	//testTwoBoundBoxes(f, varName1, lessThanVal, greaterThanVal, timestep);
	//}

      /*
      ADIOS_VARINFO* v = adios_inq_var(f, varName1);
      doubleCheckWithIdxOnBlock(f, argv[1], blockNum, v, timestep, atof(lessThanVal));

      //uint64_t point[] = {33,31,31};
      //printf("testing global block #: %d\n", getGlobalBlockNumForPoint(v,point,timestep));

      adios_free_varinfo(v);
      */
    }
    
    //testNoBoxOnSelection(f, varName1);
    //testMultiBoundBox(f, varName1, varName1);
    //testAllDifferentBoundBoxes(f, varName1, varName2);
    //testUseOneWriteBlock(f, 0, varName1, varName2); 

    
    //testOneBoundBoxForAllVar(f, varName1, varName2); 
    //testOnePointList(f, varName1, varName2);

    adios_read_close(f);
    return 1;
#endif    
}
