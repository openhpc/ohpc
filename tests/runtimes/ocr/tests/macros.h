/*
 Author: David S Scott
 Copyright Intel Corporation 2015

 This file is subject to the license agreement located in the file ../../../../LICENSE (apps/LICENSE)
 and cannot be distributed without it. This notice cannot be removed or modified.
*/

/*
an OCR "library" for computing global reductions using labeled GUIDs.


The library implements a reduction tree invoking the reduction operator at each stage.
Node zero returns the result by satisfying the event with a separate datablock

See README for more details

*/


#define DEPVDEF(name) name##DEPV_t*name##DEPV=(name##DEPV_t*)depv
#define DEPV(name,var,field) ((name##DEPV->var).field)
#define DEPVARRAY(name,var,index,field) ((name##DEPV->var[index]).field)
#define DEPVNUM(name) (sizeof(name##DEPV_t)/sizeof(ocrEdtDep_t))
#define SLOT(name,var) (offsetof(name##DEPV_t,var)/sizeof(ocrEdtDep_t))
#define SLOTARRAY(name,var,index) ((offsetof(name##DEPV_t,var)+index*sizeof(ocrEdtDep_t))/sizeof(ocrEdtDep_t))
#define PRMDEF(name) name##PRM_t*name##PRM=(name##PRM_t*)paramv
#define PRMNUM(name) ((sizeof(name##PRM_t) + sizeof(u64) -1)/sizeof(u64))
#define PRM(name,var) (name##PRM->var)
#define PRMARRAY(name,var,index) (name##PRM->var[index])


