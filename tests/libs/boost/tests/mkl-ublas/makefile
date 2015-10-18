##******************************************************************************
##  Copyright(C) 2008-2013 Intel Corporation. All Rights Reserved.
##  
##  The source code, information  and  material ("Material") contained herein is
##  owned  by Intel Corporation or its suppliers or licensors, and title to such
##  Material remains  with Intel Corporation  or its suppliers or licensors. The
##  Material  contains proprietary information  of  Intel or  its  suppliers and
##  licensors. The  Material is protected by worldwide copyright laws and treaty
##  provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
##  modified, published, uploaded, posted, transmitted, distributed or disclosed
##  in any way  without Intel's  prior  express written  permission. No  license
##  under  any patent, copyright  or  other intellectual property rights  in the
##  Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
##  implication, inducement,  estoppel or  otherwise.  Any  license  under  such
##  intellectual  property  rights must  be express  and  approved  by  Intel in
##  writing.
##  
##  *Third Party trademarks are the property of their respective owners.
##  
##  Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
##  this  notice or  any other notice embedded  in Materials by Intel or Intel's
##  suppliers or licensors in any way.
##
##******************************************************************************
##  Content:
##      Intel(R) Math Kernel Library UBLAS examples creation and run
##******************************************************************************

help:
	@echo "Usage: make {libia32|soia32|libintel64|sointel64} [function=name]"
	@echo "[compiler=compiler_name] [interface=interface_name] [threading=threading_name]"
	@echo
	@echo "name     - function name. Please see ublas.lst file"
	@echo
	@echo "compiler_name     - can be gnu or intel. Default value is intel."
	@echo "           Intel(R) C Compiler as default"
	@echo
	@echo "interface_name    - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo
	@echo "threading_name    - can be parallel or sequential. Default value is parallel."
	@echo "BOOST_ROOT=path   - path to Boost installation directory."

##------------------------------------------------------------------------------
## examples of using:
##
## make soia32 compiler=gnu         - build  by  GNU  C  compiler  and  run  all
##                                    examples of MKL  for 32-bit  applications,
##                                    dynamic linking
##
## make libintel64 compiler=gnu     - build  by  GNU  C  compiler  and  run  all
##                                    examples of MKL  for Intel(R) 64 processor
##                                    family applications, static linking
##
## make sointel64                   - build  by  Intel(R) C Compiler  (as default)
##                                    and run all examples of MKL for Intel(R) 64
##                                    processor  family  applications,   dynamic
##                                    linking
##------------------------------------------------------------------------------

include ublas.lst

ifndef function
function = $(UBLAS)
endif

ifndef compiler
compiler=intel
endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parallel
endif

RES = $(addsuffix .res ,$(function))

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib/$(_IA)"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib/$(_IA)"

ifeq ($(compiler),gnu)
CC=gcc
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
GNU_LIBS=-lstdc++
else
override CC=icc
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
COPTS = -DMKL_ILP64
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
COPTS =
endif

ifeq ($(_IA),ia32)
   ifeq ($(compiler),intel)
       SPEC_OPT=-mia32
#This option is required by Intel(R) 11.0 compiler to produce workable binaries for Pentium(R) III.
#If you don't need it, you can remove this option.
   endif
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
COPTS =
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
OMP_LIB =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
OMP_LIB = -L$(CMPLR_PATH) -liomp5
endif

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) -Wl,--start-group $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(OMP_LIB)

ifeq ($(_IA),ia32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

BOOST_ROOT = .

#UOPTS = -DNDEBUG -I$(BOOST_ROOT)
# (TRon Mar 17 2015: Added dir for the added mkl header files) 
UOPTS = -DNDEBUG -I$(BOOST_ROOT) -I$(BOOST_ROOT)/boost/intel-mkl

SIZE1 = 400
SIZE2 = 600
ITERATIONS = 100

libia32 lib32:
	$(MAKE) $(RES) _IA=ia32 EXT=a RES_EXT=lib
soia32 so32:
	$(MAKE) $(RES) _IA=ia32 EXT=so RES_EXT=so
libintel64 libem64t:
	$(MAKE) $(RES) _IA=intel64 EXT=a RES_EXT=lib
sointel64 soem64t:
	$(MAKE) $(RES) _IA=intel64 EXT=so RES_EXT=so

#-------------------------------------------------------------------------------

vpath %.cpp source

$(RES): %.res: %.cpp
	mkdir -p ./$(RES_DIR)
	$(CC) $(SPEC_OPT) $(COPTS) $(UOPTS) -w -I"$(MKLROOT)/include" $< $(GNU_LIBS) -o $(RES_DIR)/$*_ublas.out
	$(CC) $(SPEC_OPT) $(COPTS) -DMKL $(UOPTS) -w -I"$(MKLROOT)/include" $< -L$(MKL_PATH) $(MKL_LIBS) $(GNU_LIBS) -lpthread -lm -ldl -o $(RES_DIR)/$*_mkl.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH):$(CMPLR_PATH); $(RES_DIR)/$*_ublas.out $(ITERATIONS) $(SIZE1) $(SIZE2) >$(RES_DIR)/$*_ublas.res
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH):$(CMPLR_PATH); $(RES_DIR)/$*_mkl.out $(ITERATIONS) $(SIZE1) $(SIZE2) >$(RES_DIR)/$*_mkl.res
#-------------------------------------------------------------------------------
