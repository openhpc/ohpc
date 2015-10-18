include ../../common/GRAPPA.defs
##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS) -O3
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

RADIUSFLAG  = -DRADIUS=2
#description: default radius of filter to be applied is 2

DOUBLEFLAG = -DDOUBLE
#description: default data type is single precision

STARFLAG   = -DSTAR
#description: default stencil is compact (dense, square)

#DEBUGFLAG   = -DVERBOSE
#description: default diagnostic style is silent

USERFLAGS    = 
#description: parameter to specify optional flags

#set the following variables for custom libraries and/or other objects
EXTOBJS      = 
LIBS         = -Wl,-Bstatic  -lGrappa -lglog -lgflags -Wl,-Bdynamic -lpthread -lboost_system-mt 
LIBPATHS     = -L$(GRAPPATOP)/build/Make+Release/system \
               -L$(GRAPPATOP)/build/Make+Release/third-party/lib 
INCLUDEPATHS = -I$(GRAPPATOP)/build/Make+Release/third-party/include \
               -I$(GRAPPATOP)/system \
               -I$(GRAPPATOP)/system/tasks

### End User configurable options ###

# this is used by the C++ implicit compile rule
CXXFLAGS= -DENABLE_RDMA_AGGREGATOR  -std=c++11 -Winline -Wno-inline -mno-red-zone -g -DNDEBUG  

TUNEFLAGS    = $(RESTRICTFLAG) $(DEBUGFLAG) $(RADIUSFLAG)  $(USERFLAGS) $(CXXFLAGS)
PROGRAM     = stencil
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
