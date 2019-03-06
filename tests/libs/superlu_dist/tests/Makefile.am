AM_FCFLAGS = "-I ."
superlupara.o: superlupara.f90
superlu_mod.o: superlu_mod.f90

check_PROGRAMS                = superlupara
superlupara_SOURCES           = superlupara.f90
superlupara_FCFLAGS           = -c

check_PROGRAMS               += superlu_mod
superlu_mod_SOURCES           = superlu_mod.f90
superlu_mod_FCFLAGS           = -c

LIBS = -lstdc++

check_PROGRAMS               += pddrive
pddrive_SOURCES               = pddrive.c dcreate_matrix.c

check_PROGRAMS               += pddrive_ABglobal
pddrive_ABglobal_SOURCES      = pddrive_ABglobal.c

check_PROGRAMS               += pddrive1
pddrive1_SOURCES              = pddrive1.c dcreate_matrix.c

check_PROGRAMS               += pddrive1_ABglobal
pddrive1_ABglobal_SOURCES     = pddrive1_ABglobal.c

check_PROGRAMS               += pddrive2
pddrive2_SOURCES              = pddrive2.c dcreate_matrix.c dcreate_matrix_perturbed.c

check_PROGRAMS               += pddrive2_ABglobal
pddrive2_ABglobal_SOURCES     = pddrive2_ABglobal.c

check_PROGRAMS               += pddrive3
pddrive3_SOURCES              = pddrive3.c dcreate_matrix.c

check_PROGRAMS               += pddrive3_ABglobal
pddrive3_ABglobal_SOURCES     = pddrive3_ABglobal.c

check_PROGRAMS               += pddrive4
pddrive4_SOURCES              = pddrive4.c dcreate_matrix.c

check_PROGRAMS               += pddrive4_ABglobal
pddrive4_ABglobal_SOURCES     = pddrive4_ABglobal.c

check_PROGRAMS               += f_5x5
f_5x5_SOURCES                 = f_5x5.f90 superlupara.f90 superlu_mod.f90 sp_ienv.c dcreate_dist_matrix.c superlu_c2f_dwrap.c

TESTS               = test_module
TESTS              += rm_execution




