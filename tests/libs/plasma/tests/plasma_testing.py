#! /usr/bin/env python
# -*- coding: utf-8 -*-


###############################################################################
# plasma_testing.py  [nbcores]
#  nbcores is a optional argument to give the number of cores to run the testing
# Example:
#     ./plasma_testing.py
#                 No argument, so will run on half of the core if the machine has more than 2 cores
#     ./plama_testing.py nbcores
#                 Will run on nbcores
###############################################################################

from subprocess import Popen, STDOUT, PIPE
import os, sys, math
import getopt

# Linux Unix and MacOS:
if hasattr(os, "sysconf"):
   if os.sysconf_names.has_key("SC_NPROCESSORS_ONLN"):
           ncpus_av = os.sysconf("SC_NPROCESSORS_ONLN")
# Windows:
if os.environ.has_key("NUMBER_OF_PROCESSORS"):
   ncpus_av = int(os.environ["NUMBER_OF_PROCESSORS"]);

# we are going to run on half of the cores by default if we have more than 2 cores
if (ncpus_av > 2):
   ncpus=int(math.floor(ncpus_av/2))
else:
   ncpus=ncpus_av

try:
   opts, args = getopt.getopt(sys.argv[1:], "hnc:s:",
                              ["help", "cores=", "sched="])

except getopt.error, msg:
   print msg
   print "for help use --help"
   sys.exit(2)

# process options
sched=0;
execution=1;

for o, a in opts:
   if o in ("-h", "--help"):
      print sys.argv[0]+" [-h|--help] [-c n|--cores=n] [-s s|--sched=s]"
      print "  -c Fix the number of cores"
      print "  -s Specify the scheduling mode: 0 for static, 1 for dynamic"
      print "  -n Print the commands only"
      sys.exit(0)
   else:
      if o in ( '-c', '--ncores' ):
         ncpus = a
      elif o in ( '-s' , '--sched' ):
         if a == "0" :
            sched = 0
         else:
            sched = 1
      elif o in ( '-n' , '--noexec' ):
         execution=0

# Add current directory to the path for subshells of this shell
# Allows the popen to find local files in both windows and unixes
os.environ["PATH"] = os.environ["PATH"]+":."

# Define a function to open the executable (different filenames on unix and Windows)
def local_popen( f, cmdline ):
   if os.name != 'nt':
      cmdline="./" + cmdline

   if execution==0:
      print cmdline
   else:
      p=Popen( cmdline, shell=True, stdout=PIPE, stderr=STDOUT )

      r=p.poll()
      while r == None:
         r=p.poll()
      pipe=p.stdout

      if r != 0:
         print "---- TESTING " + cmdline.split()[3] + "... FAILED(" + str(p.returncode) +") !"
         for line in pipe.readlines():
            f.write(str(line))
      else:
         found=0
         for line in pipe.readlines():
            f.write(str(line))
            if "TESTING" in line :
               found = 1
               print line,
         if found == 0:
            print cmdline.split()[0] + " " + cmdline.split()[3] + ": FAILED(Unexpected error)"

   f.flush();
   return 0


# If filename cannot be opened, send output to sys.stderr
filename = "testing_results.txt"
try:
     f = open(filename, 'w')
except IOError:
     f = sys.stdout

print " "
print "---------------- Testing PLASMA Routines ----------------"
print " "
print "-- Number of cores available =", ncpus_av
print "-- Number of cores used for testing =", ncpus
print "-- Detailed results are stored in", filename
print "-- Scheduling mode: ",
if (sched == 0):
   print "Static"
else:
   print "Dynamic"

dtypes = (
("s", "d", "c", "z"),
("Single", "Double", "Complex", "Double Complex"),
)

for dtype in range(4):
  letter = dtypes[0][dtype]
  name = dtypes[1][dtype]

  print " "
  print "--------------------- In Place Transformation -------------------"
  print " "
  sys.stdout.flush()

  cmdbase="%stesting " % letter + str(ncpus) + " " + str(sched)

  test01=local_popen(f, cmdbase + " GECFI" + " 623 531 123 145 136 134")
  test02=local_popen(f, cmdbase + " GETMI" + " 623 531 123 145")

  print " "
  print "------------------------- %s ------------------------" % name
  print " "
  sys.stdout.flush()

  test0 = local_popen(f, cmdbase + " LANGE" + " 914 510 950")
  test1 = local_popen(f, cmdbase + " GEMM"  + " 1.0 -2.0 600 500 550 650 625 700")
  test2 = local_popen(f, cmdbase + " TRSM"  + " -2.0 600 500 650 625")
  test3 = local_popen(f, cmdbase + " TRMM"  + " -2.0 600 500 650 625")
  test4 = local_popen(f, cmdbase + " SYMM"  + " 1.0 -2.0 600 500 650 625 700")
  test5 = local_popen(f, cmdbase + " SYRK"  + " 1.0 -2.0 600 500 650 625")
  test6 = local_popen(f, cmdbase + " SYR2K" + " 1.0 -2.0 600 500 650 625 613")

  if letter in ( "c", "z" ) :
     test101 = local_popen(f, cmdbase + " HEMM" + " 1.0 -2.0 600 500 650 625 600")
     test102 = local_popen(f, cmdbase + " HERK" + " 1.0 -2.0 600 500 650 625")
     test103 = local_popen(f, cmdbase + " HER2K" + " 1.0 -2.0 600 500 650 625 613")

  test20 = local_popen(f, cmdbase + " POSV"  + " 500 600 25 700")
  test21 = local_popen(f, cmdbase + " POTRI" + " 500 600")
  test22 = local_popen(f, cmdbase + " GELS"  + " 0 800 400 825 25 810")
  test23 = local_popen(f, cmdbase + " GELS"  + " 1 800 400 825 25 810 4")
  test24 = local_popen(f, cmdbase + " GELS"  + " 0 400 800 825 25 810")
  test25 = local_popen(f, cmdbase + " GELS"  + " 1 400 800 825 25 810 4")
  test26 = local_popen(f, cmdbase + " GESV"  + " 800 825 25 810")
  test27 = local_popen(f, cmdbase + " GESV_INCPIV"  + " 800 825 25 810")
  test28 = local_popen(f, cmdbase + " GETRI" + " 800 825")
  test29 = local_popen(f, cmdbase + " GESVD" + " 0 825 800 855")
  test30 = local_popen(f, cmdbase + " GESVD" + " 0 800 825 810")
  test31 = local_popen(f, cmdbase + " HEGV"  + " 800 825 810")
  test32 = local_popen(f, cmdbase + " HEEV"  + " 800 825")
  test33 = local_popen(f, cmdbase + " HEGST" + " 800 825 810")
  sys.stdout.flush()

print " "
print "--------------------- Mixed Precision -------------------"
print " "

for substr in ( ("z", "C"), ("d", "S") ):
  cmdbase="%stesting " % substr[0] + str(ncpus) + " " + str(sched)

  test201 = local_popen(f, cmdbase + " %sGESV"   % substr[1] + " 800 825 25 810")
  test202 = local_popen(f, cmdbase + " %sUNGESV" % substr[1] + " 800 825 25 810")
  test203 = local_popen(f, cmdbase + " %sPOSV"   % substr[1] + " 800 825 25 810")
  sys.stdout.flush()



# This may close the sys.stdout stream, so make it the last statement
f.close()
