#!/usr/bin/env python
#usage: validate_minidft.py filename

import sys

#parse options (just filenames)
fname = sys.argv[1]
f = open( fname, 'r' )

#determine which benchmark is being tested
measured = {}
for L in f:

    if( "number of atoms/cell" in L ):
        measured['nat'] = int( L.split("=")[1] )
        continue

    if( "number of electrons" in L ):
        measured['nel'] = float( L.split("=")[1] )
        continue

    if( "number of Kohn-Sham states" in L ):
        measured['nks'] = int( L.split("=")[1] )
        continue

    if( "kinetic-energy cutoff" in L ):
        measured['gcut'] = float( L.split("=")[1].split()[0] )
        continue

    if( "number of atomic types" in L ):
        measured['nsp'] = int( L.split("=")[1] )
        continue

    if( "atomic species   valence    mass     pseudopotential" in L ):
        measured['sp'] = []
        for i in range( measured['nsp'] ):
            L = f.next()
            measured['sp'].append( L )
        #end for i
        continue

    #Etot will be overwritten after each SCF cycle
    if( "total energy" in L ):
        if( "The total energy is the sum" in L ): continue
        L = L[1:]    #pop the leading "!" that appears when SCF converges
        measured['Etot'] = float( L.split("=")[1].split()[0] )
        continue
    
    if( "Benchmark_Time" in L ):
        Ntime_str = L.split()[4]
        Ntime_h = 0.0
        if( 'h' in Ntime_str ):
            Ntime_h, Ntime_str = Ntime_str.split('h')
            Ntime_h = float(Ntime_h)
        Ntime_m = 0.0
        if( 'm' in Ntime_str ):
            Ntime_m, Ntime_str = Ntime_str.split('m')
            Ntime_m = float(Ntime_m)
        Ntime_s = 0.0
        if( 's' in Ntime_str ):
            Ntime_s, Ntime_str = Ntime_str.split('s')
            Ntime_s = float(Ntime_s)
        measured['Ntime'] = ( ( Ntime_h ) * 60 + Ntime_m ) * 60 + Ntime_s
        continue

#end for L in f
f.close()

#define reference cases
reference = {}

reference['titania']  = { 'input': 'titania_3_120.in', 
                          'Etot':  -9738.99021176,
                          'nat' : 162,
                          'nel' : 1296,
                          'nks' : 648,
                          'gcut': 120.0,
                          'sp'  : ['        Ti            12.00    47.86700     Ti( 1.00)\n',
                                   '        O              6.00    15.99940      O( 1.00)\n' ]
                          }

reference['magnesia'] = { 'input': 'magnesia_10_130.in',
                          'Etot': -33817.52389012,
                          'nat' : 2000,
                          'nel' : 8000,
                          'nks' : 4000,
                          'gcut': 130.0,
                          'sp'  : ['        Mg             2.00    24.30500     Mg( 1.00)\n',
                                   '        O              6.00    15.99900      O( 1.00)\n' ]
                          }

#match job characteristics to benchmark
for refid in reference:
    refdat = reference[refid]

    ref_eq_meas = True
    for k in ['nat','nel','nks','gcut','sp']:
        ref_eq_meas &= ( measured[k] == refdat[k] )
    
    if( ref_eq_meas ): break
#end for refid

if( ref_eq_meas ):
    print "Identified benchmark:", refdat['input']
else:
    print "Error: could not find corresponding benchmark"
    exit()

#check accuracy ( computed, expected, difference, tolerance, pass/fail )
dEtot = abs( refdat['Etot'] - measured['Etot'] )
dEtol = 1.0e-6
print "Reference Energy: %15.8f Ry" % refdat['Etot']
print "Computed  Energy: %15.8f Ry" % measured['Etot']
print "         Abs Err: %15.8f Ry" % dEtot
print "       Tolerance: %15.8f Ry" % dEtol
if( dEtot < dEtol ):
    print "PASS"
else:
    print "FAIL"

#print time
#QE does not print Benchmark_Time
#print "Benchmark_Time:", measured['Ntime'], "seconds"




 
