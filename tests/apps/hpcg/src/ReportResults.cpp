
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

/*!
 @file ReportResults.cpp

 HPCG routine
 */

#ifndef HPCG_NOMPI
#include <mpi.h> // If this routine is not compiled with HPCG_NOMPI
#endif

#include "ReportResults.hpp"
#include "YAML_Element.hpp"
#include "YAML_Doc.hpp"

#ifdef HPCG_DEBUG
#include <fstream>
using std::endl;

#include "hpcg.hpp"
#endif

/*!
 Creates a YAML file and writes the information about the HPCG run, its results, and validity.

  @param[in] geom The description of the problem's geometry.
  @param[in] A    The known system matrix
  @param[in] numberOfMgLevels Number of levels in multigrid V cycle
  @param[in] numberOfCgSets Number of CG runs performed
  @param[in] niters Number of preconditioned CG iterations performed to lower the residual below a threshold
  @param[in] times  Vector of cumulative timings for each of the phases of a preconditioned CG iteration
  @param[in] testcg_data    the data structure with the results of the CG-correctness test including pass/fail information
  @param[in] testsymmetry_data the data structure with the results of the CG symmetry test including pass/fail information
  @param[in] testnorms_data the data structure with the results of the CG norm test including pass/fail information
  @param[in] global_failure indicates whether a failure occured during the correctness tests of CG

  @see YAML_Doc
*/
void ReportResults(const SparseMatrix & A, int numberOfMgLevels, int numberOfCgSets, int refMaxIters,int optMaxIters, double times[],
		const TestCGData & testcg_data, const TestSymmetryData & testsymmetry_data, const TestNormsData & testnorms_data, int global_failure) {

  double minOfficialTime = 3600; // Any official benchmark result much run at least this many seconds

#ifndef HPCG_NOMPI
  double t4 = times[4];
  double t4min = 0.0;
  double t4max = 0.0;
  double t4avg = 0.0;
  MPI_Allreduce(&t4, &t4min, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);
  MPI_Allreduce(&t4, &t4max, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
  MPI_Allreduce(&t4, &t4avg, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  t4avg = t4avg/((double) A.geom->size);
#endif

  // initialize YAML doc

  if (A.geom->rank==0) { // Only PE 0 needs to compute and report timing results

    double fNumberOfCgSets = numberOfCgSets;
    double fniters = fNumberOfCgSets * (double) optMaxIters;
    double fnrow = A.totalNumberOfRows;
    double fnnz = A.totalNumberOfNonzeros;

    // Op counts come from implementation of CG in CG.cpp (include 1 extra for the CG preamble ops)
    double fnops_ddot = (3.0*fniters+fNumberOfCgSets)*2.0*fnrow; // 3 ddots with nrow adds and nrow mults
    double fnops_waxpby = (3.0*fniters+fNumberOfCgSets)*2.0*fnrow; // 3 WAXPBYs with nrow adds and nrow mults
    double fnops_sparsemv = (fniters+fNumberOfCgSets)*2.0*fnnz; // 1 SpMV with nnz adds and nnz mults
    // Op counts from the multigrid preconditioners
    double fnops_precond = 0.0;
    const SparseMatrix * Af = &A;
    for (int i=1; i<numberOfMgLevels; ++i) {
        double fnnz_Af = Af->totalNumberOfNonzeros;
        double fnumberOfPresmootherSteps = Af->mgData->numberOfPresmootherSteps;
        double fnumberOfPostsmootherSteps = Af->mgData->numberOfPostsmootherSteps;
        fnops_precond += fnumberOfPresmootherSteps*fniters*4.0*fnnz_Af; // number of presmoother flops
        fnops_precond += fniters*2.0*fnnz_Af; // cost of fine grid residual calculation
        fnops_precond += fnumberOfPostsmootherSteps*fniters*4.0*fnnz_Af;  // number of postsmoother flops
    	Af = Af->Ac; // Go to next coarse level
    }

    fnops_precond += fniters*4.0*((double) Af->totalNumberOfNonzeros); // One symmetric GS sweep at the coarsest level
    double fnops = fnops_ddot+fnops_waxpby+fnops_sparsemv+fnops_precond;
    double reffnops = fnops * ((double) refMaxIters)/((double) optMaxIters);

    YAML_Doc doc("HPCG-Benchmark", "2.4");
    doc.add("HPCG Benchmark","Version 2.4 June 3, 2014");

    doc.add("Machine Summary","");
    doc.get("Machine Summary")->add("Distributed Processes",A.geom->size);
    doc.get("Machine Summary")->add("Threads per processes",A.geom->numThreads);

    doc.add("Global Problem Dimensions","");
    doc.get("Global Problem Dimensions")->add("Global nx",A.geom->npx*A.geom->nx);
    doc.get("Global Problem Dimensions")->add("Global ny",A.geom->npy*A.geom->ny);
    doc.get("Global Problem Dimensions")->add("Global nz",A.geom->npz*A.geom->nz);

    doc.add("Processor Dimensions","");
    doc.get("Processor Dimensions")->add("npx",A.geom->npx);
    doc.get("Processor Dimensions")->add("npy",A.geom->npy);
    doc.get("Processor Dimensions")->add("npz",A.geom->npz);

    doc.add("Local Domain Dimensions","");
    doc.get("Local Domain Dimensions")->add("nx",A.geom->nx);
    doc.get("Local Domain Dimensions")->add("ny",A.geom->ny);
    doc.get("Local Domain Dimensions")->add("nz",A.geom->nz);

    doc.add("********** Problem Summary  ***********","");

    doc.add("Linear System Information","");
    doc.get("Linear System Information")->add("Number of Equations",A.totalNumberOfRows);
    doc.get("Linear System Information")->add("Number of Nonzero Terms",A.totalNumberOfNonzeros);

    doc.add("Multigrid Information","");
    doc.get("Multigrid Information")->add("Number of coarse grid levels", numberOfMgLevels-1);
    Af = &A;
    doc.get("Multigrid Information")->add("Coarse Grids","");
    for (int i=1; i<numberOfMgLevels; ++i) {
        doc.get("Multigrid Information")->get("Coarse Grids")->add("Grid Level",i);
        doc.get("Multigrid Information")->get("Coarse Grids")->add("Number of Equations",Af->Ac->totalNumberOfRows);
        doc.get("Multigrid Information")->get("Coarse Grids")->add("Number of Nonzero Terms",Af->Ac->totalNumberOfNonzeros);
        doc.get("Multigrid Information")->get("Coarse Grids")->add("Number of Presmoother Steps",Af->mgData->numberOfPresmootherSteps);
        doc.get("Multigrid Information")->get("Coarse Grids")->add("Number of Postsmoother Steps",Af->mgData->numberOfPostsmootherSteps);
    	Af = Af->Ac;
    }

    doc.add("********** Validation Testing Summary  ***********","");
    doc.add("Spectral Convergence Tests","");
    if (testcg_data.count_fail==0)
      doc.get("Spectral Convergence Tests")->add("Result", "PASSED");
    else
      doc.get("Spectral Convergence Tests")->add("Result", "FAILED");
    doc.get("Spectral Convergence Tests")->add("Unpreconditioned","");
    doc.get("Spectral Convergence Tests")->get("Unpreconditioned")->add("Maximum iteration count", testcg_data.niters_max_no_prec);
    doc.get("Spectral Convergence Tests")->get("Unpreconditioned")->add("Expected iteration count", testcg_data.expected_niters_no_prec);
    doc.get("Spectral Convergence Tests")->add("Preconditioned","");
    doc.get("Spectral Convergence Tests")->get("Preconditioned")->add("Maximum iteration count", testcg_data.niters_max_prec);
    doc.get("Spectral Convergence Tests")->get("Preconditioned")->add("Expected iteration count", testcg_data.expected_niters_prec);

    const char DepartureFromSymmetry[] = "Departure from Symmetry |x'Ay-y'Ax|/(2*||x||*||A||*||y||)/epsilon";
    doc.add(DepartureFromSymmetry,"");
    if (testsymmetry_data.count_fail==0)
      doc.get(DepartureFromSymmetry)->add("Result", "PASSED");
    else
      doc.get(DepartureFromSymmetry)->add("Result", "FAILED");
    doc.get(DepartureFromSymmetry)->add("Departure for SpMV", testsymmetry_data.depsym_spmv);
    doc.get(DepartureFromSymmetry)->add("Departure for MG", testsymmetry_data.depsym_mg);

    doc.add("********** Iterations Summary  ***********","");
    doc.add("Iteration Count Information","");
    if (!global_failure)
      doc.get("Iteration Count Information")->add("Result", "PASSED");
    else
      doc.get("Iteration Count Information")->add("Result", "FAILED");
    doc.get("Iteration Count Information")->add("Reference CG iterations per set", refMaxIters);
    doc.get("Iteration Count Information")->add("Optimized CG iterations per set", optMaxIters);
    doc.get("Iteration Count Information")->add("Total number of reference iterations", refMaxIters*numberOfCgSets);
    doc.get("Iteration Count Information")->add("Total number of optimized iterations", optMaxIters*numberOfCgSets);

    doc.add("********** Reproducibility Summary  ***********","");
    doc.add("Reproducibility Information","");
    if (testnorms_data.pass)
      doc.get("Reproducibility Information")->add("Result", "PASSED");
    else
      doc.get("Reproducibility Information")->add("Result", "FAILED");
    doc.get("Reproducibility Information")->add("Scaled residual mean", testnorms_data.mean);
    doc.get("Reproducibility Information")->add("Scaled residual variance", testnorms_data.variance);

    doc.add("********** Performance Summary (times in sec) ***********","");

    doc.add("Benchmark Time Summary","");
    doc.get("Benchmark Time Summary")->add("Optimization phase",times[7]);
    doc.get("Benchmark Time Summary")->add("DDOT",times[1]);
    doc.get("Benchmark Time Summary")->add("WAXPBY",times[2]);
    doc.get("Benchmark Time Summary")->add("SpMV",times[3]);
    doc.get("Benchmark Time Summary")->add("MG",times[5]);
    doc.get("Benchmark Time Summary")->add("Total",times[0]);

    doc.add("Floating Point Operations Summary","");
    doc.get("Floating Point Operations Summary")->add("Raw DDOT",fnops_ddot);
    doc.get("Floating Point Operations Summary")->add("Raw WAXPBY",fnops_waxpby);
    doc.get("Floating Point Operations Summary")->add("Raw SpMV",fnops_sparsemv);
    doc.get("Floating Point Operations Summary")->add("Raw MG",fnops_precond);
    doc.get("Floating Point Operations Summary")->add("Total",fnops);
    doc.get("Floating Point Operations Summary")->add("Total with convergence overhead",reffnops);

    doc.add("GFLOP/s Summary","");
    doc.get("GFLOP/s Summary")->add("Raw DDOT",fnops_ddot/times[1]/1.0E9);
    doc.get("GFLOP/s Summary")->add("Raw WAXPBY",fnops_waxpby/times[2]/1.0E9);
    doc.get("GFLOP/s Summary")->add("Raw SpMV",fnops_sparsemv/(times[3])/1.0E9);
    doc.get("GFLOP/s Summary")->add("Raw MG",fnops_precond/(times[5])/1.0E9);
    doc.get("GFLOP/s Summary")->add("Raw Total",fnops/times[0]/1.0E9);
    doc.get("GFLOP/s Summary")->add("Total with convergence overhead",reffnops/times[0]/1.0E9);
    // This final GFLOP/s rating includes the overhead of optimizing the data structures vs ten sets of 50 iterations of CG
    double totalGflops = reffnops/(times[0]+fNumberOfCgSets*times[7]/10.0)/1.0E9;
    doc.get("GFLOP/s Summary")->add("Total with convergence and optimization phase overhead",totalGflops);

    //double totalSparseMVTime = times[3] + times[6];
    //doc.add("Sparse Operations Overheads","");
    //doc.get("Sparse Operations Overheads")->add("SpMV GFLOP/s with overhead",fnops_sparsemv/(totalSparseMVTime)/1.0E9);
    //doc.get("Sparse Operations Overheads")->add("Overhead time (sec)", (times[7]+times[6]));
    //doc.get("Sparse Operations Overheads")->add("Overhead as percentage of time", (times[7]+times[6])/totalSparseMVTime*100.0);
    doc.add("User Optimization Overheads","");
    doc.get("User Optimization Overheads")->add("Optimization phase time (sec)", (times[7]));
    doc.get("User Optimization Overheads")->add("Optimization phase time vs reference SpMV+MG time", times[7]/times[8]);

#ifndef HPCG_NOMPI
    doc.add("DDOT Timing Variations","");
    doc.get("DDOT Timing Variations")->add("Min DDOT MPI_Allreduce time",t4min);
    doc.get("DDOT Timing Variations")->add("Max DDOT MPI_Allreduce time",t4max);
    doc.get("DDOT Timing Variations")->add("Avg DDOT MPI_Allreduce time",t4avg);

    //doc.get("Sparse Operations Overheads")->add("Halo exchange time (sec)", (times[6]));
    //doc.get("Sparse Operations Overheads")->add("Halo exchange as percentage of SpMV time", (times[6])/totalSparseMVTime*100.0);
#endif
    doc.add("********** Final Summary **********","");
    bool isValidRun = (testcg_data.count_fail==0) && (testsymmetry_data.count_fail==0) && (testnorms_data.pass) && (!global_failure);
    if (isValidRun) {
      doc.get("********** Final Summary **********")->add("HPCG result is VALID with a GFLOP/s rating of", totalGflops);
      if (!A.isDotProductOptimized) {
        doc.get("********** Final Summary **********")->add("Reference version of ComputeDotProduct used","Performance results are most likely suboptimal");
      }
      if (!A.isSpmvOptimized) {
        doc.get("********** Final Summary **********")->add("Reference version of ComputeSPMV used","Performance results are most likely suboptimal");
      }
      if (!A.isMgOptimized) {
        if (A.geom->numThreads>1)
          doc.get("********** Final Summary **********")->add("Reference version of ComputeMG used and number of threads greater than 1","Performance results are severely suboptimal");
        else // numThreads ==1
          doc.get("********** Final Summary **********")->add("Reference version of ComputeMG used","Performance results are most likely suboptimal");
      }
      if (!A.isWaxpbyOptimized) {
        doc.get("********** Final Summary **********")->add("Reference version of ComputeWAXPBY used","Performance results are most likely suboptimal");
      }
      if (times[0]>=minOfficialTime) {
    	  doc.get("********** Final Summary **********")->add("Please send the YAML file contents to","HPCG-Results@software.sandia.gov");
      }
      else {
          doc.get("********** Final Summary **********")->add("Results are valid but execution time (sec) is",times[0]);
          doc.get("********** Final Summary **********")->add("Official results execution time (sec) must be at least",minOfficialTime);

      }
    } else {
      doc.get("********** Final Summary **********")->add("HPCG result is","INVALID.");
      doc.get("********** Final Summary **********")->add("Please review the YAML file contents","You may NOT submit these results for consideration.");
    }

    std::string yaml = doc.generateYAML();
#ifdef HPCG_DEBUG
    HPCG_fout << yaml;
#endif
  }
  return;
}
