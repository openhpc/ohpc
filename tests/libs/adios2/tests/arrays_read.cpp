/*
 * Distributed under the OSI-approved Apache License, Version 2.0.  See
 * accompanying file Copyright.txt for details.
 *
 * helloBPReader.cpp: Simple self-descriptive example of how to read a variable
 * to a BP File.
 *
 * Try running like this from the build directory:
 *   mpirun -np 3 ./bin/hello_bpReader
 *
 *  Created on: Feb 16, 2017
 *      Author: William F Godoy godoywf@ornl.gov
 *
 * Copied from https://github.com/ornladios/ADIOS2/blob/v2.8.3/examples/hello/bpReader/helloBPReader.cpp
 */
#include <ios>      //std::ios_base::failure
#include <iostream> //std::cout
#include <mpi.h>
#include <stdexcept> //std::invalid_argument std::exception
#include <vector>

#include <adios2.h>

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    std::string filename = "myVector_cpp.bp";
    try
    {
        /** ADIOS class factory of IO class objects */
        adios2::ADIOS adios(MPI_COMM_WORLD);

        /*** IO class object: settings and factory of Settings: Variables,
         * Parameters, Transports, and Execution: Engines */
        adios2::IO bpIO = adios.DeclareIO("ReadBP");

        /** Engine derived class, spawned to start IO operations */
        adios2::Engine bpReader = bpIO.Open(filename, adios2::Mode::ReadRandomAccess);

        const std::map<std::string, adios2::Params> variables =
            bpIO.AvailableVariables();

        for (const auto &variablePair : variables)
        {
            std::cout << "Name: " << variablePair.first;

            for (const auto &parameter : variablePair.second)
            {
                std::cout << "\t" << parameter.first << ": " << parameter.second
                          << "\n";
            }
        }

        /** Write variable for buffering */
        adios2::Variable<float> bpFloats =
            bpIO.InquireVariable<float>("bpFloats");
        adios2::Variable<int> bpInts = bpIO.InquireVariable<int>("bpInts");

        const std::size_t Nx = 10;
        if (bpFloats) // means found
        {
            std::vector<float> myFloats;

            // read only the chunk corresponding to our rank
            bpFloats.SetSelection({{Nx * rank}, {Nx}});
            // myFloats.data is pre-allocated
            bpReader.Get<float>(bpFloats, myFloats, adios2::Mode::Sync);

            if (rank == 0)
            {
                std::cout << "MyFloats: \n";
                for (const auto number : myFloats)
                {
                    std::cout << number << " ";
                }
                std::cout << "\n";
            }
        }

        if (bpInts) // means not found
        {
            std::vector<int> myInts;
            // read only the chunk corresponding to our rank
            bpInts.SetSelection({{Nx * rank}, {Nx}});

            bpReader.Get<int>(bpInts, myInts, adios2::Mode::Sync);

            if (rank == 0)
            {
                std::cout << "myInts: \n";
                for (const auto number : myInts)
                {
                    std::cout << number << " ";
                }
                std::cout << "\n";
            }
        }

        /** Close bp file, engine becomes unreachable after this*/
        bpReader.Close();
    }
    catch (std::invalid_argument &e)
    {
        if (rank == 0)
        {
            std::cerr
                << "Invalid argument exception, STOPPING PROGRAM from rank "
                << rank << "\n";
            std::cerr << e.what() << "\n";
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    catch (std::ios_base::failure &e)
    {
        if (rank == 0)
        {
            std::cerr << "IO System base failure exception, STOPPING PROGRAM "
                         "from rank "
                      << rank << "\n";
            std::cerr << e.what() << "\n";
            std::cerr << "The file " << filename << " does not exist."
                      << " Presumably this is because hello_bpWriter has not "
                         "been run."
                      << " Run ./hello_bpWriter before running this program.\n";
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    catch (std::exception &e)
    {
        if (rank == 0)
        {
            std::cerr << "Exception, STOPPING PROGRAM from rank " << rank
                      << "\n";
            std::cerr << e.what() << "\n";
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Finalize();

    return 0;
}
