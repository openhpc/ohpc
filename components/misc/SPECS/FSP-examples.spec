Summary: Example source for use within FSP development environment
Name: FSP-examples
Version: 1.0
Release: 1
License: Intel
Group:   Development/Languages
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%{!?FSP_HOME: %define FSP_HOME /opt/fsp}

%description

Collection of simple example programs for use wihin FSP development
environment.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/%{FSP_HOME}/pub/examples/mpi
%{__cat} << EOF > %{buildroot}/%{FSP_HOME}/pub/examples/mpi/hello.c
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
  int num_procs, num_local;
  char mach_name[MPI_MAX_PROCESSOR_NAME];
  int mach_len;

  MPI_Init (&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank (MPI_COMM_WORLD, &num_local);
  MPI_Get_processor_name(mach_name,&mach_len);

  MPI_Barrier(MPI_COMM_WORLD);

  if(num_local == 0)
      printf("\n Hello, world (%i procs total)\n",num_procs);

  MPI_Barrier(MPI_COMM_WORLD);

  printf("    --> Process # %3i of %3i is alive. -> %s\n",
   num_local,num_procs,mach_name);

  MPI_Finalize();
  return 0;
}
EOF

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{FSP_HOME}
%dir %{FSP_HOME}/pub
%{FSP_HOME}/pub/examples



%changelog

