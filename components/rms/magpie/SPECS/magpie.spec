#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
#
#  Copyright (C) 2013-2015 Lawrence Livermore National Security, LLC.
#  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
#  Written by Albert Chu <chu11@llnl.gov>
#  LLNL-CODE-644248
#
#  This file is part of Magpie, scripts for running Hadoop on
#  traditional HPC systems.  For details, see https://github.com/llnl/magpie.
#
#  Magpie is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  Magpie is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Magpie.  If not, see <http://www.gnu.org/licenses/>.
#

%include %{_sourcedir}/OHPC_macros
%global pname magpie

Summary: Scripts for running Big Data software in HPC environments
Name: %{pname}%{PROJ_DELIM}
Version: 2.3
Release: 1%{?dist}
License: GPLv2
URL: https://github.com/LLNL/magpie
Group: %{PROJ_NAME}/rms
Source0: https://github.com/LLNL/magpie/archive/%{version}.tar.gz

# Java 8 or greater required on all cluster nodes.
# Java development package added to head node.
Requires: java-devel >= 1.8

#!BuildIgnore: post-build-checks

%global install_path %{OHPC_UTILS}/%{pname}

%description
Magpie contains a number of scripts for running Big Data software in HPC environments.
Thus far, Hadoop, Spark, Hbase, Storm, Pig, Mahout, Phoenix, Kafka, Zeppelin, and 
Zookeeper are supported. It currently supports running over the parallel file system 
Lustre and running over any generic network filesytem. There is scheduler/resource 
manager support for Slurm, Moab, Torque, and LSF.

%prep
%setup -q -n %{pname}-%{version}

%build
# Fix-up file permissions and shebang data that cause warnings/errors in CentOS 8
# Not `install` command in next section due to large file count
%{__chmod} -x magpie/job/magpie-job-ray-rayips.py
%{__chmod} -x magpie/job/magpie-job-tensorflow-horovod-synthetic-benchmark.py
%{__chmod} -x magpie/job/magpie-job-tensorflow-tfadd.py
%{__chmod} -x submission-scripts/script-templates/magpie-hive
%{__chmod} -x doc/README.hive
%{__chmod} -x conf/zookeeper/zookeeper-env.sh
%{__chmod} -x conf/zookeeper/zookeeper-master-env.sh
%{__chmod} -x conf/hive/hive-env.sh
%{__chmod} -x conf/hive/hive-log4j2.properties
%{__chmod} -x conf/hive/hive-site.xml
%{__chmod} -x conf/hive/postgresql.conf
%{__chmod} -x conf/hive/spark-hive-site.xml
%{__chmod} -x conf/hive/tez-site.xml
%{__chmod} -x conf/storm/storm-daemon-env.sh
%{__chmod} -x conf/hadoop/hadoop-env-2.X.sh
%{__chmod} -x conf/hadoop/yarn-env-2.X.sh
%{__chmod} -x conf/zeppelin/zeppelin-site.xml
%{__chmod} -x testsuite/testscripts/test-ray.py
%{__chmod} -x testsuite/testscripts/test-tensorflow.py
%{__chmod} -x examples/example-environment-extra
%{__sed} -i "s#/usr/bin/env bash#/usr/bin/bash#" conf/hadoop/hadoop-user-functions-3-X.sh
%{__sed} -i "s#/usr/bin/env bash#/usr/bin/bash#" conf/spark/spark-env-0.X.sh
%{__sed} -i "s#/usr/bin/env bash#/usr/bin/bash#" conf/spark/spark-env-1.X.sh
%{__sed} -i "s#/usr/bin/env bash#/usr/bin/bash#" conf/spark/spark-env-2.X.sh
%{__sed} -i "s#/usr/bin/env python#%{__python3}#" magpie/job/magpie-job-zeppelin-checkzeppelinup.py

%install
%{__mkdir} -p -m 775 ${RPM_BUILD_ROOT}%{install_path}
%{__cp} -a . ${RPM_BUILD_ROOT}%{install_path}

# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads Magpie scripts"
puts stderr " "
puts stderr "\nVersion %{version}\n"
}

module-whatis "Name: Magpie "
module-whatis "Version: %{version}"
module-whatis "Category: Resource Managers"
module-whatis "Description: Scripts for running Big Data in HPC environments"
module-whatis "URL: https://github.com/LLNL/magpie"

set     java_check                  [exec which javac]
if { [catch {exec \$java_check -version 2> /dev/null}] } {
   puts stderr "ERROR: Java not available."
} else {
   setenv          JAVA_HOME           [file dirname [file dirname [exec readlink -f \$java_check]]]
}

set     version			    %{version}
setenv          MAGPIE_PATH         %{install_path}
setenv          MAGPIE_SCRIPTS_HOME %{install_path}
setenv          %{PNAME}_DIR        %{install_path}
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%files
%dir %{install_path}
%{install_path}
%doc doc/* NEWS README.md TODO VERSION
%license COPYING DISCLAIMER
%{OHPC_MODULES}/%{pname}

