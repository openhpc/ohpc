### <img src="https://github.com/openhpc/ohpc/blob/master/docs/recipes/install/common/figures/ohpc_logo.png" width="170" valign="middle" hspace="5" alt="OpenHPC"/>

#### Community building blocks for HPC systems (v1.3.9)

[![Components](https://img.shields.io/badge/components%20available-89-green.svg) ](https://github.com/openhpc/ohpc/wiki/Component-List-v1.3.9)
[![Updates](https://img.shields.io/badge/updates-37%25-lightgrey.svg) ](https://github.com/openhpc/ohpc/releases/tag/v1.3.9.GA)
[![Tests](https://img.shields.io/badge/test%20coverage-%20%20%20-brightgreen.svg) ](http://test.openhpc.community:8080/job/1.3.x/view/1.3.9/)

#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, containers, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged binary RPMs available per architecture type are summarized as follows:

Base OS     | aarch64 | x86_64  | noarch
:---:       | :---:   | :---:   | :---:
CentOS 7.7  | 413     | 743     | 45
SLES 12 SP4 | 417     | 736     | 45

A list of all available components is available on the OpenHPC
[wiki](https://github.com/openhpc/ohpc/wiki/Component-List-v1.3.9), and
specific versioning details can be found in the "Package Manifest" appendix
located in each of the companion install guide documents. A list of updated
packages can be found in the [release
notes](https://github.com/openhpc/ohpc/releases/tag/v1.3.9.GA). The release
notes also contain important information for those upgrading from previous
versions of OpenHPC.

#### Getting started

OpenHPC provides pre-built binaries via repositories for use with standard
Linux package manager tools (e.g. ```yum``` or ```zypper```). Package
repositories are housed at https://build.openhpc.community. To get started, you
can enable an OpenHPC repository locally through installation of an
```ohpc-release``` RPM which includes gpg keys for package signing and defines
the URL locations for [base] and [update] package repositories. Copies of the
```ohpc-release``` package are provided below for convenience. Installation guides 
tailored for each supported resource manager are also available below or in
the downloads section of the latest
[release](https://github.com/openhpc/ohpc/releases/tag/v1.3.9.GA). Instructions
for mirroring the OpenHPC repositories with rsync can be found in the
[wiki](https://github.com/openhpc/ohpc/wiki/Repository-Access-via-rsync).

---

#### Architecture = (aarch64)

###### [CentOS 7.7]
* [Latest version of 'ohpc-release' for EL7](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.aarch64.rpm) (md5sum=16ad76e74b591a3b6dcc3cb8597d3f7d)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-Warewulf-PBSPro-1.3.9-aarch64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.9-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.9/OpenHPC-1.3.9.CentOS_7.aarch64.tar) mirror of yum repository (md5sum=f6c3b935eb52931f0b9a142826bd5e0e))

###### [SLES 12 SP4]
* [Latest version of 'ohpc-release' for SLE_12](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.aarch64.rpm) (md5sum=706a42f7785952f8b543c501eeec05da)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-SLE_12-Warewulf-PBSPro-1.3.9-aarch64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.9-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.9/OpenHPC-1.3.9.SLE_12.aarch64.tar) mirror of zypper repository (md5sum=9d68dcda779dbd8f255423f71d55d2fa)

---

#### Architecture = (x86_64)

###### [CentOS 7.7]
* [Latest version of 'ohpc-release' for EL7](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.x86_64.rpm) (md5sum=d5139cf3aa83d095e6851628e8a684fa)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-Warewulf-PBSPro-1.3.9-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.9-x86_64.pdf)
* [Install Guide (with xCAT (Stateless) + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-xCAT-Stateless-SLURM-1.3.9-x86_64.pdf)
* [Install Guide (with xCAT (Stateful) + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-CentOS7-xCAT-Stateful-SLURM-1.3.9-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.9/OpenHPC-1.3.9.CentOS_7.x86_64.tar) mirror of yum repository (md5sum=2a97a0995b5b046f36e83dec76e4cd4d))

###### [SLES 12 SP4]
* [Latest version of 'ohpc-release' for SLE_12](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.x86_64.rpm) (md5sum=0a5954a9520e067aeb09e5377e9964a2)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-SLE_12-Warewulf-PBSPro-1.3.9-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.9.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.9-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.9/OpenHPC-1.3.9.SLE_12.x86_64.tar) mirror of zypper repository (md5sum=3a303ec35d6f77fe7463507892aa9095)

---

#### Questions, Comments, or Bug Reports?

Subscribe to the users email list at https://groups.io/g/openhpc-users or see
the http://openhpc.community page for more pointers.

#### Additional Software Requests?

Please see the component submission page at
https://github.com/openhpc/submissions for more information regarding new
software inclusion requests.

#### Register your system

If you are using elements of OpenHPC, please consider registering your
system(s) using the [System Registration
Form](https://drive.google.com/open?id=1KvFM5DONJigVhOlmDpafNTDDRNTYVdolaYYzfrHkOWI).


