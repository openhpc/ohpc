---

### OpenHPC: Community building blocks for HPC systems. (v1.1)

---

#### Introduction

This stack provides a variety of common, pre-built ingredients
required to deploy and manage an HPC Linux cluster including
provisioning tools, resource management, I/O clients, development
tools, and a variety of scientific libraries.

The compatible OS version for this release and the total number of
pre-packaged RPMs available are as follows:

        CentOS 7.2       - 270 RPMs (Base)
        SuSE SLES 12 SP1 - 267 RPMs (Base)

Note that a detailed list of all available components is available in
the "Package Manifest" appendix located in the companion install
guide documents. 

#### Getting started

OpenHPC provides pre-built binaries via repositories for use with standard
Linux package manager tools (e.g. ```yum``` or ```zypper```). Package
repositories are housed at https://build.openhpc.community. To get started, you
can enable an OpenHPC repository locally through installation of an
```ohpc-release``` RPM which includes gpg keys for package signing and defines
the URL locations for [base] and [update] package repositories. Copies of the
```ohpc-release``` package are provided below for convenience. A companion install
guide with example instructions for installation is also available below or in
in the downloads section of the latest
[release](https://github.com/openhpc/ohpc/releases/tag/v1.1.GA).

###### [CentOS 7.2]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.1.GA/ohpc-release-centos7.2-1.1-1.x86_64.rpm) (md5sum=f9349b2c2b117a4e3efdac8cd59cc327)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.1.GA/Install_guide-CentOS7.2-1.1.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.1/CentOS_7.2/iso/OpenHPC-1.1_CentOS_7.2.iso) mirror of yum repository (md5sum=d09fdd3d935d7f8d628cacbce3015c5c)
* Source file [ISO image](http://build.openhpc.community/OpenHPC:/1.1/CentOS_7.2/iso/OpenHPC-1.1_SOURCE_CentOS_7.2.iso) (md5sum=1c2c5286880da2d79f1c460fc50399d4)

###### [SLE 12 SP1]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.1.GA/ohpc-release-sles12sp1-1.1-1.x86_64.rpm) (md5sum=a99904b08c90548faaedf7201d60e101)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.1.GA/Install_guide-SLES12SP1-1.1.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.1/SLE_12_SP1/iso/OpenHPC-1.1_SLE_12_SP1.iso) mirror of zypper repository (md5sum=bac52eae6a9a02c85029903834eb5660)
* Source file [ISO image](http://build.openhpc.community/OpenHPC:/1.1/SLE_12_SP1/iso/OpenHPC-1.1_SOURCE_SLE_12_SP1.iso) (md5sum=8f3d92c5b7a8885d0ab8869d01c81539)

#### Questions, Comments, or Bug Reports?

Subscribe to the users email list at : https://groups.io/g/openhpc-users or see
the http://openhpc.community page for more pointers.

