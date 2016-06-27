---

### OpenHPC: Community building blocks for HPC systems.

---

#### Introduction

This stack provides a variety of common, pre-built ingredients
required to deploy and manage an HPC Linux cluster including
provisioning tools, resource management, I/O clients, development
tools, and a variety of scientific libraries.

The compatible OS version for this release and the total number of
pre-packaged RPMs available are as follows:

        CentOS 7.1 - 255 RPMs (Base) / 17 RPMs (Updates)

Note that a detailed list of all available components is available in
the "Package Manifest" appendix located in the companion install
guide document. 

#### Getting started

OpenHPC provides pre-built binaries via repositories for use with standard
Linux package manager tools (e.g. ```yum```). Package repositories are housed
at https://build.openhpc.community. To get started, you can enable an OpenHPC
repository locally through installation of an ```ohpc-release``` RPM which
includes gpg keys for package signing and defines the URL locations for [base]
and [update] package repositories. A copy of the ```ohpc-release``` file is
provided below for convenience. A companion install guide with example
instructions for installation is also available below or in in the downloads
section of the latest
[release](https://github.com/openhpc/ohpc/releases/tag/v1.0.1.GA).

###### [CentOS 7.1]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.0.1.GA/ohpc-release-1.0-1.x86_64.rpm) (md5sum=8dbaed1bea6245c04cf201131df74bc3)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.0.1.GA/Install_guide-CentOS7.1-1.0.1.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.0/CentOS_7.1/iso/OpenHPC-1.0-CentOS_7.1.iso) mirror of Yum repository (md5sum=e5672b5272d9c93ca323304331db1353)

#### Questions, Comments, or Bug Reports?

Subscribe to the users email list at : https://groups.io/g/openhpc-users or see
the http://openhpc.community page for more pointers.

