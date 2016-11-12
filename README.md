---

### OpenHPC: Community building blocks for HPC systems. (v1.2)

---

#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged RPMs available per architecture type are summarized as follows:

Base OS     | x86_64 | aarch64 | noarch
:---:       | :---:  | :---:   | :---:
CentOS 7.2  | 310    | 198     | 43
SLES 12 SP1 | 312    | 205     | 43

Note that a detailed list of all available components is available in
the "Package Manifest" appendix located in each of the companion install
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
the downloads section of the latest
[release](https://github.com/openhpc/ohpc/releases/tag/v1.2.GA).

---

#### (x86_64) Architecture 
###### [CentOS 7.2]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.GA/ohpc-release-centos7.2-1.2-1.x86_64.rpm) (md5sum=2e33939e87e2fb5daf97d7875015a6ca)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.2.GA/Install_guide-CentOS7.2-1.2.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.2/CentOS_7.2/iso/OpenHPC-1.2_CentOS_7.2.iso) mirror of yum repository (md5sum=)

###### [SLES 12 SP1]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.GA/ohpc-release-sles12sp1-1.2-1.x86_64.rpm) (md5sum=58acd4320af0eceefcb8bcbc5f8a5a74)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.2.GA/Install_guide-SLES12SP1-1.2.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.2/SLE_12_SP1/iso/OpenHPC-1.2_SLE_12_SP1.iso) mirror of zypper repository (md5sum=)

---

#### (aarch64) Architecture

Note that ARM-based builds in this release are being provided as a [Technology Preview](https://github.com/openhpc/ohpc/wiki/ARM-Tech-Preview).

###### [CentOS 7.2]
* [ohpc-release.aarch64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.GA/ohpc-release-centos7.2-1.2-1.aarch64.rpm) (md5sum=c29859ab4cdcf38021e20cde80fc0eb3)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.2.GA/Install_guide-CentOS7.2-aarch64-1.2.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.2/CentOS_7.2/iso/OpenHPC-1.2_CentOS_7.2-aarch64.iso) mirror of yum repository (md5sum=)

###### [SLES 12 SP1]
* [ohpc-release.aarch64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.GA/ohpc-release-sles12sp1-aarch64-1.2-1.x86_64.rpm) (md5sum=fe25f8badffad78f805eb41cee8fff7c)
* [Install Guide](https://github.com/openhpc/ohpc/releases/download/v1.2.GA/Install_guide-SLES12SP1-aarch64-1.2.pdf)
* [ISO image](http://build.openhpc.community/OpenHPC:/1.2/SLE_12_SP1/iso/OpenHPC-1.2_SLE_12_SP1-aarch64.iso) mirror of zypper repository (md5sum=)


#### Questions, Comments, or Bug Reports?

Subscribe to the users email list at https://groups.io/g/openhpc-users or see
the http://openhpc.community page for more pointers.

#### Additional Software Requests?

Please see the companion submission page at
https://github.com/openhpc/submissions for more information regarding new
software component requests.
