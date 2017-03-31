---

### OpenHPC: Community building blocks for HPC systems. (v1.3)

---

#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged RPMs available per architecture type are summarized as follows:

Base OS     | x86_64  | aarch64  | noarch
:---:       | :---:   | :---:    | :---:
CentOS 7.3  | 301     | 193      | 43
SLES 12 SP2 | 306     | 198      | 43

A detailed list of all available components is available in
the "Package Manifest" appendix located in each of the companion install
guide documents, and a list of updated packages can be found in the
[release notes](https://github.com/openhpc/ohpc/releases/tag/v1.3.GA). 

#### Getting started

OpenHPC provides pre-built binaries via repositories for use with standard
Linux package manager tools (e.g. ```yum``` or ```zypper```). Package
repositories are housed at https://build.openhpc.community. To get started, you
can enable an OpenHPC repository locally through installation of an
```ohpc-release``` RPM which includes gpg keys for package signing and defines
the URL locations for [base] and [update] package repositories. Copies of the
```ohpc-release``` package are provided below for convenience. Installtion guides 
taylored for each supported resource manager are also available below or in
the downloads section of the latest
[release](https://github.com/openhpc/ohpc/releases/tag/v1.3.GA).

---

#### Architecture = (x86_64)

###### [CentOS 7.3]
* [ohpc-release-1.3-1.el7.x86_64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.x86_64.rpm) (md5sum=d5139cf3aa83d095e6851628e8a684fa)
* [Install Guide (with PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-CentOS7-PBSPro-1.3-x86_64.pdf)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-CentOS7-SLURM-1.3-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3/OpenHPC-1.3.CentOS_7_x86_64.tar) mirror of yum repository (md5sum=ea0f3c47c9bac45c401dedee13bc2804)

###### [SLES 12 SP2]
* [ohpc-release-1.3-1.sle12.x86_64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.x86_64.rpm) (md5sum=0a5954a9520e067aeb09e5377e9964a2)
* [Install Guide (with PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-SLE_12-PBSPro-1.3-x86_64.pdf)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-SLE_12-SLURM-1.3-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3/OpenHPC-1.3.SLE_12_x86_64.tar) mirror of zypper repository (md5sum=e9fb908fcf51a3034721996c0c97501f)

---

#### Architecture = (aarch64)

Note that ARM-based builds in this release are being provided as a **Technology Preview**. See [here](https://github.com/openhpc/ohpc/wiki/ARM-Tech-Preview) for latest info.

###### [CentOS 7.3]
* [ohpc-release-1.3-1.el7.aarch64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.aarch64.rpm) (md5sum=16ad76e74b591a3b6dcc3cb8597d3f7d)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-CentOS7-SLURM-1.3-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3/OpenHPC-1.3.CentOS_7_aarch64.tar) mirror of yum repository (md5sum=087123b5624349b0b56873b956a0faa0)

###### [SLES 12 SP2]
* [ohpc-release-1.3-1.sle12.aarch64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.aarch64.rpm) (md5sum=706a42f7785952f8b543c501eeec05da)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/Install_guide-SLE_12-SLURM-1.3-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3/OpenHPC-1.3.SLE_12_aarch64.tar) mirror of zypper repository (md5sum=e11d4bb93c91eff173a673e672b42c46)

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


