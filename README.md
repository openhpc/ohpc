
---

### OpenHPC: Community building blocks for HPC systems. (v1.3.3)

---

[![Components](https://img.shields.io/badge/components%20available-77-green.svg) ](https://github.com/openhpc/ohpc/wiki/Component-List-v1.3.3)
[![Additions](https://img.shields.io/badge/new%20additions-2-blue.svg) ](https://github.com/openhpc/ohpc/releases/tag/v1.3.3.GA)
[![Updates](https://img.shields.io/badge/updates-27%25-lightgrey.svg) ](https://github.com/openhpc/ohpc/releases/tag/v1.3.3.GA)
[![Tests](https://img.shields.io/badge/tests%20passing-100%25-brightgreen.svg) ](http://test.openhpc.community:8080/job/1.3.x/view/1.3.3/)


#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged binary RPMs available per architecture type are summarized as follows:

Base OS     | x86_64  | aarch64  | noarch
:---:       | :---:   | :---:    | :---:
CentOS 7.4  | 517     | 309      | 34
SLES 12 SP3 | 521     | 317      | 34

A list of all available components is available on the OpenHPC
[wiki](https://github.com/openhpc/ohpc/wiki/Component-List-v1.3.3), and
specific versioning details can be found in the "Package Manifest" appendix
located in each of the companion install guide documents. A list of updated
packages can be found in the [release
notes](https://github.com/openhpc/ohpc/releases/tag/v1.3.3.GA). The release
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
[release](https://github.com/openhpc/ohpc/releases/tag/v1.3.3.GA).

---

#### Architecture = (x86_64)

###### [CentOS 7.4]
* [Latest OpenHPC Release RPM](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.x86_64.rpm) (md5sum=d5139cf3aa83d095e6851628e8a684fa)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-CentOS7-Warewulf-PBSPro-1.3.3-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.3-x86_64.pdf)
* [Install Guide (with xCAT + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-CentOS7-xCAT-SLURM-1.3.3-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.3/OpenHPC-1.3.3.CentOS_7.x86_64.tar) mirror of yum repository (md5sum=ff5b805f33a9b92cc09a0849fa463ce5)

###### [SLES 12 SP3]
* [Latest OpenHPC Release RPM](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.x86_64.rpm) (md5sum=0a5954a9520e067aeb09e5377e9964a2)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-SLE_12-Warewulf-PBSPro-1.3.3-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.3-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.3/OpenHPC-1.3.3.SLE_12.x86_64.tar) mirror of zypper repository (md5sum=b14fa31dc8844dd3895474a9a1b787b8)

---

#### Architecture = (aarch64)

Note that ARM-based builds in this release are being provided as a **Technology Preview**. See [here](https://github.com/openhpc/ohpc/wiki/ARM-Tech-Preview) for latest info.

###### [CentOS 7.4]
* [Latest OpenHPC Release RPM](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.aarch64.rpm) (md5sum=16ad76e74b591a3b6dcc3cb8597d3f7d)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.3-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.3/OpenHPC-1.3.3.CentOS_7.aarch64.tar) mirror of yum repository (md5sum=6799236eec263892a8a432c2ae377618)

###### [SLES 12 SP3]
* [Latest OpenHPC Release RPM](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.aarch64.rpm) (md5sum=706a42f7785952f8b543c501eeec05da)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.3.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.3-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.3/OpenHPC-1.3.3.SLE_12.aarch64.tar) mirror of zypper repository (md5sum=19952d4a0884b2215b22db70d54dac31)

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


