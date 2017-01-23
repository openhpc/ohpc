---

### OpenHPC: Community building blocks for HPC systems. (v1.2.1)

---

#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged RPMs available per architecture type are summarized as follows:

Base OS     | x86_64             | aarch64            | noarch
:---:       | :---:              | :---:              | :---:
CentOS 7.2  | 310 (+31 Updates) | 198 (+17 Updates) | 43
SLES 12 SP1 | 312 (+31 Updates) | 205 (+18 Updates) | 43

A detailed list of all available components is available in
the "Package Manifest" appendix located in each of the companion install
guide documents, and a list of updated packages can be found in the
[release notes](https://github.com/openhpc/ohpc/releases/tag/v1.2.1.GA). 

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
[release](https://github.com/openhpc/ohpc/releases/tag/v1.2.1.GA).

---

#### Architecture = (x86_64)

###### [CentOS 7.2]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.GA/ohpc-release-centos7.2-1.2-1.x86_64.rpm) (md5sum=2e33939e87e2fb5daf97d7875015a6ca)
* [Install Guide (with PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-CentOS7.2-PBSPro-1.2.1-x86_64.pdf)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-CentOS7.2-SLURM-1.2.1-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/OpenHPC-1.2.1_CentOS_7.2_x86_64.tar) mirror of yum repository (md5sum=2d1eb85aab6cd6cc2b127eafa7abcaaf)

###### [SLES 12 SP1]
* [ohpc-release.x86_64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/ohpc-release-sles12sp1-1.2-1.x86_64.rpm) (md5sum=58acd4320af0eceefcb8bcbc5f8a5a74)
* [Install Guide (with PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-SLE_12_SP1-PBSPro-1.2.1-x86_64.pdf)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-SLE_12_SP1-SLURM-1.2.1-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/OpenHPC-1.2_SLE_12_SP1_x86_64.tar) mirror of zypper repository (md5sum=93e1acdda8dc7bbff9596172fc37965f)

---

#### Architecture = (aarch64)

Note that ARM-based builds in this release are being provided as a **Technology Preview**. See [here](https://github.com/openhpc/ohpc/wiki/ARM-Tech-Preview) for latest info.

###### [CentOS 7.2]
* [ohpc-release.aarch64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/ohpc-release-centos7.2-1.2-1.aarch64.rpm) (md5sum=c29859ab4cdcf38021e20cde80fc0eb3)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-CentOS7.2-SLURM-1.2.1-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/OpenHPC-1.2_CentOS_7.2_aarch64.tar) mirror of yum repository (md5sum=c0d2cd41930cd3b5a4e6d0089e35a991)

###### [SLES 12 SP1]
* [ohpc-release.aarch64.rpm] (https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/ohpc-release-sles12sp1-aarch64-1.2-1.x86_64.rpm) (md5sum=fe25f8badffad78f805eb41cee8fff7c)
* [Install Guide (with Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.2.1.GA/Install_guide-SLE_12_SP1-SLURM-1.2.1-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/OpenHPC-1.2_SLE_12_SP1_aarch64.tar) mirror of zypper repository (md5sum=30998ec60d6807bd28ec6e4d970bf3a5)

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


