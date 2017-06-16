
---

### OpenHPC: Community building blocks for HPC systems. (v1.3.1)

---

[![Components](https://img.shields.io/badge/components%20available-67-green.svg) ](https://github.com/openhpc/ohpc/wiki/Component-List)
[![Additions](https://img.shields.io/badge/new%20additions-3-blue.svg) ](https://github.com/openhpc/ohpc/releases/tag/v1.3.1.GA)
[![Updates](https://img.shields.io/badge/updates-44%25-lightgrey.svg) ](https://github.com/openhpc/ohpc/releases/tag/v1.3.1.GA)


#### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, and a variety of
scientific libraries.

The compatible OS version(s) for this release and the total number of
pre-packaged binary RPMs available per architecture type are summarized as follows:

Base OS     | x86_64  | aarch64  | noarch
:---:       | :---:   | :---:    | :---:
CentOS 7.3  | 583     | 361      | 60
SLES 12 SP2 | 587     | 363      | 60

A detailed list of all available components is available in
the "Package Manifest" appendix located in each of the companion install
guide documents, and a list of updated packages can be found in the
[release notes](https://github.com/openhpc/ohpc/releases/tag/v1.3.1.GA). 

#### \*\* Important note for	those upgrading	from versions prior to 1.3

There are significant changes included in the `warewulf-httpd.conf file` that ships with the warewulf-provision-server-ohpc package. If upgrading from a previously installed version, the updated config file will be saved as `/etc/httpd/conf.d/warewulf-httpd.conf.rpmnew` locally. You will need to copy this new version to the production file and restart the web server to ensure correct provisioning behavior. As an example for CentOS:

```
[sms]# cp /etc/httpd/conf.d/warewulf-httpd.conf.rpmnew /etc/httpd/conf.d/warewulf-httpd.conf
[sms]# systemctl restart httpd
```

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
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-CentOS7-Warewulf-PBSPro-1.3.1-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.1-x86_64.pdf)
* [Install Guide (with xCAT & Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-CentOS7-xCAT-SLURM-1.3.1-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.1/OpenHPC-1.3.1.CentOS_7.x86_64.tar) mirror of yum repository (md5sum=e53ad41d09fed7b2ee763cfec6c2fcb1)

###### [SLES 12 SP2]
* [ohpc-release-1.3-1.sle12.x86_64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.x86_64.rpm) (md5sum=0a5954a9520e067aeb09e5377e9964a2)
* [Install Guide (with Warewulf + PBS Professional)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-SLE_12-Warewulf-PBSPro-1.3.1-x86_64.pdf)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.1-x86_64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.1/OpenHPC-1.3.1.SLE_12.x86_64.tar) mirror of zypper repository (md5sum=a7860f34247ebd3f926511621e8b863f)

---

#### Architecture = (aarch64)

Note that ARM-based builds in this release are being provided as a **Technology Preview**. See [here](https://github.com/openhpc/ohpc/wiki/ARM-Tech-Preview) for latest info.

###### [CentOS 7.3]
* [ohpc-release-1.3-1.el7.aarch64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.el7.aarch64.rpm) (md5sum=16ad76e74b591a3b6dcc3cb8597d3f7d)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-CentOS7-Warewulf-SLURM-1.3.1-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.1/OpenHPC-1.3.1.CentOS_7.aarch64.tar) mirror of yum repository (md5sum=0c9602f1ac898569b56a6cd4f6505881)

###### [SLES 12 SP2]
* [ohpc-release-1.3-1.sle12.aarch64.rpm](https://github.com/openhpc/ohpc/releases/download/v1.3.GA/ohpc-release-1.3-1.sle12.aarch64.rpm) (md5sum=706a42f7785952f8b543c501eeec05da)
* [Install Guide (with Warewulf + Slurm)](https://github.com/openhpc/ohpc/releases/download/v1.3.1.GA/Install_guide-SLE_12-Warewulf-SLURM-1.3.1-aarch64.pdf)
* [Tar Archive](http://build.openhpc.community/dist/1.3.1/OpenHPC-1.3.1.SLE_12.aarch64.tar) mirror of zypper repository (md5sum=5589854024ba16da4a989fed4c4ef1c5)

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


