<!-- markdownlint-disable MD013 MD033 -->
# <img src="https://github.com/openhpc/ohpc/blob/master/docs/recipes/install/common/figures/ohpc_logo.png" width="170" valign="middle" hspace="5" alt="OpenHPC"/>
<!-- markdownlint-enable MD013 MD033 -->

## Community building blocks for HPC systems

### Introduction

This stack provides a variety of common, pre-built ingredients required to
deploy and manage an HPC Linux cluster including provisioning tools, resource
management, I/O clients, runtimes, development tools, containers, and a variety of
scientific libraries.

There are currently three release series: [1.3.x][13xbranch], [2.x][2xbranch] and
[3.x][3xbranch], which target different major Linux OS distributions:

- The 1.3.x series targets CentOS7 and SLES12.
- The 2.x series targets CentOS8 and Leap15.
- The 3.x series targets EL9, Leap 15 and openEuler 22.03.

### Getting started

OpenHPC provides pre-built binaries via repositories for use with standard
Linux package manager tools (e.g. ```yum``` or ```zypper```). To get started,
you can enable an OpenHPC repository locally through installation of an
```ohpc-release``` RPM which includes gpg keys for package signing and defines
the URL locations for [base] and [update] package repositories. Installation
guides tailored for each supported provisioning system and resource manager
with detailed example instructions for installing a cluster are also available.
Copies of the ```ohpc-release``` package and installation guides along with
more information is available on the relevant release series pages
([1.3.x][13xbranch], [2.x][2xbranch] or [3.x][3xbranch]).

---

### Questions, Comments, or Bug Reports?

Subscribe to the [users email list][userlist] or see the
<https://openhpc.community/> page for more pointers.

### Additional Software Requests?

Please see the component [submission page][submission] for more information
regarding new software inclusion requests.

### Contributing to OpenHPC

Please see the steps described in [CONTRIBUTING.md](CONTRIBUTING.md).

### Register your system

If you are using elements of OpenHPC, please consider registering your system(s)
using the [System Registration Form][register].

[13xbranch]: https://github.com/openhpc/ohpc/wiki/1.3.X
[2xbranch]: https://github.com/openhpc/ohpc/wiki/2.x
[3xbranch]: https://github.com/openhpc/ohpc/wiki/3.x
[register]: https://drive.google.com/open?id=1KvFM5DONJigVhOlmDpafNTDDRNTYVdolaYYzfrHkOWI
[submission]: https://github.com/openhpc/submission
[userlist]: https://groups.io/g/openhpc-users
