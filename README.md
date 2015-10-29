# [OpenHPC](https://openhpc.community) [![Build Status](http://obs.koomie.com:8080/badge/badge-1.0-CentOS_7.1.png)](https://obs.koomie.com/project/show/OpenHPC:1.0:Factory)

OpenHPC: Community building blocks for HPC systems.

## Introduction

This stack provides a variety of common, pre-built ingredients
required to deploy and manage an HPC Linux cluster including
provisioning tools, resource management, I/O clients, development
tools, and a plethora of scientific libraries.

The compatible OS versions for this release and the total number of
FSP-packaged RPMs for each variant is as follows:

	CentOS 7.1 - 166 RPMs

Note that a detailed list of all available components is available in
the "Package Manifest" appendix located in the companion Install
Guide document.

## Installation

```sh
wget -P /etc/yum.repos.d http://build.openhpc.community/OpenHPC:/1.0/CentOS_7.1:/OpenHPC:1.0.repo
yum -y install docs-ohpc
xpdf /opt/ohpc/pub/doc/Install_guide.pdf
```
## Questions, Comments, or Bug Reports?

Please direct to: karl.w.schulz@intel.com
