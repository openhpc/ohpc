# OpenHPC container images

> (Everything defaults to `podman` as it provides the possibility to build
> containers without running as root. See 
> https://podman.io/blogs/2019/10/28/podman-with-nfs.html for details.)

The OpenHPC project currently provides the following container images:

 * ohpc-gnu8:1.3.9
 * ohpc-gnu8-mpich:1.3.9
 * ohpc-gnu8-mvapich2:1.3.9
 * ohpc-gnu8-openmpi3:1.3.9

All containers images are also available with the version specifier `latest`.

`ohpc-gnu8` is based on `centos7` and all other container images are based on
`ohpc-gnu8`.

Currently all container images are reachable at `quay.io/adrianreber/`:

`podman pull quay.io/adrianreber/ohpc-gnu8:latest`

## Rebuilding the OpenHPC container images

The OpenHPC container images can be locally rebuilt just by running `make`.
To override the version with which the container images are tagged use a
command like this: `VERSION=10.30 make`.

Using the command `make push` the container images will be built and pushed
to their default location. The container registry can be changed with the
following command: `DEST=container.registry/path make push`.

## Using the OpenHPC container images

One possible use case is to have quick access to the OpenHPC provided compiler
and MPI compiler:
```
$ cat test/hello.c 
#include <stdio.h>

int main()
{
	printf("Hello, world!\n");
}
$ podman run --rm -v `pwd`/test:/tmp:z ohpc-gnu8:1.3.9 gcc -o /tmp/hello /tmp/hello.c
$ podman run --rm -v `pwd`/test:/tmp:z ohpc-gnu8:1.3.9 /tmp/hello
Hello, world!
$ podman run --rm -v `pwd`/test:/tmp:z ohpc-gnu8-mpich:1.3.9 mpicc -o /tmp/mpi-hello /tmp/mpi-hello.c
$ podman run --rm -v `pwd`/test:/tmp:z ohpc-gnu8-mpich:1.3.9 /tmp/mpi-hello
 Hello, world (1 procs total)
    --> Process #   0 of   1 is alive. ->962f5dcd6374
```

It is also possible to build containers based on the OpenHPC containers:
```
$ cat Dockerfile
FROM quay.io/adrianreber/ohpc-gnu8-mpich:latest

COPY mpi-hello.c /home
RUN mpicc -o /home/mpi-hello /home/mpi-hello.c

WORKDIR /home
CMD mpirun -np 8 /home/mpi-hello

$ podman build . --tag=my-ohpc-container
$ podman run my-ohpc-container

 Hello, world (8 procs total)
    --> Process #   0 of   8 is alive. ->c7121a4fcc95
    --> Process #   1 of   8 is alive. ->c7121a4fcc95
    --> Process #   2 of   8 is alive. ->c7121a4fcc95
    --> Process #   3 of   8 is alive. ->c7121a4fcc95
    --> Process #   4 of   8 is alive. ->c7121a4fcc95
    --> Process #   5 of   8 is alive. ->c7121a4fcc95
    --> Process #   6 of   8 is alive. ->c7121a4fcc95
    --> Process #   7 of   8 is alive. ->c7121a4fcc95
```
