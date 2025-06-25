# OpenHPC 3.x with Slurm running Rocky9 in a container

This is a simple single-user container environment for learning and testing
Slurm on OpenHPC 3.x with Rocky9.

The cluster contains a head node, login node, and 8 compute nodes as separate
containers with a shared docker network and shared docker storage.  The
containers are named `openhpc-head`, `openhpc-login` and `openhpc-node-[0-7]`
respectively and will overwrite/delete any local containers with those names.
Shared storage is in /project and /scratch in volumes
`openhpc-container-project` and `openhpc-container-scratch` respectively.  The
/home directory is not shared.

## Run the cluster

If you want to use podman or another container system with the same syntax as
Docker, set the `CONTAINER` environment variable as follows:

```bash
export CONTAINER=podman
```

Build and Run the cluster with the following:

```bash
./run.sh
```

This will create a Docker network and shared storage and start the cluster with
8 nodes and connect to the login node.  Exiting the shell will shutdown the
cluster cleanly.

To connect to the cluster as if you were using `ssh login` use the following in
a new terminal:

```bash
./ssh.sh
```

To login to the head node as root run the following in a separate terminal
(everything will be lost when exiting from `./run.sh`):

```bash
USER=root ./ssh.sh
```

When you are done run the following:

```bash
./delete.sh
```

This will remove the container cluster network, storage, and container images.
You may want to prune the container images as well.

## Examples

Copy examples to project folder

If you have rsync installed locally (look at ./rsync.sh for details), run in a
new local terminal:

```bash
./rsync.sh -av ./examples openhpc-login:/project/$USER/
```

If you don't have rsync

```bash
docker cp examples openhpc-login:/project/$USER/
docker exec -i openhpc-login chown -R $USER:$USER /project/$USER
```

And run the MPI example

```bash
cd /project/$USER/examples/mpi
bash run.sh
```

Check the results in the `slurm-*.out` file
