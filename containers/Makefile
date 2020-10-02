COMPILER := gnu9
MPIS := openmpi4 mpich mvapich2
CONTAINERS := $(COMPILER) $(addprefix $(COMPILER)-,$(MPIS))
CONTAINER_RUNTIME := podman
CONTAINERS_PUSH := $(addsuffix -push,$(CONTAINERS))
COMPILER_CHECK := $(addsuffix -check,$(COMPILER))
MPIS_CHECK := $(addsuffix -check,$(addprefix $(COMPILER)-,$(MPIS)))

DEST ?= quay.io/ohpc/
VERSION ?= 2.0.0

.PHONY: all $(CONTAINERS) push $(CONTAINERS_PUSH) $(COMPILER_CHECK) $(MPIS_CHECK) clean

all: $(CONTAINERS)

$(CONTAINERS):
	$(CONTAINER_RUNTIME) build . -f Dockerfile.ohpc-$@ --tag=ohpc-$@:$(VERSION)

define PUSH
$(1)-push: $(1)
	$(CONTAINER_RUNTIME) push ohpc-$(1):$(VERSION) $(DEST)ohpc-$(1):$(VERSION)
	$(CONTAINER_RUNTIME) push ohpc-$(1):$(VERSION) $(DEST)ohpc-$(1):latest
endef
$(foreach c,$(CONTAINERS),$(eval $(call PUSH,$(c))))

push: $(CONTAINERS_PUSH)

$(COMPILER_CHECK): $(COMPILER)
	rm -f test/hello
	$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$<:$(VERSION) gcc -o /tmp/hello /tmp/hello.c
	$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$<:$(VERSION) /tmp/hello
	rm -f test/hello

define MPI_CHECK
$(1)-check: $(1)
	rm -f test/hello test/mpi-hello
	$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$(1):$(VERSION) mpicc -o /tmp/hello /tmp/hello.c
	$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$(1):$(VERSION) /tmp/hello
	$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$(1):$(VERSION) mpicc -o /tmp/mpi-hello /tmp/mpi-hello.c
	if [[ "$(1)" == *"openmpi"* ]]; then \
		mkdir /tmp/podman-mpirun ;\
		. $(MODULESHOME)/init/bash && module purge && \
		. $(MODULESHOME)/init/bash && module load `echo $(1) | sed -e "s,-, ,g"` ;\
		OMPI_MCA_orte_tmpdir_base=/tmp/podman-mpirun mpirun $(CONTAINER_RUNTIME) run -v /tmp/podman-mpirun:/tmp/podman-mpirun --env-host --rm -v `pwd`/test:/tmp:z --userns=keep-id  --net=host --pid=host --ipc=host ohpc-$(1):$(VERSION) /tmp/mpi-hello ;\
		rm -rf /tmp/podman-mpirun test/podman-mpirun ;\
		. $(MODULESHOME)/init/bash && module purge ;\
	elif [[ "$(1)" == *"mpich"* ]]; then \
		$(CONTAINER_RUNTIME) run --rm -v `pwd`/test:/tmp:z ohpc-$(1):$(VERSION) /tmp/mpi-hello ;\
	fi
	rm -f test/hello test/mpi-hello
endef
$(foreach c,$(addprefix $(COMPILER)-,$(MPIS)),$(eval $(call MPI_CHECK,$(c))))

check: $(COMPILER_CHECK) $(MPIS_CHECK)

clean:
	rm -f test/hello test/mpi-hello
