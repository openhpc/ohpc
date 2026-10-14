Prose before any block is never extracted, even with a fence:

```bash
echo "not extracted"
```

<!-- +section: chapters/example.md.j2 -->
<!-- +section: example/head.md.j2 -->
## A head node step

<!-- ohpc_begin -->
<!-- ohpc_comment Install the base packages (it's quoted) -->
```bash
dnf -y install ohpc-base
```

<!-- ohpc_if_set enable_feature -->
```bash
dnf -y install feature-ohpc
```
<!-- ohpc_else -->
<!-- ohpc_command echo "feature disabled" -->
<!-- ohpc_fi -->
<!-- ohpc_if [[ "${num_computes}" -gt 0 && -e "${inputFile}" ]] -->
```console
systemctl status ohpc.service
```
<!-- ohpc_fi -->
<!-- ohpc_command #<<< ohpc_proxy:head >>># -->
<!-- ohpc_end -->

Prose between blocks is skipped, including its fences:

```bash
echo "between blocks"
```

<!-- -section: example/head.md.j2 -->
<!-- +section: example/compute.md.j2 -->
<!-- ohpc_begin -->
```bash
wwctl image build rocky-10
```
<!-- ohpc_end -->
<!-- -section: example/compute.md.j2 -->
<!-- -section: chapters/example.md.j2 -->
