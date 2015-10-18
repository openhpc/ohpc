#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "adios_types.h"
#include "core/adios_subvolume.h"

// Uncomment to print all data volumes (original, intermediate, and final)
//#define VERBOSE

#define SHIFT argv++; argc--;
#define SHIFT_N(n) argv += n; argc -= n;

static uint64_t prod(int ndims, uint64_t *dims) {
    uint64_t prod = 1;
    while (ndims) {
        prod *= *dims;
        ndims--;
        dims++;
    }
    return prod;
}

static void fill_random(int ndims, uint64_t *dims, int *volume) {
    int i;
    if (ndims == 1) {
        for (i = 0; i < *dims; i++)
            volume[i] = rand();
    } else {
        uint64_t stride = prod(ndims - 1, dims + 1);
        for (i = 0; i < *dims; i++) {
            fill_random(ndims - 1, dims + 1, volume + stride * i);
        }
    }
}

static void fill(int ndims, uint64_t *dims, int *volume, int fillv) {
    int i;
    if (ndims == 1) {
        for (i = 0; i < *dims; i++)
            volume[i] = fillv;
    } else {
        uint64_t stride = prod(ndims - 1, dims + 1);
        for (i = 0; i < *dims; i++) {
            fill(ndims - 1, dims + 1, volume + stride * i, fillv);
        }
    }
}

static void fill_subvolume(int ndims, uint64_t *dims, int *volume, uint64_t *offsets, uint64_t *filldims, int fillv) {
    int i;
    if (ndims == 1) {
        for (i = *offsets; i < *offsets + *filldims; i++)
            volume[i] = fillv;
    } else {
        uint64_t stride = prod(ndims - 1, dims + 1);
        for (i = *offsets; i < *offsets + *filldims; i++) {
            fill_subvolume(ndims - 1, dims + 1, volume + stride * i, offsets + 1, filldims + 1, fillv);
        }
    }
}

static void printvolume(int ndims, uint64_t *dims, int *volume, int indent) {
    int i, j;
    if (ndims == 1) {
        for (j = 0; j < indent; j++)
            printf(" ");
        printf("[");
        for (i = 0; i < *dims; i++)
            printf(" %x", volume[i]);
        printf(" ]\n");
    } else {
        uint64_t stride = prod(ndims - 1, dims + 1);
        for (j = 0; j < indent; j++)
            printf(" ");
        printf("[\n");
        for (i = 0; i < *dims; i++) {
            printvolume(ndims - 1, dims + 1, volume + stride * i, indent + 2);
        }
        for (j = 0; j < indent; j++)
            printf(" ");
        printf("]\n");
    }
}

char *cmd;
static void usage_and_exit() {
    fprintf(stderr, "Usage: %s <ndims> <num volumes> <subvolume dims ...> <volume 1 dims ...> ... <volume N dims ...>\n", cmd);
    exit(1);
}

int runtest(int argc, char **argv) {
    int ndims, nvolumes;
    uint64_t *subvolume_dims;
    uint64_t *volume_dims, *volume_offsets;
    int **volumes;
    int *final_volume;
    int i, j;

    cmd = *argv;
    SHIFT;
    if (argc < 2) usage_and_exit();
    ndims = atoi(argv[0]);
    nvolumes = atoi(argv[1]);
    SHIFT_N(2);

    if (argc < 2 * ndims * nvolumes + ndims) usage_and_exit();

#ifdef VERBOSE
    printf("Using %d-dimensional space with %d volumes\n", ndims, nvolumes);
#endif

    subvolume_dims = malloc(ndims * sizeof(uint64_t));
    volume_dims = malloc(nvolumes * ndims * sizeof(uint64_t));
    volume_offsets = malloc(nvolumes * ndims * sizeof(uint64_t));
    volumes = malloc(nvolumes * sizeof(int*));

#ifdef VERBOSE
    printf("Subvolume dimensions:");
#endif
    for (i = 0; i < ndims; i++) {
        subvolume_dims[i] = atoll(argv[i]);
#ifdef VERBOSE
        printf(" %llu", subvolume_dims[i]);
#endif
    }
#ifdef VERBOSE
    printf("\n");
#endif

    SHIFT_N(ndims);

    for (i = 0; i < nvolumes; i++) {
#ifdef VERBOSE
        printf("Volume %d:\n", i);
#endif

#ifdef VERBOSE
        printf("  Dimensions:");
#endif
        for (j = 0; j < ndims; j++) {
            volume_dims[i * ndims + j] = atoll(argv[j]);
#ifdef VERBOSE
            printf(" %llu", volume_dims[i * ndims + j]);
#endif
        }
#ifdef VERBOSE
        printf("\n");
#endif
        SHIFT_N(ndims);

#ifdef VERBOSE
        printf("  Offsets:");
#endif
        for (j = 0; j < ndims; j++) {
            volume_offsets[i * ndims + j] = atoll(argv[j]);
#ifdef VERBOSE
            printf(" %llu", volume_offsets[i * ndims + j]);
#endif
            assert(volume_offsets[i * ndims + j] + subvolume_dims[j] <= volume_dims[i * ndims + j]);
        }
#ifdef VERBOSE
        printf("\n");
#endif
        SHIFT_N(ndims);

        volumes[i] = malloc(prod(ndims, volume_dims + i*ndims) * sizeof(int));
        fill(ndims, volume_dims + i*ndims, volumes[i], i);
        //printvolume(ndims, volume_dims + i*ndims, volumes[i], 0);
    }

    // final_volume = first volume, but with garbage filled into the subvolume to copy
    final_volume = malloc(prod(ndims, volume_dims + 0*ndims) * sizeof(int));
    memcpy(final_volume, volumes[0], prod(ndims, volume_dims + 0*ndims));
    fill_subvolume(ndims, volume_dims + 0*ndims, final_volume, volume_offsets + 0*ndims, subvolume_dims, -1);

#ifdef VERBOSE
    printf("--- Final volume before copy:\n");
    printvolume(ndims, volume_dims + 0*ndims, final_volume, 0);
    printf("----------\n");
#endif

#ifdef VERBOSE
    printf("--- Volume 0:\n");
    printvolume(ndims, volume_dims + 0*ndims, volumes[0], 0);
    printf("----------\n");
#endif

    for (i = 1; i < nvolumes; i++) {
#ifdef VERBOSE
        printf("Copying subvolume from volume %d to %d\n", i-1 + 1, i + 1);
#endif
        copy_subvolume(volumes[i], volumes[i-1], ndims, subvolume_dims,
                       volume_dims + i*ndims, volume_offsets + i*ndims,
                       volume_dims + (i-1)*ndims, volume_offsets + (i-1)*ndims,
                       adios_integer, adios_flag_no);

#ifdef VERBOSE
        printf("--- Volume %d:\n", i);
        printvolume(ndims, volume_dims + i*ndims, volumes[i], 0);
        printf("----------\n");
#endif
    }

#ifdef VERBOSE
    printf("Copying subvolume from volume %d to replica of volume 1\n", nvolumes);
#endif
    copy_subvolume(final_volume, volumes[nvolumes-1], ndims, subvolume_dims,
                   volume_dims + 0*ndims, volume_offsets + 0*ndims,
                   volume_dims + (nvolumes-1)*ndims, volume_offsets + (nvolumes-1)*ndims,
                   adios_integer, adios_flag_no);

#ifdef VERBOSE
    printf("--- Final volume:\n", i);
    printvolume(ndims, volume_dims + 0*ndims, final_volume, 0);
    printf("----------\n");
#endif

#ifdef VERBOSE
    printf("Comparing volume 0 and its replica after copy operations...\n");
#endif
    int success = (memcmp(volumes[0], final_volume, prod(ndims, volume_dims + 0*ndims)) == 0);

    if (success) {
        printf("Success!\n");
        return 0;
    } else {
        printf("Failed!\n");
        return 1;
    }
}

#include <string.h>
int runtest_helper(const char *cmdline) {
    int argc = 0;
    char *argv[256];
    char *cmdline_tmp = strdup(cmdline);

    do {
        argv[argc] = strtok(argc == 0 ? cmdline_tmp : NULL, " \t");
    } while (argv[argc++]);

    argc--;
#ifdef VERBOSE
    printf("argc = %d\n", argc);
#endif

    int ret = runtest(argc, argv);

    free(cmdline_tmp);
    return ret;
}

int main(int argc, char **argv) {
    if (argc == 1) {
        printf("Running copy_volume tests...\n");
        printf("Test 1...\n");
        int ret = runtest_helper(
                "dummycmd 2 4 "	\
                "10 10 "		\
                "10 10 " "0 0 "	\
                "20 20 " "5 5 "	\
                "10 20 " "0 0 "	\
                "20 10 " "0 0 ");

        if (ret != 0)
            return ret;

        return 0;
    } else {
        return runtest(argc, argv);
    }
}
