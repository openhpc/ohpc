/*
 * transforms_specparse.c
 *
 * Tests the "specparse" functionality, which parses the string passed as transform="..." into
 * the transform ID and a list of key-value pairs.
 *
 *  Created on: Jul 25, 2013
 *      Author: David A. Boyuka II
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "core/transforms/adios_transforms_specparse.h"

#define DISABLE_SPECPARSE_TESTS

#ifdef DISABLE_SPECPARSE_TESTS

int main(int argc, char **argv) { return 0; }

#else

struct specparse_test {
    const char *specstr;
    struct adios_transform_spec expected;
} TESTS[] = {
    {   .specstr = "identity:a=123,b,c=321,,,f=12321",
        .expected = {
            .transform_type     = adios_transform_identity,
            .transform_type_str = "identity",
            .param_count        = 6,
            .params             = (struct adios_transform_spec_kv_pair *) {
                (struct adios_transform_spec_kv_pair){ .key = "a", .value = "123"  },
                (struct adios_transform_spec_kv_pair){ .key = "b", .value = NULL   },
                (struct adios_transform_spec_kv_pair){ .key = "c", .value = "321"  },
                (struct adios_transform_spec_kv_pair){ .key = "",  .value = NULL   },
                (struct adios_transform_spec_kv_pair){ .key = "",  .value = NULL   },
                (struct adios_transform_spec_kv_pair){ .key = "f", .value = "12321"},
            }
        }
    },
    {   .specstr = "identity",
        .expected = {
            .transform_type     = adios_transform_identity,
            .transform_type_str = "identity",
            .param_count        = 0,
            .params             = NULL
        }
    },
    {   .specstr = "none:a=123,b,c=321,,,f=12321",
        .expected = {
            .transform_type     = adios_transform_none,
            .transform_type_str = "none",
            .param_count        = 0,
            .params             = NULL
        }
    },
    {   .specstr = "***impossible-transform-name***:a=123,b,c=321,,,f=12321",
        .expected = {
            .transform_type     = adios_transform_unknown,
            .transform_type_str = "***impossible-transform-name***",
            .param_count        = 0,
            .params             = NULL
        }
    },
};


const int NUM_TESTS = sizeof(TESTS)/sizeof(TESTS[0]);

void run_test(struct specparse_test *test) {
    const struct adios_transform_spec *actual = adios_transform_parse_spec(test->specstr, NULL);
    const struct adios_transform_spec *expected = &test->expected;

    // Check transform type ID
    assert(actual->transform_type == expected->transform_type);

    // Check transform type string
    assert(actual->transform_type_str && expected->transform_type_str);
    assert(strcmp(actual->transform_type_str, expected->transform_type_str) == 0);

    // Check parameter count, and ensure parameter list exists for both or neither
    assert(actual->param_count == expected->param_count);
    assert((actual->params != NULL) == (expected->params != NULL));

    // If there is a parameter list, check it
    if (expected->params) {
        int i;
        for (i = 0; i < expected->param_count; i++) {
            const struct adios_transform_spec_kv_pair *actual_p = &actual->params[i];
            const struct adios_transform_spec_kv_pair *expected_p = &expected->params[i];

            // Check that the keys are non-NULL and match
            assert(actual_p->key && expected_p->key);
            assert(strcmp(actual_p->key, expected_p->key) == 0);

            // Check that values are either both or neither present, and if the former, that they match
            assert((actual_p->value != NULL) == (expected_p->value != NULL));
            if (expected_p->value != NULL)
                assert(strcmp(actual_p->value, expected_p->value) == 0);
        }
    }

    adios_transform_free_spec(&actual);
}

void init_tests() {
    TESTS[0].specstr = strdup ("identity:a=123,b,c=321,,,f=12321");
    TESTS[0].expected.transform_type = adios_transform_identity;
    TESTS[0].expected.transform_type_str = "identity";
    TESTS[0].expected.param_count = 6;
    TESTS[0].expected.params = (struct adios_transform_spec_kv_pair *)
                  malloc (sizeof (struct adios_transform_spec_kv_pair) * 6);
    TESTS[0].expected.params[0].key = strdup ("a");
    TESTS[0].expected.params[0].value = strdup ("123");
    TESTS[0].expected.params[1].key = strdup ("b");
    TESTS[0].expected.params[1].value = NULL;
    TESTS[0].expected.params[2].key = strdup ("c");
    TESTS[0].expected.params[2].value = strdup ("321");
    TESTS[0].expected.params[3].key = strdup ("");
    TESTS[0].expected.params[3].value = NULL;
    TESTS[0].expected.params[4].key = strdup ("");
    TESTS[0].expected.params[4].value = NULL;
    TESTS[0].expected.params[5].key = strdup ("f");
    TESTS[0].expected.params[5].value = strdup ("12321");

    TESTS[1].specstr = strdup ("identity");
    TESTS[1].expected.transform_type = adios_transform_identity;
    TESTS[1].expected.transform_type_str = "identity";
    TESTS[1].expected.param_count = 0;
    TESTS[1].expected.params = NULL;

    TESTS[2].specstr = strdup ("none:a=123,b,c=321,,,f=12321");
    TESTS[2].expected.transform_type = adios_transform_none;
    TESTS[2].expected.transform_type_str = "none";
    TESTS[2].expected.param_count = 0;
    TESTS[2].expected.params = NULL;

    TESTS[3].specstr = strdup ("***impossible-transform-name***:a=123,b,c=321,,,f=12321");
    TESTS[3].expected.transform_type = adios_transform_unknown;
    TESTS[3].expected.transform_type_str = "***impossible-transform-name***";
    TESTS[3].expected.param_count = 0;
    TESTS[3].expected.params = NULL;
}

void run_tests() {
    int i;
    for (i = 0; i < NUM_TESTS; i++) {
        run_test(&TESTS[i]);
    }
}

int main(int argc, char **argv) {

    init_tests();
    run_tests();

    return 0;
}

#endif /* else of DISABLE_SPECPARSE_TESTS */
