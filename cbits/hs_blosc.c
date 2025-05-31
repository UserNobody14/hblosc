#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <blosc.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Initialize the Blosc library */
void hs_blosc_init(void) {
    blosc_init();
}

/* Destroy the Blosc library */
void hs_blosc_destroy(void) {
    blosc_destroy();
}

/* Free the last result */
void free_result(void) {
    /* This is a placeholder function to match the one from blosc.cpp
       In a real implementation, you would need to track and free any allocated memory */
}

#ifdef __cplusplus
}
#endif 