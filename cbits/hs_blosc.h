#ifndef HS_BLOSC_H
#define HS_BLOSC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Initialize the Blosc library */
void hs_blosc_init(void);

/* Destroy the Blosc library */
void hs_blosc_destroy(void);

/* Free the last result */
void free_result(void);

#ifdef __cplusplus
}
#endif

#endif /* HS_BLOSC_H */ 