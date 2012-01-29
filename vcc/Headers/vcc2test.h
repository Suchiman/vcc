//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#include "vcc.h"

#ifdef VERIFY2

void *malloc(unsigned __int64);
void free(void *p);

#else 

#define size_t _vcc_size_t
void *malloc(size_t);
void free(obj_t);

#endif

#define NULL ((void*)0)
