//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#include "vcc.h"

#define size_t _vcc_size_t
void *malloc(size_t);
void free(obj_t);
#define NULL ((void*)0)
