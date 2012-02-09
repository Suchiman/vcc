//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#ifndef _VCC_H
#define _VCC_H

#ifndef VERIFY

#define _(...) /* VCC annotations hidden from compiler */

#else

#ifdef __cplusplus

#include <vccpp.h>

#else 

#include <vccp.h>

#endif

#endif

#endif // _VCC_H
