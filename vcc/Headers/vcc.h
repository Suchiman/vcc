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

#ifndef __cplusplus

#include <vccp.h>

#endif

#endif

#ifdef __cplusplus

#include <vccpp.h>

#endif

#endif // _VCC_H
