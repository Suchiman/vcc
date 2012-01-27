//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
#ifndef _VCC_H
#define _VCC_H

#include <crtdefs.h>

#ifndef VERIFY

// hide annotations from C compiler

#define _(...) /* nothing */

#define vcc_attr(k, v)
#define vcc_nospeccast(_TYPE_, _EXPR_) ((_TYPE_)_EXPR_)
#define vcc_generic_instance(_F_, ...) _F_ __VA_ARGS__

#else

#include <vccp.h>

#endif

#endif // _VCC_H
