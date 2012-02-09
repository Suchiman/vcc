//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

#include <vccpp-m.h>

#ifndef VERIFY

// hide annotations from C compiler

#define _(...) /* nothing */

#endif 

namespace VCC
{
    // function prototypes
    void Assert(bool);
    void Assume(bool);
    void Requires(bool);
    void Ensures(bool);

    template<class T> void Wrap(T);
    template<class T> void Unwrap(T);
    template<class T> void Writes(T);
    template<class T> bool Wrapped(T);
    template<class T> bool Mutable(T);
    template<class T> bool ThreadLocal(T);
    
    // special variables
    bool IMPLIES;
    bool RESULT;

    // templates for quantifiers
    template<class T> bool Forall(T);
    template<class T> bool Exists(T);
}
