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
    // assert/assume
    void Assert(bool);
    void Assume(bool);

    // method contracts
    void Requires(bool);
    void Ensures(bool);
    void Pure();
    template<class T> void Writes(T);
    template<class T> bool Maintains(T);
    template<class T> T Result();

    // object state
    template<class T> bool Wrapped(T);
    template<class T> bool Mutable(T);
    template<class T> bool Threadlocal(T);

    // statements
    template<class T> void Wrap(T o)
    {
      VCC::Writes(o); // TODO: also writes o->\owns
    }

    template<class T> void Unwrap(T o)
    {
      VCC::Writes(o);
    }

    // special variables
    bool Implies;

    // templates for quantifiers
    template<class T> bool Forall(T);
    template<class T> bool Exists(T);
}
