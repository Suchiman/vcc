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
    void Admissibility();
    void AtomicInline();
    void BeginUpdate();
    void Ensures(bool);
    void Invariant(bool);
    void Pure();
    void Requires(bool);
    template<class T> void Reads(T);
    template<class T> T Result();
    template<class T> void Returns(T);
    template<class T> void Writes(T);

    // data contracts
    void DynamicOwns();
    void VolatileOwns();
    
    // object state
    template<class T> bool Activeclaim(T);
    template<class T> void Atomic(T);
    template<class T> bool Claims(T, bool);
    template<class T> void HavocOthers(T);
    template<class T> bool Mine(T);    
    template<class T> bool Mutable(T);
    template<class T> T Old(T);
    template<class T> bool Threadlocal(T);
    template<class T> bool Wrapped(T);

   
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
    template<class T> bool ForAll(T);
    template<class T> bool Exists(T);
}
