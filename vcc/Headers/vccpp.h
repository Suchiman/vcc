//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

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
    void BeginGhost();
    void EndGhost();

    template<class T> void Wrap(T);
    template<class T> void Unwrap(T);
    
    // special variables
    bool IMPLIES;
    bool TRUE;
    bool RESULT;

    // templates for quantifiers
    template<class T> bool Forall(T);
    template<class T> bool Exists(T);
}
