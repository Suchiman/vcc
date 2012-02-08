//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

#ifndef VERIFY

// hide annotations from C compiler

#define _(...) /* nothing */

#else

namespace VCC
{
    void Assert(bool);
    void Assume(bool);
    bool Implies;
    void BeginGhost();
    void EndGhost();
    template<class T> bool Forall(T);
    template<class T> bool Exists(T);
}

#endif 
