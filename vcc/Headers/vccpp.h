//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

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

#ifndef VERIFY

// hide annotations from C compiler

#define _(...) /* nothing */

#else

#define REWRITE_GHOST

#endif 

#ifdef REWRITE_GHOST

#define vcc_ghost(X) VCC::BeginGhost(); X; VCC::EndGhost();

#else

#define vcc_ghost(...) /* nothing */

#endif