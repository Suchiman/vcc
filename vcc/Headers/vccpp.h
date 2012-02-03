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
    void BeginGhost();
    void EndGhost();
//    bool Implies;
}

#ifndef VERIFY

// hide annotations from C compiler

#define _(...)  /* nothing */

#else

#define REWRITE_GHOST

#endif 

#ifdef REWRITE_GHOST

#define vcc_ghost(X) VCC::BeginGhost(); X; VCC::EndGhost();

#else

#define vcc_ghost(...) /* nothing */

#endif
