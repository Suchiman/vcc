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
    // types
    typedef int Claim;
    typedef int Integer;
    typedef int Object;

    // assert/assume
    void Assert(bool);
    void Assume(bool);

    // method contracts
    void Admissibility();
    void AtomicInline();
    void BeginUpdate();
    void Ensures(bool);
    void FrameAxiom();
    void Invariant(bool);
    void Pure();
    void Requires(bool);
    template<class T> void Reads(T)     { };
    template<class T> T Result()        { T t; return t; };
    template<class T> void Returns(T)   { };
    template<class T> void Writes(T)    { };

    // data contracts
    void DynamicOwns();
    void VolatileOwns();
    
    // object state
    template<class T> bool Activeclaim(T)         { return true; };
    template<class T> void Always(T, bool)        { };
    template<class T> void Atomic(T)              { };
    template<class T> void BumpVolatileVersion(T) { };
    template<class T> bool Claims(T, bool)        { return true; };
    template<class T> int Claimcount(T)           { return 0; };
    template<class T> bool Closed(T)              { return true; };
    template<class T> void Destroyclaim(Claim, T) { };
    template<class T> T Extent(T t)               { return t; }
    template<class T> bool Fresh(T)               { return true; };
    template<class T> void HavocOthers(T)         { };
    template<class T> Claim Makeclaim(T, bool)    { };
    template<class T> bool Mallocroot(T)          { return true; }
    template<class T> bool Mine(T)                { return true; };
    template<class T> bool Mutable(T)             { return true; };
    template<class T> T Old(T)                    { T t; return t; };
    template<class T> bool Onunwrap(T, bool)      { return true; }
    template<class T> int Span(T)                 { return 0; };
    template<class T> bool Threadlocal(T)         { return true; };
    template<class T> void Unwrapping(T)          { };
    template<class T> bool Valid(T)               { return true; };
    template<class T> bool Wrapped(T)             { return true; };
    
    template<class T> void* Owns(T)               { return 0; };
    template<class T> void* Owner(T)              { return 0; };
   
    template <typename T> class Ghost
    {
    public:
      Ghost(T t);
      operator T();
    };
    
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
    template<class T> bool ForAll(T)  { return true; };
    template<class T> bool Exists(T)  { return true; };

    // helper function to ensure that we find certain functions in the AST
    void asthelper() 
    {
      VCC::Wrap(0);
      VCC::Unwrap(0);
    }
}
