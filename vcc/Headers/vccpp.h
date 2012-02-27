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
    class Claim {
    public:
      Claim();
      Claim(const volatile Claim&);
      Claim(const Claim&);
      bool operator==(Claim);
      bool operator!=(Claim);
    };

    class Integer {
    public:
      bool operator==(Integer);
      bool operator!=(Integer);
      bool operator<(Integer);
      bool operator>(Integer);
      bool operator<=(Integer);
      bool operator>=(Integer);
    };

    class Natural {
    public:
      bool operator==(Natural);
      bool operator!=(Natural);
      bool operator<(Natural);
      bool operator>(Natural);
      bool operator<=(Natural);
      bool operator>=(Natural);
    };

    class Object {
    public:
      bool operator==(void*);
      bool operator!=(void*);
    };
    
    class Set {
    public:
      Set(...);
      bool operator==(Set);
      bool operator!=(Set);
      bool operator-=(Set);
      bool operator+=(Set);
    };

    class State {      
    public:
      State();
      State(const volatile State&);
      State(const State&);
      bool operator==(State);
      bool operator!=(State);
    };

    template <class From, class To> class Map {
    public:
      To& operator[](From);
      To& operator[](From) volatile;
    };

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
    template<class T> void Reads(T, ...)  { };
    template<class T> T Result()          { T t; return t; };
    template<class T> void Returns(T)     { };
    template<class T> void Writes(T, ...) { };

    // data contracts
    void DynamicOwns();
    void VolatileOwns();
    
    // object state
    template<class T> bool Activeclaim(T)               { return true; };
    template<class T> void Always(T, bool)              { };
    template<class T> T* Alloc()                        { return 0; };
    template<class T1, class T2> bool Approves(T1, T2)  { return true; };
    template<class T> T Array(T t, int)                 { return t; };
    template<class T> T Arrayrange(T t, int)            { return t; };
    template<class T> T At(State, T t)                  { return t; };
    template<class T> void Atomic(T, ...)               { };
    template<class T> void BumpVolatileVersion(T)       { };
    bool Claims(Claim, bool)                            { return true; };
    template<class T> int Claimcount(T)                 { return 0; };
    template<class T> bool Claimsobject(Claim, T)       { return true; };
    template<class T> bool Closed(T)                    { return true; };
    template<class T1, class T2> bool Depends(T1, T2)   { return true; }
    template<class T> void Destroyclaim(Claim, T)       { };
    Set Diff(Set s, Set)                                { return s; };
    template<class T> Set Domain(T)                     { return 0; };
    template<class T> T Extent(T t)                     { return t; }
    template<class T> bool Fresh(T)                     { return true; };
    template<class T> void HavocOthers(T)               { };
    template<class T> bool In(T, Set)                   { return true; }; 
    template<class T> bool Inv(T)                       { return true; };
    template<class T> T Labeled(const char*, T t)       { return t; };
    template<class T> Claim Makeclaim(T, bool)          { };
    template<class T> bool Mallocroot(T)                { return true; }
    template<class T> bool Mine(T, ...)                 { return true; };
    template<class T> bool Mutable(T)                   { return true; };
    template<class T> bool Notshared(T)                 { return true; };
    State Now()                                         { State s; return s; };
    template<class T> T Old(T)                          { T t; return t; };
    template<class T> bool Onunwrap(T, bool)            { return true; };
    bool Programentrypoint()                            { return true; };
    template<class T> int Span(T)                       { return 0; };
    template<class T> bool Threadlocal(T)               { return true; };
    template<class T> bool Unchanged(T)                 { return true; };
    Set Union(Set s, Set)                               { return s; };
    void* Universe()                                    { return 0; };
    template<class T> void Unwrapping(T, ...)           { };
    template<class T> bool Valid(T)                     { return true; };
    template<class T> T Whenclaimed(T t)                { return t; };
    template<class T> bool Wrapped(T)                   { return true; };
    
    template<class T> Set Owns(T)                       { Set s; return s; };
    template<class T> Object Owner(T)                   { Object o; return o; };
   
    template <typename T> class Ghost
    {
    private:
      T _t_member_;

    public:
      Ghost(T t);
      Ghost(const Ghost<T> &g);
      operator T() const;
    };
    
    template <typename T> class GhostOut
    {
    private:
      T _t_member_;

    public:
      GhostOut(T t);
      operator T() const;
    };
    
    template <typename T> class Unchecked 
    {
    private:
      T _t_member_;

    public:
      Unchecked(const T&);
      operator T() const;
    };
    
    template <typename T> Ghost<T> CreateGhost(T);
    template <typename T> GhostOut<T> CreateGhostOut(T);

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
    void Trigger(...);

    // helper function to ensure that we find certain functions in the AST
    void asthelper() 
    {
      VCC::Wrap(0);
      VCC::Unwrap(0);
    }
}
