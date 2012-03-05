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

#else

void free(void*, size_t);

#endif 

namespace VCC
{
    // types
    template<class T> class Ghost;
    
    class Claim {
    public:
      Claim();
      Claim(const Claim&);
      Claim(const volatile Claim&);
      Claim operator=(const Ghost<Claim>&);
      Claim operator=(const volatile Claim&);
      bool operator==(Claim);
      bool operator==(Ghost<Claim>);
      bool operator!=(Claim);
      bool operator!=(Ghost<Claim>);
    };

    class Integer {
    public:
      bool operator==(Integer);
      bool operator==(Ghost<Integer>);
      bool operator!=(Integer);
      bool operator!=(Ghost<Integer>);
      bool operator<(Integer);
      bool operator<(Ghost<Integer>);
      bool operator>(Integer);
      bool operator>(Ghost<Integer>);
      bool operator<=(Integer);
      bool operator<=(Ghost<Integer>);
      bool operator>=(Integer);
      bool operator>=(Ghost<Integer>);
    };

    class Natural {
    public:
      bool operator==(Natural);
      bool operator==(Ghost<Natural>);
      bool operator!=(Natural);
      bool operator!=(Ghost<Natural>);
      bool operator<(Natural);
      bool operator<(Ghost<Natural>);
      bool operator>(Natural);
      bool operator>(Ghost<Natural>);
      bool operator<=(Natural);
      bool operator<=(Ghost<Natural>);
      bool operator>=(Natural);
      bool operator>=(Ghost<Natural>);
    };

    class Object {
    public:
      Object(void*);
      bool operator==(void*);
      bool operator==(Ghost<Object>);
      bool operator==(Ghost<Claim>);
      bool operator!=(void*);
      bool operator!=(Ghost<Object>);
      bool operator!=(Ghost<Claim>);
      Object operator=(void*);      
    };
    
    class Set {
    public:
      Set(...);
      bool operator==(Set);
      bool operator==(Ghost<Set>);
      bool operator!=(Set);
      bool operator!=(Ghost<Set>);
      bool operator-=(Set);
      bool operator-=(Ghost<Set>);
      bool operator+=(Set);
      bool operator+=(Ghost<Set>);
    };

    class Objset: public Set
    {
    };

    class State {      
    public:
      State();
      State(const volatile State&);
      State(const State&);
      bool operator==(State);
      bool operator==(Ghost<State>);
      bool operator!=(State);
      bool operator!=(Ghost<State>);
    };

    template <class From, class To> class Map {
    public:
      To& operator[](From);
      To& operator[](From) volatile;
      bool operator==(Map<From,To>);
      bool operator==(Ghost<Map<From,To>>);
      bool operator!=(Map<From,To>);
      bool operator!=(Ghost<Map<From,To>>);
      volatile Map<From, To> operator=(const Map<From, To>&) volatile;
    };

    template <typename T> class Ghost
    {
    private:
      T _t_member_;
    public:
      Ghost(T t);
      Ghost(const Ghost<T> &);
      Ghost(const Ghost<Object> &);
      operator T() const;
      bool operator==(T);
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
    template<class T> void ContractFor(T);
    void Reads(...);
    template<class T> T Result();
    template<class T> void Returns(T);
    void Writes(...);

    // data contracts
    void DynamicOwns();
    void VolatileOwns();
    
    // object state
    template<class T> bool Activeclaim(T);
    template<class T> int Addr(T);
    template<class T1, class T2> bool Addreq(T1, T2);
    template<class T> void Always(T, bool);
    template<class T> T* Alloc();
    template<class T1, class T2> bool Approves(T1, T2);
    template<class T> T Array(T t, size_t);
    template<class T> Set Arrayrange(T t, size_t);
    template<class T> bool Arraysdisjoint(T, size_t, T, size_t);
    template<class T> T At(State, T t);
    void Atomic(...);
    template<class T> void Blobify(T);    
    template<class T> void BumpVolatileVersion(T);
    bool Claims(Claim, bool);
    template<class T> unsigned int Claimcount(T);
    template<class T> bool Claimsobject(Claim, T);
    template<class T> bool Closed(T);
    template<class T1, class T2> bool Depends(T1, T2);
    template<class T> void Destroyclaim(Claim, T);
    Set Diff(Set, Set);
    bool Disjoint(Set, Set);
    template<class T> Set Domain(T);
    template<class T> Set Extent(T);
    template<class T> bool Extentmutable(T);
    template<class T> bool Fresh(T);
    bool Fullcontext();
    template<class T> Set Fullextent(T);
    template<class T> void HavocOthers(T);
    template<class T> bool In(T, Set);
    template<class T> bool In0(T, Set);
    Set Inter(Set, Set);
    template<class T> bool Inv(T);
    template<class T, class V> bool Is(V);
    template<class T> T Labeled(const char*, T);
    template<class T1, class T2, class L> Map<T1, T2> Lambda(L);
    template<class T> Claim Makeclaim(T, bool);
    template<class T> bool Mallocroot(T);
    bool Mine(...);
    template<class T> bool Mutable(T);
    template<class T> bool Mutablearray(T, size_t);
    template<class T> bool Nonprimitiveptr(T);
    template<class T> bool Notshared(T);
    State Now();
    template<class T> bool Objectroot(T);
    template<class T> T Old(T);
    template<class T> bool Onunwrap(T, bool);
    bool Programentrypoint();
    template<class T> int Span(T);
    template<class T> bool Threadlocal(T);
    template<class T> bool Threadlocalarray(T, size_t);
    void Unblobify();
    template<class T> bool Unchanged(T);
    Set Union(Set, Set);
    Set Universe();
    void Unwrapping(...);
    template<class T> bool Valid(T);
    template<class T> T Whenclaimed(T);
    template<class T> bool Wrapped(T);
    template<class T> bool Wrapped0(T);
    
    template<class T> Set Owns(T);
    template<class T> Object Owner(T);
   
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

    // matching helper functions

    bool Matchulong(unsigned __int64) {
      _(pure)
      _(ensures \result<bool> == true)
    };

    bool Matchlong(__int64) {
      _(pure)
      _(ensures \result<bool> == true)
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
    bool Equivalence;
    bool Implies;

    // templates for quantifiers
    template<class T> bool ForAll(T);
    template<class T> bool Exists(T);
    void Trigger(...);
}
