//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

#include <vccpp-m.h>

#ifndef VERIFY

// hide annotations from C compiler

#define _(...)        /* nothing */
#define vcc_attr(...) /* nothing */

#else

void free(void*, size_t);
#define vcc_attr(k, v) __declspec("System.Diagnostics.Contracts.CodeContract.StringVccAttr", k, v)

#endif 

namespace VCC
{
    // types
    template<class T> class Ghost;

    class Thread {
    public:
      bool operator==(Thread);
      bool operator!=(Thread);
    };
    
    class Claim {
    public:
      Claim();
      Claim(const Claim&);
      Claim(const volatile Claim&);
      Claim operator=(const Ghost<Claim>&) volatile;
      Claim operator=(const volatile Claim&);
      bool operator==(Claim);
      bool operator==(void*);
      volatile bool operator==(Claim) volatile;
      bool operator==(Ghost<Claim>);
      bool operator!=(Claim);
      bool operator!=(void*);
      volatile bool operator!=(Claim) volatile;
      bool operator!=(Ghost<Claim>);
    };

    class Integer {
    public:
      Integer();
      Integer(int);
      Integer operator-=(Integer);
      Integer operator+=(Integer);
      Integer operator/(Ghost<Integer>);
      Integer operator%(Ghost<Integer>);
      operator int() const;
    };

    class Natural {
    public:
      Natural();
      Natural(unsigned);
      Natural operator-=(Natural);
      Natural operator+=(Natural);
      Natural operator/(Ghost<Natural>);
      Natural operator%(Ghost<Natural>);
      operator unsigned() const;
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

    class Object {
    public:
      Object();
      Object(const Object&);
      Object(const volatile Object&);
      Object(void*);
      bool operator==(void*);
      bool operator==(Ghost<Object>) volatile;
      bool operator==(Ghost<Claim>) volatile;
      bool operator==(Ghost<Set>) volatile;
      bool operator==(Ghost<Thread>) volatile;
      bool volatile operator==(void*) volatile;
      bool operator!=(void*);
      bool operator!=(Ghost<Object>) volatile;
      bool operator!=(Ghost<Claim>) volatile;
      bool operator!=(Ghost<Set>) volatile;
      bool operator!=(Ghost<Thread>) volatile;
      bool volatile operator!=(void*) volatile;
      Object operator=(void*);      
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
      volatile State operator=(const State&) volatile;
    };

    template <class From, class To> class Map {
    private:
      From *_from_member_;
      To *_to_member_;
    public:
      To& operator[](From);
      To& operator[](From) volatile;
      bool operator==(Map<From,To>);
      bool operator==(Ghost<Map<From,To>>);
      bool operator!=(Map<From,To>);
      bool operator!=(Ghost<Map<From,To>>);
      volatile Map<From, To> operator=(const Map<From, To>&) volatile;
    };

    template <class T> class Ghost
    {
    private:
      T *_t_member_;
    public:
      Ghost(T t);
      Ghost(const Ghost<T> &);
      Ghost(const Ghost<Object> &);
      operator T() const;
      operator Ghost<Object>() const;
      bool operator==(T);
      bool operator!=(T);
      bool operator==(Object);
      bool operator!=(Object);
      T operator ->();
    };
      
    template <class S, class T> class Ghost< Map <S, T> >
    {
    public:
      T& operator[](S);
      operator Map<S, T>() const;
    };

    template <class T> class GhostOut: public Ghost<T>
    {
    private:
      T *_t_member_;
    public:
      GhostOut(T t);
      GhostOut(const GhostOut<T> &);
      GhostOut(const GhostOut<Object> &);
      operator T() const;
      operator GhostOut<Object>() const;
      bool operator==(T);
      T operator ->();
    };
   
    template <class T> class TypeLockageFunctor {
    public:
      T operator()(T);
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
    template<class T> bool Accountclaim(Claim, T);
    template<class T> bool Activeclaim(T);
    template<class T> T Activemember(T);
    template<class T> Integer Addr(T);
    template<class T1, class T2> bool Addreq(T1, T2);
    template<class T> T* Allocarray(size_t);
    template<class T> void Always(T, bool);
    template<class T> bool Alwaysbyclaim(Claim, T);
    template<class T> T* Alloc();
    template<class T1, class T2> bool Approves(T1, T2);
    template<class T> T Array(T, size_t);
    template<class T> T Arraymembers(T, size_t);
    template<class T> Set Arrayrange(T t, size_t);
    template<class T> bool Arraysdisjoint(T, size_t, T, size_t);
    void AssumeCorrect();
    template<class T> T At(State, T t);
    void Atomic(...);
    template<class T> TypeLockageFunctor<T> AtomicOp(...);
    template<class T> TypeLockageFunctor<T> AtomicRead(...);
    template<class T> void BeginGhostAtomic(T);
    template<class T> Object Blob(T);
    template<class T> Object BlobOf(T);
    template<class T1, class T2> Object Blob(T1, T2);
    template<class T> void Blobify(T);    
    template<class T> void BumpVolatileVersion(T);
    template<class T> TypeLockageFunctor<T> ByClaim(Claim);
    template<class T> bool Claimable(T);
    template<class T> unsigned int Claimcount(T);
    bool Claims(Claim, bool);
    template<class T> bool Claimsobject(Claim, T);
    bool Claimsclaim(Claim, Claim);
    template<class T> bool Closed(T);
    template<class T> void Decreases(T, ...);
    template<class T> bool Deepeq(T, T);
    template<class T1, class T2> bool Depends(T1, T2);
    template<class T> void Destroyclaim(Claim, T);
    Set Diff(Set, Set);
    bool Disjoint(Set, Set);
    template<class T> Set Domain(T);
    template<class T> T Domainroot(T);
    template<class T> bool Domainupdatedat(T, Set);
    template<class T> T Embedding(T);
    void EndGhostAtomic();
    template<class T> Set Extent(T);
    template<class T> bool Extentfresh(T);
    template<class T> bool Extentmutable(T);
    template<class T> void Free(T*);
    bool FreeRequiresOrEnsures(bool);
    template<class T> bool Fresh(T);
    bool Fullcontext();
    template<class T> Set Fullextent(T);
    template<class T> Object Gemb(T);
    template<class T> void HavocOthers(T);
    template<class T> bool In(T, Set);
    template<class T> bool In0(T, Set);
    template<class T1, class T2> bool Inarray(T1, T2, size_t);
    template<class T> void Increases(T, ...);
    template<class T1, class T2> int Indexwithin(T1, T2);
    Set Inter(Set, Set);
    template<class T> bool Inv(T);
    template<class T> bool Inv2(T);
    template<class T> bool Inv2s(State, State, T);
    template<class T> bool Inrangephysptr(T);
    template<class T, class V> bool Is(V);
    template<class T> bool Isarray(T, size_t);
    template<class T> bool Isghost(T);
    void IsolateProof();
    template<class T1, class T2> void JoinBlobs(T1, T2);
    template<class T> T Labeled(const char*, T);
    template<class T1, class T2, class L> Map<T1, T2> Lambda(L);
    template<class T1, class T2, class T3, class L> Map<T1, Map<T2, T3>> Lambda(L);
    template<class T1, class T2, class T3, class T4, class L> Map<T1, Map<T2, Map<T3, T4>>> Lambda(L);
    template<class T> Claim Makeclaim(T, bool);
    template<class T> bool Mallocroot(T);
    bool Mine(...);
    template<class T> bool Mutable(T);
    template<class T> bool Mutablearray(T, size_t);
    template<class T> bool Nested(T);
    template<class T> bool Nonprimitiveptr(T);
    void NoReadsCheck();
    bool Normalexit();
    template<class T> bool Notshared(T);
    State Now();
    template<class T> bool Objectroot(T);
    template<class T> T Old(T);
    template<class T> bool Onunwrap(T, bool);
    Integer Plus(Integer, Integer);
    template<class T> T Precise(T);
    template<class T> T ReadOnly(T);
    void ReadsHavoc();
    template<class T> void RecursiveWith(T);
    template<class T> T Retype(T);
    template <class T> TypeLockageFunctor<T> RootArray(size_t);
    template <class T> TypeLockageFunctor<T> RootIndex(size_t);
    bool Programentrypoint();
    template<class T> bool Shalloweq(T, T);
    template<class T> size_t Size(T);
    size_t Sizeofobject(Object);
    void SkipSmoke();
    void SplitBlob(Object, size_t);
    template<class T> Set Span(T);
    bool Starthere();
    template<class T> bool Threadlocal(T);
    template<class T> bool Threadlocalarray(T, size_t);
    template<class T> T Unblobify(T);
    template<class T> bool Unchanged(T);    
    template<class T> T Unchecked(T);
    Set Union(Set, Set);
    template<class T> bool Unionactive(T);
    template<class T> void UnionReinterpret(T);
    Set Universe();
    void Unwrapping(...);
    Claim Upgradeclaim(Set, bool);
    template<class T> bool Valid(T);
    template<class T> Set Vdomain(T);
    template<class T> T Whenclaimed(T);
    template<class T> bool Wrapped(T);
    template<class T> bool Wrappedwithdeepdomain(T);
    template<class T> bool Wrapped0(T);
    template<class T> bool Writable(T);
    
    template<class T> Set Owns(T);
    template<class T> Object Owner(T);
    
    template <class T> Ghost<T> CreateGhost(Ghost<T>);
    template <class T> Ghost<T> CreateGhost(T);
    template <class T> GhostOut<T> CreateGhostOut(GhostOut<T>);
    template <class T> GhostOut<T> CreateGhostOut(T);

    // matching helper functions

    bool Matchulong(unsigned __int64) {
      _(pure)
      _(ensures \result == true)
      return true;
    };

    bool Matchlong(__int64) {
      _(pure)
      _(ensures \result == true)
      return true;
    };

    // statements
    template<class T> void Wrap(T o, ...)
    {
      VCC::Writes(o); 
      VCC::Writes(VCC::Owns(o));
    }

    template<class T> void Unwrap(T o, ...)
    {
      VCC::Writes(o);
    }

    // special variables
    bool Equivalence;
    bool Implies;
    bool ReverseImplies;
    Thread Me;

    // templates for quantifiers
    template<class T> bool ForAll(T);
    template<class T> bool Exists(T);
    void Trigger(...);

    void AstHelper() {
      // reference some functions so we are sure that are in the AST even when they are not explicitly referenced
      VCC::Wrap(0);
      VCC::Unwrap(0);
    }
}
