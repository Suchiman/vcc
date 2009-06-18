Preparation:

  1. Open the solution, make sure it compiles (F6), with just a few warnings about multiple definitions of __bitset etc.; 
     if it complains about vcc2.h not being found, set the appropriate additional include path in 
     Project-> VccDemo Properties…->Configuration Properties->C/C++->General->Additional Include Directories
     
  2. Open SpinLock.h and make sure that #define SIMPLE_SPIN_LOCKS has not been commented out
  
  3. If you want to show how model inspection works, remove pre-condition 
     "  requires(access_claim != SpinLock->protected_obj) " from the Release function in the non-simple case


Basics:

* 

Concurrency:

* Using SpinLock.h and SpinLock.c can be used to explain the basics of concurrent data structures, and some other concepts:
  - volatile fields
  - spec fields
  - spec parameters
  - atomic writes (which do not require an unwrap)
  - ghost updates that happen instantaneous; this lattern can be shown by moving the set_closed_owner outside 
    of the atomic block in the Release function, which shows that the invariant is violated at the end of the
    atomic block, which also shows up nicely in the error model
  
  The lock as it is there cannot really be used because we still need to have it wrapped (i.e., owned by the current
  thread) to use it, which defeats the idea of a lock. But for the moment, we glance over this as we will run into
  this problem once we try to use the lock.
  
* Using Client.c, we can show how to use the simplified lock: LockContainer now owns the lock that has the data
  structure that we are interested in as its protected_obj
  - show initialization of the thing
  - Using the thing does work but requires ownership of the LockContainer, which makes is unsuitable for the
    concurrent calse
      
Claims:      
      
* Introduce claims:
 - comment out the SIMPLE_SPIN_LOCKS in SpinLock.h
 - instead of requiting the SpinLock to be wrapped, we require a claim that guarantees that the lock is closed
 - look that the implementation of Release and Acquire; it is still mostly the same with the difference being
   that the atomic clause uses the claim to prove closedness; removing the claim there will show up as a nice
   error when trying to verify Acquire
 - this is also a good opportunity to show the model viewer. For this, you will need to have done the preparation
   #3. If you now try to verify Require, verification will fail. Right-click the closing braces and show the error
   model. It will immediately show that access_claim and protected object are aliasing each other. Aha, the lock
   cannot be used to protect its own access claim - makes sense. Add 
   
   requires(access_claim != SpinLock->protected_obj)
   
   to the contract in SpinLock.h and the function verifies ok.
   

   
* Look at how to use claims to access the Lock in a concurrent setting
 - The ConcurrentUser does not own the lock container, instead is has a claim on the lock container
 - Initialization of ConcurrentUser does not relinquish ownership of the LockContainer, so multiple 
   ConcurrentUsers can be created and potentially be handed to multiple threads
 - Access can be achieved by just owning the ConcurrentUser and not the LockContainer
