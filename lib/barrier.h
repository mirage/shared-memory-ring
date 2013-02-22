/* Taken from include/minios/x86/os.h
 *
 * random collection of macros and definition
 */

#ifndef _BARRIER_H_
#define _BARRIER_H_

/* This is a barrier for the compiler only, NOT the processor! */
#define barrier() asm volatile ( "" : : : "memory")

#if defined(__i386__)
#define xen_mb()  asm volatile ( "lock; addl $0,0(%%esp)" : : : "memory" )
#elif defined(__x86_64__)
#define xen_mb()  asm volatile ( "mfence" : : : "memory")
#elif defined(__arm__)
#define xen_mb()   asm volatile ("dmb" : : : "memory")
#else
#error "I don't know how to implement barriers for this architecture"
#endif

#endif /* _BARRIER_H_ */
