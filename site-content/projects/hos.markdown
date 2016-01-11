---
title: Hos
author: Travis Athougies
slug: hos
summary: Hos is an operating system for X86-64, written in mainly Haskell 98 for the JHC compiler. It consists of a microkernel and a few user-space drivers, including a simple ATA driver, but no filesystem.
language: Haskell
status: INPROGRESS
priority: 1
github: https://github.com/tathougies/hos
version: 0.0.1
---

# Introduction

Hos is an operating system consisting of a microkernel and a few user-space programs.
It is written mostly in JHC-compatible Haskell 98, with fallbacks to C when necessary (garbage collection, for example).

For an overview of how to build and run Hos, see the project page on [Github](https://github.com/tathougies/hos).

Continue reading for a quick tour around the source code.

## To Long Mode and Beyond...

The main Hos kernel runs in long mode. We use ISOLINUX to start a small multiboot-compatible kernel written in X86-64 assembler
that takes us from protected mode directly into long mode. This can be found at `cbits/boot.S`. This file also initializes the
garbage collector (`bootstrap_kernel`), JHC run-time system (see the call to `jhc_hs_init`), and finally calls the `main` haskell
function (see call to `_amain`).

This boot code also maps our kernel into the address space at `0xffffff7f00000000`. This gives the kernel 4 GB of room,
because the top 512 GB are used for a self-referential mapping of the page tables themselves.

## Dealing with memory

Operating systems have to be familiar with the intimate details of memory, but Haskell is a garbage collected language. This
means we must resort to C for the garbage collector.

The `bootstrap_kernel` function in `cbits/multiboot.c` deals with all the memory setup prior to running the main kernel.
This function maps the multiboot modules into visible address space and initializes the buddy page allocator (`initalize_regions`).

The page allocator allows us to allocate and free physical pages. We tie this into the JHC run-time system by mapping the
allocated physical pages into the next available address in the kernel memory region. Once a kernel page is allocated it
is never freed, but the memory is re-used. This should eventually change if we were to make Hos into a "real" OS.

The code for the page allocator is in `cbits/multiboot.c` in the `alloc_from_regions` and `free_from_regions`. The allocator
is initialized in the `initialize_regions` function in the same file, which uses the multiboot memory map to create the
proper free regions.

The page allocation system used is a simple buddy based allocator, explained in depth [here](http://wiki.osdev.org/Page_Frame_Allocation#Buddy_Allocation_System).

## The run-time system

A modified version of the JHC run-time system is in the `rts` directory. The JHC garbage collector only needs three calls to
integrate with a new memory system. These calls are `ext_page_aligned_alloc` which returns the virtual address of newly 
allocated pages, `ext_free` which frees an allocation made by `ext_page_aligned_alloc`, and `ext_alloc_megablock` which returns
a new megablock. Essentially, these calls all delegate to the buddy physical page allocator, and then establish the correct
virtual mapping.

This is all we need to run Haskell bare bones!

## The main kernel

The main kernel is located in the `src` directory. The entry point is at `src/main.hs`. The `hosMain` function takes in
an `Arch` record, corresponding to architecture-specific calls, and produces an IO action that will run the operating system.

It begins by parsing the init tasks ELF headers from the multiboot module using the C code to locate the module.
It then creates a new init task, adds it to the task table, prepares the system to enter userspace, and switches to user
space almost immediately.

The kernel is structured as a co-routine with user space. Essentially, we yield to userspace using the `archSwitchToUserspace`
function, which runs the userspace continuation, until a system call is made or a trap is hit. We then handle the specific
details, and switch back into userspace. This entire system is in the `kernelize` function in `main.hs`. Note the large
case expression in this function, which handles all the reasons we may have come back from userspace.

I won't deal with the architecture-specific code here, but it should look familiar to anyone with experience in operating systems.

## User space

Just as we have a kernel-level run-time system, there is also a user-level one at `progs/hos`. This re-exports enough C standard
library functions to let the vanilla JHC run-time system run in Hos userspace. Notably, it uses dlmalloc to provide memory management
and a C-level Hos system call library to provide `sbrk` functionality (see `progs/hos/hos.c`).

Currently, I've written four user-level drivers:

* *init* which handles message routing between the drivers, as well as auto-starting the other drivers
* *storage* which exposes what will become our file system. This server exposes a simple key-value store that allows querying via tags.
* *pci* which reads in PCI configuration data and uses *storage* to auto-start appropriate servers
* *ata* which scans the ATA Bus discovered by *pci*, and reads the first 8 kilobytes of our ATAPI CD-ROM into memory at 0x18000000000.
  You can verify that it works by using the virtual box debugging console.
  
# Conclusion

Hos is still unfinished. I would like to make the ATA driver auto-start an ISO9660 filesystem driver which will expose the CD objects
to *storage* as objects. Then, I would like to create a simple VGA text console driver, and load it from the CD using *init*.

This would allow us to make a much more impressive demo. Patches welcome. [Ping me](mailto:travis@athougies.net) if you have any questions. Happy hacking!
