# Heuristics used for synthesis

This file lists the major heuristics used for synthesis.

## Initial row

Initial row is always assumed as
    CFA     rbp   ra
    rsp+8   u     c-8

## With or without %rbp?

When synthesizing a FDE, there is sometimes a choice between using %rbp or
not. For instance, it is possible that the original program uses %rbp for
something entirely different than keeping a base pointer, without it being
obvious: the synthesis must then avoid using %rbp.

When synthesizing a FDE, two passes are applied on the function: a first pass
that tracks %rbp to generate a correct table, but is denied using %rbp as an
indexing mean for CFA. If this first pass fails by losing track of its CFA at
some point, we fall back to a second phase that does the same, but switches its
CFA indexing to %rbp if possible.

This method works in practice because
 * if the first pass succeeded, then a correct CFA indexing was found,
 * if not, the original compiler could not generate a correct CFA indexing
   either and was forced to use %rbp as a base pointer (except corner cases,
   eg. clang sometimes generate code without possible correct unwinding data in
   pre-abort error handling paths)

## Lossy merge

When two or more code branches merge at some point, we require that the
unwinding data propagated by all of the branches can be merged into
consistent data.

Most of the time, *consistent* means strictly equivalent, but it can be
weakened by allowing rows with %rbp undefined on one side and defined on the
other to be merged — thus assuming the merged data is %rbp undefined, allowing
a information loss.

We actually process the control flow graph of a subroutine by walking it
depth-first. When first encountering a new block, the propagated row is saved
as the initial data for this block. When we encounter it again from another
predecessor, the propagated row is merged if possible, or aborts with
inconsistency. This merge operation is thus algorithmically free if the data
first stored in the block is %rbp undefined — it is possible to just erase the
data on the newly merged unwinding data. The other way around, changing the
data already present, with which subsequent computations have already been
made, would require recomputing a lot of data. We thus *only allow it* if the
block is a leaf block in the control flow graph of the subroutine.

This restriction in the application conditions works well in practice because
gcc does not generate such lossy merges, and clang generates those only for the
exit block of a function — just before `retq`.

## CFA state tracking

### When CFA is an offset of %rsp

If the CFA is an offset of %rsp, it must be kept up to date when %rsp changes.
In the BAP IR, every such change will generate some instruction `%rsp <- EXPR`.

 * If the expression is just `%rsp <- %rsp + offset`, the CFA is updated with
   this offset (most cases).
 * If not, the analysis loses track and aborts. This case did not occur during
   our testing while the CFA was indexed by %rsp.

### When CFA is offset of %rbp

If the CFA is an offset of %rbp, nothing special is required to track the CFA.

### Switching between the modes: %rsp to %rbp indexing

If the CFA is currently an offset of %rsp, an indexing mode change is detected
when %rip is saved to %rbp. If the synthesis is currently allowed to use %rbp
indexing (see *With or without %rbp?*), the indexing mode is then switched. If
not, the current CFA indexing is kept.

### Switching between the modes: %rbp to %rsp indexing

The only event that triggers a revert to %rsp-based indexing is when %rbp gets
overwritten with something while %rbp indexing.

It is non-trivial to decide which %rsp offset should be used when switching
back. So far, we have only encountered switches back to %rsp at the very end of
functions — when %rbp was popped from the stack. Thus, we thus assume that upon
restore, CFA=%rsp+8. This only works in practice since in the observed cases,
compilers tend to stick to %rbp indexing when they decide to use it in a
function.

## %rbp state tracking

Tracking the state of %rbp (or any other callee-saved register) can be done by
tracking the program points at which

 * %rbp is undefined and an instruction saves %rbp to the stack,
 * %rbp is defined and an instruction overwrites %rbp with the data initially
   saved on the stack
