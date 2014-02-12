#Outstanding Issues

##~~CPSR~~ DONE

Computing the correct status flags after an instruction is not possible with the current implementation. Currently the result of a computation is inspected, from which the Z and N flags can be correctly set. The V and C flags require additional information about the operands, and the exact behaviour of the V and C flags depends on the particular instruction being executed.

The current structure with `compute<Arity>Op` higher order functions needs to be revised, and the status flags resulting from a computation need to be computed as part of the operation.

It would also be easier to compute status flags if operations were performed on 64 bit unsigned values and then transformed back into 32 bit signed values, since the C flag could be calculated by checking bit 32.
