# Instruction Encoding

All instructions are 64 bit wide.
The opcode is always the lowest 5 bits of the instruction word.  
A sign extended immediate of 33 bits is required by some instructions.  
If present, it is always the highest 33 bits.  
An instruction may encode up to two destination register
and three source registers.  
There being 16 registers, this encoding may take up to 20 bits,
encoded at these offsets in the instruction word:

- 05..=08: src0
- 09..=12: src1
- 13..=16: dst0
- 17..=20: src2
- 21..=24: dst1,

Any bits not assigned meaning for some opcode  
by the registers or immediate may be assigned further opcode-specific semiantics.  
Any bits not assigned any semantics are to be zero.


# ALU Operations
The ALU has three inputs (A, B, C) and two outputs (O, I).
Whenever a bit pattern in an instruction refers to an operation that the ALU executes, it is in fact an index into the following list:

## 0x0: And / Or
    O = A & B & C 
    I = A | B | C

## 0x1: Not / Neg
    O = ~A
    I = -A

## 0x3: Xor / Select
    O = (A * bool(C)) | (B * !bool(C))
    I = A ^ B ^ C

## 0x4: Add
    O = A + B
    I = carry

## 0x5: Sub
    O = A - B
    I = borrow

## 0x6: UMul (unsigned)
    O = lower(A * B + C)
    I = higher(A * B + C)

## 0x7: IMul (signed)
    O = lower(A * B + C)
    I = higher(A * B + C)

## 0x8: Shift Left / Compose
    O = (A << B) + C
    I = ((A << 32) | B) + C

## 0x9: Sign Extend 01
    O = sext(trunc32(A))
    I = sext(trunc16(A))

## 0xA: Sign Extend 02
    O = sext(trunc8(A))
    I = sext(trunc1(A))

## 0xB: Shift Right
    O = (A >> B) + C (logical)
    I = (A >> B) + C (arithmetic)

## 0xC: Test Equal / Test Not Equal
    O = A == B
    I = A != B

## 0xD: Test Less (signed)
    O = A < B
    I = !(A < B)

## 0xE: Test Greater (signed)
    O = A > B
    I = !(A > B)

## 0xF: Test Below (unsigned)
    O = A < B
    I = !(A < B)

## 0xF: Test Above (unsigned)
    O = A > B
    I = !(A > B)

## 0x10: Passthrough
    O = A | C
    I = B | C

## Call Helper
    O = A + B
    I = C

# Instruction Listing

## Nop
    Does nothing.

## Load Effective Address (%dest, %base, %index, scale[..], $offset)
    Compute a 64 bit address and store it in %dest.
    Scale is stored as in the Load instruction at bits 19..=30.
    Likewise, the registers correspond:
    %base corresponds to src0.
    %index corresponds to src1.
    %dest corresponds to dst0.
    The address is calculated as (%base + %index * (scale + 1) + $offset)

## Store: (%value, %base, %index, scale[..], size[..], $offset)
    Store a register's value into memory.  
    Scale is a zero extended numeric value interspersedly stored at the following offsets, in order from lowest to highest:
    - 15..=16
    - 21..=30
    The address is calculated as (%base + %index * (scale + 1) + $offset).
    %value corresponds to src2.
    %base corresponds to src0.
    %index corresponds to src1.
    Size is a 2-bit value indicating the amount of bytes to write
    stored at offset 13, interpreted thusly:
    - 0: 1 byte
    - 1: 2 bytes
    - 2: 4 bytes
    - 3: 8 bytes

## Load: (%dest, %base, %index, scale[..], size[..], $offset)
    Load a value from memory into a register.  
    Scale is a zero extended numeric value interspersedly stored at the following offsets, in order from lowest to highest:
    - 19..=30
    The address is calculated as (%base + %index * (scale + 1) + $offset).
    %base corresponds to src0.
    %index corresponds to src1.
    %dest corresponds to dst0.
    Size is a 2-bit value indicating the amount of bytes to read stored at offset 17, interpreted thusly:
    - 0: 1 byte
    - 1: 2 bytes
    - 2: 4 bytes
    - 3: 8 bytes
    The read value is zero-extended to 64 bits before being stored
    into the %dest register.

## Accumulator Op 1 (%O, %I, %A, %B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %B = src1
    %C = src2

## Accumulator Op 2 (%O, %I, %A, %B, $C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %B = src1

## Accumulator Op 3 (%O, %I, %A, $B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %C = src2

## Accumulator Op 4 (%O, %I, $A, %B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %B = src1
    %C = src2

## Accumulator Op 5 (%O, %A, %B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %A = src1
    %B = src2
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register

## Accumulator Op 6 (%O, %A, $B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %A = src1
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register

## Accumulator Op 7 (%O, $A, %B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %B = src2
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register
