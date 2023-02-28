# Instruction Encoding

All instructions are 64 bit wide.
The opcode is always the lowest 5 bits of the instruction word.  
A sign extended immediate of 32 bits is required by some instructions.  
If present, it is always the highest 32 bits.  
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
by the registers or immediate may be assigned further opcode-specific semantics.  
Any bits not assigned any semantics are to be zero.


# ALU Operations
The ALU has three inputs (A, B, C) and two outputs (O, I).
Whenever a bit pattern in an instruction refers to an operation that the ALU executes, it is in fact an index into the following list:

## 0x0: And
    O = A & B

## 0x1: Or
    O = A | B

## 0x2: Xor
    O = A | B

## 0x3: Not
    O = ~A

## 0x4: Neg
    O = -A

## 0x5: Add
    O = A + B + bool(C)
    I = carry

## 0x6: Sub
    O = A - B - bool(C)
    I = borrow

## 0x7: UMul
    O = lower((A * B) + C)
    I = higher((A * B) + C)

## 0x8: IMul
    O = lower((A * B) + C)
    I = higher((A * B) + C)

## 0x9: Shift Left
    O = A << B

## 0xA: Logical Shift Right
    O = A >> B

## 0xB: Arithmetic Shift Right
    O = A >> B

## 0xC: Compose
    O = (A << 32) | trunc32(B)

## 0xD: Sign Extend Byte
    O = sext(trunc8(A))

## 0xE: Sign Extend Short
    O = sext(trunc16(A))

## 0xF: Sign Extend Long
    O = sext(trunc32(A))

## 0x10: Test Equal
    O = I = A == B

## 0x11: Test Not Equal
    O = I = A != B

## 0x12: Test Greater (signed)
    O = I = A > B

## 0x13: Test Not Greater (signed)
    O = I = !(A > B)

## 0x14: Test Less (signed)
    O = I = A < B

## 0x15: Test Not Less(signed)
    O = I = !(A < B)

## 0x16: Test Above (unsigned)
    O = I = A > B

## 0x17: Test Not Above (unsigned)
    O = I = !(A > B)

## 0x18: Test Below (unsigned)
    O = I = A < B

## 0x19: Test Not Below(unsigned)
    O = I = !(A < B)

## 0x1A: Select
    O = A * bool(C) & B * !bool(C)
    I = C

## 0x1B Call Helper
    O = A + B
    I = C

## 0x1C Passthrough
    O = A
    I = B

## 0x1D Flags Passthrough
    O = C
    I = C

# Instruction Listing

## 0x0 Nop
    Does nothing.

## 0x1 Load Effective Address (%dest, %base, %index, scale[..], $offset)
    Compute a 64 bit address and store it in %dest.
    Scale is stored as in the Load instruction at bits 19..=30.
    Likewise, the registers correspond:
    %base corresponds to src0.
    %index corresponds to src1.
    %dest corresponds to dst0.
    The address is calculated as (%base + %index * (scale + 1) + $offset)

## 0x2 Store: (%value, %base, %index, scale[..], size[..], $offset)
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

## 0x3 Load: (%dest, %base, %index, scale[..], size[..], $offset)
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

## 0x4 Accumulator Op 1 (%O, %I, %A, %B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %B = src1
    %C = src2

## 0x5 Accumulator Op 2 (%O, %I, %A, %B, $C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %B = src1

## 0x6 Accumulator Op 3 (%O, %I, %A, $B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %A = src0
    %C = src2

## 0x7 Accumulator Op 4 (%O, %I, $A, %B, %C, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %I = dst1
    %B = src1
    %C = src2

## 0x8 Accumulator Op 5 (%O, %A, %B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %A = src1
    %B = src2
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register

## 0x9: Invalid

## 0xA Accumulator Op 6 (%O, %A, $B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %A = src1
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register

## 0xB Accumulator Op 7 (%O, $A, %B, op[..])
    Perform an accumulator operation on the specified
    registers.
    The operation is given by op, a 6-bit field at offset 25.
    The registers map as follows:
    %O = dst0
    %B = src2
    The C input and I output on the ALU are both
    read from and written to the 64-bit flags register
