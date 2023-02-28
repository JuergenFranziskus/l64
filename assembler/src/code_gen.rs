use std::collections::HashMap;
use num::{BigInt, ToPrimitive, Zero};
use crate::{ast::{Arg, Expr, ExprKind, Line, LineKind, Operation, Program, Register, ArgKind, OpKind, MemoryArg, BinaryOp}, span::Span};

pub struct CodeGen<'a> {
    bytes: Vec<u8>,
    offset: u64,
    labels: HashMap<&'a str, BigInt>,
    relocations: Vec<Relocation<'a>>,
}
impl<'a> CodeGen<'a> {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            offset: 0,
            labels: HashMap::new(),
            relocations: Vec::new(),
        }
    }
    pub fn finish(mut self) -> Vec<u8> {
        self.apply_relocations();
        self.bytes
    }
    fn apply_relocations(&mut self) {
        for relocation in &self.relocations {
            let value = self.eval_expr(&relocation.value).unwrap();
            let bytes = value.to_signed_bytes_le();
            for i in 0..relocation.size as usize {
                let byte = if i >= bytes.len() {
                    0
                }
                else {
                    bytes[i]
                };
                self.bytes[relocation.address as usize + i] = byte;
            }
        }
    }

    pub fn gen_code(&mut self, program: &Program<'a>) {
        for line in &program.lines {
            self.gen_line(line);
        }
    }

    fn gen_line(&mut self, line: &Line<'a>) {
        if let Some(label) = line.label {
            self.labels.insert(label, self.offset.into());
        }

        match &line.kind {
            None => (),
            Some(LineKind::Equ(val)) => {
                let Some(label) = line.label else { return };
                self.labels.insert(label, self.eval_expr(val).unwrap());
            }
            Some(LineKind::DefineBytes(args)) => self.gen_db(args),
            Some(LineKind::Op(op)) => self.gen_op(op),
        }
    }

    fn gen_db(&mut self, args: &[Expr<'a>]) {
        for arg in args {
            let val = self.eval_expr(arg).unwrap();
            let bytes = val.to_signed_bytes_le();
            self.bytes.extend_from_slice(&bytes);
            self.offset += bytes.len() as u64;
        }
    }

    fn gen_op(&mut self, op: &Operation<'a>) {
        let kind = op.op;
        let args = &op.args;


        match kind {
            OpKind::Nop => self.gen_nop(args),
            OpKind::Add => self.gen_alu_op(0x5, args),
            OpKind::Mov => self.gen_mov(args),
            OpKind::Jump => self.gen_jump(args),
            OpKind::Store => self.gen_store(args),
            OpKind::Load => self.gen_load(args),
            OpKind::Lea => self.gen_lea(args),
            OpKind::JumpIf => self.gen_jumpif(args),
            OpKind::CompareNotBelow => self.gen_alu_op(0x19, args),
            OpKind::Call => self.gen_call(args),
        }
    }
    fn gen_nop(&mut self, args: &[Arg<'a>]) {
        assert!(args.is_empty());
        self.bytes.extend_from_slice(&[0; 8]);
        self.offset += 8;
    } 
    fn gen_mov(&mut self, args: &[Arg<'a>]) {
        if args.len() == 2 {
            self.gen_alu_op(0x5, args);
        }
        else {
            assert_eq!(args.len(), 4);
            let ArgKind::Register(dst0) = args[0].kind else { panic!() };
            let ArgKind::Register(dst1) = args[1].kind else { panic!() };
    
            let mut op = 0x4;
            let mut src0 = None;
            let mut src1 = None;
    
            if self.set_reg_or_imm(&mut src0, &args[2].kind) {
                op = 0x7;
            }
            if self.set_reg_or_imm(&mut src1, &args[3].kind) {
                op = 0x6;
            }

            let src0 = src0.unwrap_or(Register(0));
            let src1 = src1.unwrap_or(Register(0));
            let src2 = Register(0);
            let imm = BigInt::zero();
    
            let opcode = Self::encode_alu_op(op, 0x1C, src0, src1, src2, dst0, dst1, &imm);
            self.bytes.extend_from_slice(&opcode.to_le_bytes());
            self.offset += 8;
        }
    }
    fn gen_jump(&mut self, args: &[Arg<'a>]) {
        let rip = Arg {
            span: Span::dummy(),
            kind: ArgKind::Register(Register(15)),
        };
        let args = [rip, args[0].clone()];
        self.gen_mov(&args);
    }
    fn gen_store(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(value) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };
        assert_eq!(args.len(), 2);

        let arg_code = self.encode_memory_arg_store_style(mem);
        let value = (value.0 as u64) << 17;

        let instruction = 0x2 | arg_code | value;
        self.bytes.extend_from_slice(&instruction.to_le_bytes());
        self.offset += 8;
    }
    fn gen_load(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dest) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };
        assert_eq!(args.len(), 2);

        let arg_code = self.encode_memory_arg_load_style(mem);
        let dest = (dest.0 as u64) << 13;

        let instruction = 0x3 | arg_code | dest;
        self.bytes.extend_from_slice(&instruction.to_le_bytes());
        self.offset += 8;
    }
    fn gen_lea(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dest) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };
        assert_eq!(args.len(), 2);

        let arg_code = self.encode_memory_arg_load_style(mem);
        let dest = (dest.0 as u64) << 13;

        let instruction = 0x1 | arg_code | dest;
        self.bytes.extend_from_slice(&instruction.to_le_bytes());
        self.offset += 8;
    }
    fn gen_jumpif(&mut self, args: &[Arg<'a>]) {
        assert_eq!(args.len(), 2);
        let is_rflags = args[1].kind == ArgKind::Flags;

        let rip = Arg {
            span: Span::dummy(),
            kind: ArgKind::Register(Register(15)),
        };
        let zero = Arg {
            span: Span::dummy(),
            kind: ArgKind::Register(Register(0)),
        };
        let new_args: Vec<Arg<'a>>;
        if is_rflags {
            new_args = vec![rip.clone(), args[0].clone(), rip, args[1].clone()];
        }
        else {
            new_args = vec!(rip.clone(), zero, args[0].clone(), rip, args[1].clone());
        }

        self.gen_alu_op(0x1A, &new_args);
    }
    fn gen_call(&mut self, args: &[Arg<'a>]) {
        assert_eq!(args.len(), 2);
        let rip = Arg {
            span: Span::dummy(),
            kind: ArgKind::Register(Register(15)),
        };
        let zero = Arg {
            span: Span::dummy(),
            kind: ArgKind::Register(Register(0)),
        };

        let args = [rip.clone(), args[1].clone(), zero, args[0].clone(), rip];
        self.gen_alu_op(0x1B, &args);
    }

    fn encode_memory_arg_store_style(&mut self, arg: &MemoryArg<'a>) -> u64 {
        let base = arg.base.unwrap_or(Register(0));
        let index = arg.index.unwrap_or(Register(0));
        let scale = arg.scale.as_ref().map(|e| self.eval_expr(e).unwrap() - 1).unwrap_or(BigInt::zero()).to_u64().unwrap();
        self.relocate_mem_arg_offset(arg.offset.as_ref());
        
        let base = (base.0 as u64) << 5;
        let index = (index.0 as u64) << 9;
        
        let scale_lowest = (scale & 0b11) << 15;
        let scale_highest = (scale & 0b111111111100) << 21;
        let size = arg.size().bit_pattern() << 13;

        base | index | scale_lowest | scale_highest | size
    }
    fn encode_memory_arg_load_style(&mut self, arg: &MemoryArg<'a>) -> u64 {
        let base = arg.base.unwrap_or(Register(0));
        let index = arg.index.unwrap_or(Register(0));
        let scale = arg.scale.as_ref().map(|e| self.eval_expr(e).unwrap() - 1).unwrap_or(BigInt::zero()).to_u64().unwrap();
        self.relocate_mem_arg_offset(arg.offset.as_ref());
        
        let base = (base.0 as u64) << 5;
        let index = (index.0 as u64) << 9;
        
        let scale_lowest = scale << 19;
        let size = arg.size().bit_pattern() << 17;

        base | index | scale_lowest | scale | size
    }
    fn relocate_mem_arg_offset(&mut self, offset: Option<&Expr<'a>>) {
        if let Some(e) = offset {
            self.relocations.push(Relocation {
                address: self.offset + 4,
                size: 4,
                value: e.clone(),
            })
        }
    }


    fn gen_alu_op(&mut self, alu_code: u64, args: &[Arg<'a>]) {
        let op = match args.len() {
            2 => self.gen_unary_alu_op(alu_code, args),
            3 => self.gen_binary_alu_op(alu_code, args),
            4 => self.gen_binary_flags_op(alu_code, args),
            5 => self.gen_full_alu_op(alu_code, args),
            _ => panic!(),
        };
        self.bytes.extend_from_slice(&op.to_le_bytes());
        self.offset += 8;
    }
    fn gen_unary_alu_op(&mut self, alu_code: u64, args: &[Arg<'a>]) -> u64 {
        assert_eq!(args.len(), 2);
        let ArgKind::Register(dst0) = args[0].kind else { panic!() };
        let dst1 = Register(0);

        let mut op = 0x4;
        let mut src0 = None;

        if self.set_reg_or_imm(&mut src0, &args[1].kind) {
            op = 0x7;
        }

        let src0 = src0.unwrap_or(Register(0));
        let src1 = Register(0);
        let src2 = Register(0);
        let imm = BigInt::zero();

        Self::encode_alu_op(op, alu_code, src0, src1, src2, dst0, dst1, &imm)
    }
    fn gen_binary_alu_op(&mut self, alu_code: u64, args: &[Arg<'a>]) -> u64 {
        assert_eq!(args.len(), 3);
        let ArgKind::Register(dst0) = args[0].kind else { panic!() };
        let dst1 = Register(0);

        let mut op = 0x4;
        let mut src0 = None;
        let mut src1 = None;

        if self.set_reg_or_imm(&mut src0, &args[1].kind) {
            op = 0x7;
        }
        if self.set_reg_or_imm(&mut src1,  &args[2].kind) {
            op = 0x6;
        }

        let src0 = src0.unwrap_or(Register(0));
        let src1 = src1.unwrap_or(Register(0));
        let src2 = Register(0);
        let imm = BigInt::zero();

        Self::encode_alu_op(op, alu_code, src0, src1, src2, dst0, dst1, &imm)
    }
    fn gen_binary_flags_op(&mut self, alu_code: u64, args: &[Arg<'a>]) -> u64 {
        assert_eq!(args.len(), 4);
        let ArgKind::Register(dst0) = args[0].kind else { panic!() };
        let ArgKind::Flags = args[3].kind else { panic!() };
        let dst1 = Register(0);

        let mut op = 0x8;
        let mut src0 = None;
        let mut src1 = None;

        if self.set_reg_or_imm(&mut src0, &args[1].kind) {
            op = 0xB;
        }
        if self.set_reg_or_imm(&mut src1, &args[2].kind) {
            op = 0xA;
        }

        let src0 = src0.unwrap_or(Register(0));
        let src1 = src1.unwrap_or(Register(0));
        let src2 = Register(0);
        let imm = BigInt::zero();

        Self::encode_alu_op(op, alu_code, src0, src1, src2, dst0, dst1, &imm)
    }
    fn gen_full_alu_op(&mut self, alu_code: u64, args: &[Arg<'a>]) -> u64 {
        assert_eq!(args.len(), 5);
        let ArgKind::Register(dst0) = args[0].kind else { panic!() };
        let ArgKind::Register(dst1) = args[1].kind else { panic!() };

        let mut op = 0x4;
        let mut src0 = None;
        let mut src1 = None;
        let mut src2 = None;

        if self.set_reg_or_imm(&mut src0, &args[2].kind) {
            op = 0x7;
        }
        if self.set_reg_or_imm(&mut src1, &args[3].kind) {
            op = 0x6;
        }
        if self.set_reg_or_imm(&mut src2, &args[4].kind) {
            op = 0x5;
        }

        let src0 = src0.unwrap_or(Register(0));
        let src1 = src1.unwrap_or(Register(0));
        let src2 = src2.unwrap_or(Register(0));
        let imm = BigInt::zero();

        Self::encode_alu_op(op, alu_code, src0, src1, src2, dst0, dst1, &imm)
    }

    fn set_reg_or_imm(&mut self, reg: &mut Option<Register>, arg: &ArgKind<'a>) -> bool {
        match arg {
            ArgKind::Expression(e) => {
                self.relocations.push(Relocation {
                    address: self.offset + 4,
                    size: 4,
                    value: e.clone(),
                });
                true
            },
            &ArgKind::Register(r) => { *reg = Some(r); false },
            _ => panic!(),
        }
    }

    fn encode_alu_op(
        opcode: u64,
        alu_code: u64,
        src0: Register,
        src1: Register,
        src2: Register,
        dst0: Register,
        dst1: Register,
        immediate: &BigInt,
    ) -> u64 {
        let opcode = opcode << 0;
        let alu_code = alu_code << 25;
        let src0 = (src0.0 as u64) << 5;
        let src1 = (src1.0 as u64) << 9;
        let src2 = (src2.0 as u64) << 17;
        let dst0 = (dst0.0 as u64) << 13;
        let dst1 = (dst1.0 as u64) << 21;
        let immediate = immediate.to_u64().unwrap() << 32;

        opcode | alu_code | src0 | src1 | src2 | dst0 | dst1 | immediate
    }

    fn eval_expr(&self, expr: &Expr) -> Option<BigInt> {
        match &expr.kind {
            ExprKind::Integer(val) => Some(val.clone()),
            &ExprKind::Label(l) => self.labels.get(l).cloned(),
            &ExprKind::String(str) => {
                Some(BigInt::from_signed_bytes_le(str.as_bytes()))
            }
            &ExprKind::Binary(ref left, op, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                let result = match op {
                    BinaryOp::Add => left + right,
                    BinaryOp::Sub => left - right,
                };
                Some(result)
            }
            &ExprKind::Here => Some(self.offset.into())
        }
    }
}




struct Relocation<'a> {
    address: u64,
    size: u64,
    value: Expr<'a>,
}
