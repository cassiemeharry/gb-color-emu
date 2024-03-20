//! JIT'ed CPU for the SM83 (as seen in the GameBoy Color)
//!
//! Every time we jump to a new address, we JIT compile the next string of
//! instructions until the next (possibly conditional) jump. This will
//! drastically reduce the interpreter overhead.
//!
//! IO is simulated at a high level. The T-cycle level delays are ignored
//! (though the cycle counts should still be accurate).

#![allow(non_snake_case)]

use dynasmrt::{DynasmApi, DynasmLabelApi};
use std::{fmt, mem::MaybeUninit, rc::Rc};

use crate::{
    cart::Cart,
    interval_tree::IntervalTree,
    lcd::LcdController,
    memory::{Memory, MemoryMapChange as MMC},
    system::{SystemRef, RwSource},
    screen::Screen,
    sound::APU,
};

pub(crate) mod debuginfo;

#[cfg(test)]
mod test;

// pub use gbc_asm::gbc_asm;

#[derive(Copy, Clone, PartialEq)]
pub struct WriteRecord {
    pub target_address: u16,
    pub data: u8,
}

impl PartialEq<(u16, u8)> for WriteRecord {
    fn eq(&self, &(a, d): &(u16, u8)) -> bool {
        self.target_address == a
            && self.data == d
    }
}

impl From<(u16, u8)> for WriteRecord {
    fn from((a, d): (u16, u8)) -> Self {
        Self { target_address: a, data: d }
    }
}

impl fmt::Debug for WriteRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("WriteRecord")
            .field("addr", &format_args!("{:#06x}", self.target_address))
            .field("data", &format_args!("{:#04x}", self.data))
            .finish()
    }
}

// JIT depends on registers being the first field here, so we can transmute a
// pointer from CPU to a pointer to Registers.
#[repr(C)]
#[derive(Clone)]
pub struct CPU {
    pub(crate) registers: Registers,
    assembled: IntervalTree<u16, Rc<OwnedJittedFunc>>,
    pub(crate) memory_map_changes: Vec<MMC>,
    // #[cfg(test)]
    pub writes: Vec<WriteRecord>,
    dma: DMA,
    cycles: u64,
}

impl fmt::Debug for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("CPU")
            .field("registers", &self.registers)
            .field("memory_map_changes", &self.memory_map_changes)
            .field("writes", &self.writes)
            .field("cycles", &self.cycles)
            .finish_non_exhaustive()
    }
}

struct Sys<'a> {
    cart: &'a mut Cart,
    lcd: &'a mut LcdController,
    memory: &'a mut Memory,
    screen: &'a mut Screen,
    sound: &'a mut APU,
}

impl Sys<'_> {
    fn as_full<'b>(&'b mut self, cpu: &'b mut CPU) -> SystemRef<'b> {
        SystemRef {
            cart: &mut *self.cart,
            cpu,
            lcd: &mut *self.lcd,
            memory: &mut *self.memory,
            screen: &mut *self.screen,
            sound: &mut *self.sound,
        }
    }
}

#[derive(Clone, Debug, Default)]
struct DMA {
    source: Option<u16>,
}

impl CPU {
    pub fn new() -> Self {
        let mut registers = Registers::default();
        registers.interrupt_master_enable = 1;
        registers.interrupt_enable = InterruptFlags::empty();
        registers.interrupt_flags = InterruptFlags::from_bits_truncate(0xe1);
        Self {
            registers,
            assembled: IntervalTree::new(),
            memory_map_changes: vec![],
            // #[cfg(test)]
            writes: vec![],
            dma: DMA::default(),
            cycles: 0,
        }
    }

    #[inline]
    pub fn get_registers<'a>(self: &'a CPU) -> &'a Registers {
        &self.registers
    }

    #[inline]
    pub(crate) fn dma_active(&self) -> bool {
        self.dma.source.is_some()
    }

    #[inline]
    pub(crate) fn start_dma(&mut self, source_upper: u8) {
        assert!(self.dma.source.is_none());
        let source = (source_upper as u16) << 8;
        debug!("Starting DMA from {:#06x}", source);
        self.dma.source = Some(source);
    }

    fn run_dma(&mut self, this_cycles: u64) {
        if let Some(source) = self.dma.source.take() {
            todo!()
        }
    }

    /// Run the CPU until the next jump or halt, returning the number of cycles
    /// elapsed.
    #[inline(never)]
    pub(crate) fn step(&mut self, cart: &mut Cart, lcd: &mut LcdController, memory: &mut Memory, screen: &mut Screen, sound: &mut APU) -> u64 {
        self.writes.clear();

        let mut sys = Sys { cart, lcd, memory, screen, sound };

        let pending_interrupts = self.registers.interrupt_flags & self.registers.interrupt_enable;
        // if !pending_interrupts.is_empty() {
        //     debug!("CPU pending interrupts: {:?}", pending_interrupts);
        // }
        let prev_pc = self.registers.pc;
        let cycles = if self.registers.interrupt_master_enable != 0 && !pending_interrupts.is_empty() {
            // Handle interrupts manually.
            info!("CPU is handling interrupt: {:?}", pending_interrupts);
            self.registers.interrupt_master_enable = 0;
            let handler_pc = if self.registers.interrupt_flags.contains(InterruptFlags::VBLANK) {
                self.registers.interrupt_flags.remove(InterruptFlags::VBLANK);
                0x0040
            } else if self.registers.interrupt_flags.contains(InterruptFlags::LCD_STAT) {
                self.registers.interrupt_flags.remove(InterruptFlags::LCD_STAT);
                0x0048
            } else if self.registers.interrupt_flags.contains(InterruptFlags::TIMER) {
                self.registers.interrupt_flags.remove(InterruptFlags::TIMER);
                0x0050
            } else if self.registers.interrupt_flags.contains(InterruptFlags::SERIAL) {
                self.registers.interrupt_flags.remove(InterruptFlags::SERIAL);
                0x0058
            } else if self.registers.interrupt_flags.contains(InterruptFlags::JOYPAD) {
                self.registers.interrupt_flags.remove(InterruptFlags::JOYPAD);
                0x0060
            } else {
                unreachable!()
            };
            let mut jit_data = sys.as_full(&mut *self);
            jit_data.cpu.registers.sp = jit_data.cpu.registers.sp.wrapping_sub(1);
            jit_data.write(jit_data.cpu.registers.sp, (prev_pc >> 8) as u8, RwSource::CPU);
            jit_data.cpu.registers.sp = jit_data.cpu.registers.sp.wrapping_sub(1);
            jit_data.write(jit_data.cpu.registers.sp, prev_pc as u8, RwSource::CPU);
            jit_data.cpu.registers.pc = handler_pc;
            20
        } else {
            let f: Rc<OwnedJittedFunc> = match self.assembled.get(self.registers.pc) {
                Some(f) => {
                    trace!("Found already JIT'ed code for PC {:#06x}", self.registers.pc);
                    Rc::clone(f)
                }
                None => {
                    debug!("Need to assemble JIT code at PC {:#06x}", self.registers.pc);
                    // let bytes = cart.read_slice(self.registers.pc..);
                    // if bytes.is_empty() {
                    //     warn!("Tried to execute code outside of cart range!");
                    //     return 0;
                    // }
                    let pc = self.registers.pc;
                    let jit_data = sys.as_full(&mut *self);
                    let assembler = JitAssembler::new(jit_data, pc);
                    let (jitted, last_pc) = assembler.assemble();
                    self.assembled.insert((self.registers.pc, last_pc), Rc::new(jitted));
                    // debug!("Assembled tree: {:#06x?}", self.assembled);
                    let owned_ref = match self.assembled.get(self.registers.pc) {
                        Some(o) => o,
                        None => panic!("Failed to retrieve just-inserted JIT code from cache at {:#06x}!", self.registers.pc),
                    };
                    Rc::clone(owned_ref)
                }
            };
            trace!("GBC CPU is at {:p}", self as *mut CPU);
            trace!("GBC registers are at {:p}", &self.registers as *const Registers);
            let mut jit_data = sys.as_full(&mut *self);
            let jit_data_ref = &mut jit_data;
            trace!("GBC JIT data is at {:p}", jit_data_ref as *mut SystemRef);
            let fn_ptr = f.as_fn();
            trace!("Calling JIT'ed function at {:p}", fn_ptr as usize as *mut u8);
            // unsafe { std::intrinsics::breakpoint() };
            let result = (fn_ptr)(jit_data_ref);
            trace!("GBC registers after running JIT'ed code:\n{:x?}", self.registers);
            result
        };

        let new_pc = self.registers.pc;
        // assert!(new_pc != prev_pc);
        trace!("PC changed from {:#06x} to {:#06x}", prev_pc, new_pc);

        trace!("Memory map changes: {:?}", self.memory_map_changes);
        for change in self.memory_map_changes.drain(..) {
            match change {
                MMC::InvalidateAll => {
                    warn!("Memory changed, throwing away entire JIT cache");
                    self.assembled = IntervalTree::new();
                }
                MMC::InvalidateRange(range) => {
                    let removed = self.assembled.remove_range(range.clone());
                    if removed > 0 {
                        warn!("Memory changed, threw away {} code fragments in range {:#06x?}", removed, range);
                    }
                }
                MMC::InvalidateOne(addr) => {
                    let removed = self.assembled.remove_one(addr);
                    if removed > 0 {
                        warn!("Memory changed, threw away {} code fragments from addr {:#06x}", removed, addr);
                    }
                }
            }
        }

        let cycles = cycles as u64;
        self.run_dma(cycles);
        self.cycles += cycles;
        self.cycles
    }
}

mod gb_flag_indexes {
    pub(super) const ZERO: u8 = 7;
    pub(super) const SUB: u8 = 6;
    pub(super) const HALF_CARRY: u8 = 5;
    pub(super) const CARRY: u8 = 4;
}

mod x86_flag_indexes {
    pub(super) const HALF_CARRY: u8 = 4;
    pub(super) const ZERO: u8 = 6;
    pub(super) const CARRY: u8 = 0;
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct GBFlags: u8 {
        const ZERO       = 1 << gb_flag_indexes::ZERO;
        const SUB        = 1 << gb_flag_indexes::SUB;
        const HALF_CARRY = 1 << gb_flag_indexes::HALF_CARRY;
        const CARRY      = 1 << gb_flag_indexes::CARRY;
    }

    #[derive(Default)]
    pub struct X86Flags: u8 {
        const ZERO       = 1 << x86_flag_indexes::ZERO;
        const HALF_CARRY = 1 << x86_flag_indexes::HALF_CARRY;
        const CARRY      = 1 << x86_flag_indexes::CARRY;
    }

    #[derive(Default)]
    pub struct InterruptFlags: u8 {
        const VBLANK   = 1 << 0;
        const LCD_STAT = 1 << 1;
        const TIMER    = 1 << 2;
        const SERIAL   = 1 << 3;
        const JOYPAD   = 1 << 4;
    }

    #[derive(Default)]
    pub struct SpeedSwitchFlags: u8 {
        const CURRENT_HIGH = 1 << 7;
        const PREPARE_SWITCH = 1 << 0;
    }
}


#[repr(C)]
#[derive(Clone, Default, Eq, PartialEq)]
pub struct Registers {
    pub a: u8,
    pub f: GBFlags,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
    pub interrupt_enable: InterruptFlags,
    pub interrupt_flags: InterruptFlags,
    pub interrupt_master_enable: u8,
    pub key1: SpeedSwitchFlags,
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Registers")
            .field("a", &format_args!("{:#04x}", self.a))
            .field("f", &self.f)
            .field("b", &format_args!("{:#04x}", self.b))
            .field("c", &format_args!("{:#04x}", self.c))
            .field("d", &format_args!("{:#04x}", self.d))
            .field("e", &format_args!("{:#04x}", self.e))
            .field("h", &format_args!("{:#04x}", self.h))
            .field("l", &format_args!("{:#04x}", self.l))
            .field("sp", &format_args!("{:#06x}", self.sp))
            .field("pc", &format_args!("{:#06x}", self.pc))
            .field("ie", &self.interrupt_enable)
            .field("if", &self.interrupt_flags)
            .field("ime", &self.interrupt_master_enable)
            .finish()
    }
}

impl Registers {
    const OFFSET_A: i32 = 0;
    const OFFSET_F: i32 = 1;
    const OFFSET_B: i32 = 2;
    const OFFSET_C: i32 = 3;
    const OFFSET_D: i32 = 4;
    const OFFSET_E: i32 = 5;
    const OFFSET_H: i32 = 6;
    const OFFSET_L: i32 = 7;
    const OFFSET_SP: i32 = 8;
    const OFFSET_PC: i32 = 10;
    // const OFFSET_IE: i32 = 12;
    // const OFFSET_IF: i32 = 13;
    const OFFSET_IME: i32 = 14;
}

// #[repr(C)]
// #[derive(Debug)]
// pub(crate) struct SystemRef<'a> {
//     pub(crate) cpu: &'a mut CPU,
//     pub(crate) cart: &'a mut Cart,
//     pub(crate) memory: &'a mut Memory,
// }

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Reg {
    B, C, D, E, H, L, IndHL, A,
}

impl TryFrom<u8> for Reg {
    type Error = ();

    fn try_from(x: u8) -> Result<Self, Self::Error> {
        match x {
            0 => Ok(Self::B),
            1 => Ok(Self::C),
            2 => Ok(Self::D),
            3 => Ok(Self::E),
            4 => Ok(Self::H),
            5 => Ok(Self::L),
            6 => Ok(Self::IndHL),
            7 => Ok(Self::A),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::B => f.write_str("B"),
            Self::C => f.write_str("C"),
            Self::D => f.write_str("D"),
            Self::E => f.write_str("E"),
            Self::H => f.write_str("H"),
            Self::L => f.write_str("L"),
            Self::IndHL => f.write_str("(HL)"),
            Self::A => f.write_str("A"),
        }
    }
}

impl Reg {
    #[track_caller]
    fn offset(self) -> i32 {
        match self {
            Self::B => Registers::OFFSET_B,
            Self::C => Registers::OFFSET_C,
            Self::D => Registers::OFFSET_D,
            Self::E => Registers::OFFSET_E,
            Self::H => Registers::OFFSET_H,
            Self::L => Registers::OFFSET_L,
            Self::IndHL => panic!("(HL) \"register\" must be handled separately"),
            Self::A => Registers::OFFSET_A,
        }
    }
}

#[allow(unused)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RegPair {
    BC, DE, HL, SP, AF,
}

impl fmt::Display for RegPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::BC => f.write_str("BC"),
            Self::DE => f.write_str("DE"),
            Self::HL => f.write_str("HL"),
            Self::SP => f.write_str("SP"),
            Self::AF => f.write_str("AF"),
        }
    }
}

impl RegPair {
    fn offset_msb(self) -> i32 {
        match self {
            Self::BC => Registers::OFFSET_B,
            Self::DE => Registers::OFFSET_D,
            Self::HL => Registers::OFFSET_H,
            Self::SP => Registers::OFFSET_SP + 1,
            Self::AF => Registers::OFFSET_A,
        }
    }
    fn offset_lsb(self) -> i32 {
        match self {
            Self::BC => Registers::OFFSET_C,
            Self::DE => Registers::OFFSET_E,
            Self::HL => Registers::OFFSET_L,
            Self::SP => Registers::OFFSET_SP,
            Self::AF => Registers::OFFSET_F,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ConditionCode {
    NotZero, Zero, NoCarry, Carry,
    // ParityOdd, ParityEven, Positive, Negative,
}

impl fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NotZero => f.write_str("nz"),
            Self::Zero => f.write_str("z"),
            Self::NoCarry => f.write_str("nc"),
            Self::Carry => f.write_str("c"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ALUOp {
    ADD, ADC, SUB, SBC, AND, XOR, OR, CP,
}

impl fmt::Display for ALUOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug)]
enum CBOp {
    RLC, RRC, RL, RR, SLA, SRA, SWAP, SRL,
    BIT(u8), RES(u8), SET(u8),
}

impl TryFrom<u8> for CBOp {
    type Error = ();

    fn try_from(x: u8) -> Result<Self, Self::Error> {
        match x {
            0x00 => Ok(Self::RLC),
            0x01 => Ok(Self::RRC),
            0x02 => Ok(Self::RL),
            0x03 => Ok(Self::RR),
            0x04 => Ok(Self::SLA),
            0x05 => Ok(Self::SRA),
            0x06 => Ok(Self::SWAP),
            0x07 => Ok(Self::SRL),
            0x08..=0x0f => Ok(Self::BIT(x - 0x08)),
            0x10..=0x17 => Ok(Self::RES(x - 0x10)),
            0x18..=0x1f => Ok(Self::SET(x - 0x18)),
            0x20.. => Err(())
        }
    }
}

impl fmt::Display for CBOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::RLC => write!(f, "RLC"),
            Self::RRC => write!(f, "RRC"),
            Self::RL => write!(f, "RL"),
            Self::RR => write!(f, "RR"),
            Self::SLA => write!(f, "SLA"),
            Self::SRA => write!(f, "SRA"),
            Self::SWAP => write!(f, "SWAP"),
            Self::SRL => write!(f, "SRL"),
            Self::BIT(n) => write!(f, "BIT {},", n),
            Self::RES(n) => write!(f, "RES {},", n),
            Self::SET(n) => write!(f, "SET {},", n),
        }
    }
}

type JittedFunc = extern "C" fn (&mut SystemRef) -> u32;

#[derive(Debug)]
pub(crate) struct OwnedJittedFunc {
    buffer: dynasmrt::ExecutableBuffer,
    offset: dynasmrt::AssemblyOffset,
    debug_obj_file: Option<debuginfo::DebugObjFile>,
}

impl OwnedJittedFunc {
    fn new(ops: dynasmrt::x64::Assembler, offset: dynasmrt::AssemblyOffset) -> Self {
        Self {
            buffer: ops.finalize().unwrap(),
            offset,
            debug_obj_file: None,
        }
    }

    #[inline]
    fn as_fn(&self) -> JittedFunc {
        let ptr = self.buffer.ptr(self.offset);
        trace!("JittedFn pointer: {:p}", ptr);
        unsafe {
            std::mem::transmute::<*const u8, JittedFunc>(ptr)
        }
    }
}

// Stack vars:
//
// [rbp-0]: *mut SystemRef
// [rbp-8]: *mut u8 -- read return slot
// [rbp-0x18]: u8 -- extra cycles
// [rbp-0x19]: bool(u8) -- whether PC was overwritten
//
// Registers:
//
// rax: scratch
// rdi: *mut SystemRef
// rsi: ???
// rdx: ???
// rcx: ???
// r8:  ???
// r9:  ???

macro_rules! dynasm {
    ($ops:expr => $($t:tt)*) => {
        ::dynasmrt::dynasm!($ops
            ; .arch x64
            ; .alias scratch, rax
            ; .alias reg_ptr, rdi
            ; .alias cart_ptr, rsi
            ; .alias mem_ptr, rsi
            $($t)*
        );
    };
}

macro_rules! jit_gb_regs_to_rdi {
    ($assembler:expr) => {
        dynasm!($assembler.ops =>
            ; mov rdi, [rbp]
            ; mov rdi, QWORD [rdi + memoffset::offset_of!(SystemRef, cpu) as _]
        );
    }
}

macro_rules! jit_enter {
    ($assembler:expr) => {
        dynasm!($assembler.ops =>
            // ; int3
            ; push rbp
            ; mov rbp, rsp
            ; sub rsp, JitAssembler::STACK_SIZE_BYTES as _
            ; mov [rbp], rdi
            ; mov BYTE [rbp - 0x18], 0
            // ; mov BYTE [rbp - 0x19], 0
            ; mov rdi, QWORD [rdi + memoffset::offset_of!(SystemRef, cpu) as _]
        );
    }
}

macro_rules! jit_set_pc {
    ($assembler:expr, 0, $($target:tt)*) => {{
        dynasm!($assembler.ops =>
            ; mov WORD [reg_ptr + Registers::OFFSET_PC], $($target)*
            // ; mov BYTE [rbp - 0x19], 1
        );
    }};
    ($assembler:expr, $extra_cycles:expr, $($target:tt)*) => {{
        dynasm!($assembler.ops =>
            ; mov WORD [reg_ptr + Registers::OFFSET_PC], $($target)*
            ; add BYTE [rbp - 0x18], $extra_cycles
            // ; mov BYTE [rbp - 0x19], 1
        );
    }};
}

macro_rules! jit_return {
    ($assembler:expr, ok) => {
        // call_jit_helper!($assembler, jit_helper_debug_registers);
        dynasm!($assembler.ops =>
            ; xor ecx, ecx
            ; mov eax, $assembler.cycles as i32
            ; mov cl, BYTE [rbp - 0x18]
            ; add eax, ecx
            // ; cmp BYTE [rbp - 0x19], 0
            // ; jne >ret_pc_set
            // ; mov WORD [reg_ptr + Registers::OFFSET_PC], $assembler.current_pc as i16
            // ; ret_pc_set:
            ; add rsp, JitAssembler::STACK_SIZE_BYTES as _
            ; pop rbp
            ; ret
        );
    };
    ($assembler:expr, fail) => {
        jit_return!($assembler, ok)
    };
}

macro_rules! jit_check_cc {
    ($assembler:expr, $cc:expr, $action:block) => {{
        dynasm!($assembler.ops =>
            ;; match $cc {
                ConditionCode::NotZero => jit_check_cc!(@check $assembler, ZERO, do when unset),
                ConditionCode::Zero => jit_check_cc!(@check $assembler, ZERO, do when set),
                ConditionCode::NoCarry => jit_check_cc!(@check $assembler, CARRY, do when unset),
                ConditionCode::Carry => jit_check_cc!(@check $assembler, CARRY, do when set),
            }
            ; mov al, BYTE [reg_ptr + Registers::OFFSET_F]
            ;; { $action; }
            ; cc_check_done:
        );
    }};
    (@check $assembler:expr, $flag:ident, do when unset) => {{
        dynasm!($assembler.ops =>
            ; mov al, BYTE [reg_ptr + Registers::OFFSET_F]
            ; and al, BYTE GBFlags::$flag.bits() as i8
            ; jnz >cc_check_done
        );
    }};
    (@check $assembler:expr, $flag:ident, do when set) => {{
        dynasm!($assembler.ops =>
            ; mov al, BYTE [reg_ptr + Registers::OFFSET_F]
            ; and al, BYTE GBFlags::$flag.bits() as i8
            ; jz >cc_check_done
        );
    }};
}

#[cfg(test)]
macro_rules! jit_debugger {
    ($assembler:expr) => {};
}

#[cfg(not(test))]
macro_rules! jit_debugger {
    ($assembler:expr) => {
        dynasm!($assembler.ops =>
            ; int3
        );
    };
}

macro_rules! call_jit_helper {
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, BYTE cl) => {
        trace!("JIT helper call arg 3 = {}", stringify!(cl));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, BYTE $c:tt) => {
        trace!("JIT helper call arg 3 = {}", stringify!($c));
        dynasm!($assembler.ops =>
            ; mov cl, $c
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, WORD cx) => {
        trace!("JIT helper call arg 3 = {}", stringify!(cx));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, WORD $c:tt) => {
        trace!("JIT helper call arg 3 = {}", stringify!($c));
        dynasm!($assembler.ops =>
            ; mov cx, $c
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, QWORD rcx) => {
        trace!("JIT helper call arg 3 = {}", stringify!(rcx));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, $b_ty:ident $b:tt, QWORD $c:tt) => {
        trace!("JIT helper call arg 3 = {}", stringify!($c));
        dynasm!($assembler.ops =>
            ; mov rcx, $c
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a, $b_ty $b)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, BYTE dl) => {
        trace!("JIT helper call arg 2 = {}", stringify!(dl));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, BYTE $b:tt) => {
        trace!("JIT helper call arg 2 = {}", stringify!($b));
        dynasm!($assembler.ops =>
            ; mov dl, $b
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, WORD dx) => {
        trace!("JIT helper call arg 2 = {}", stringify!(dx));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, WORD $b:tt) => {
        trace!("JIT helper call arg 2 = {}", stringify!($b));
        dynasm!($assembler.ops =>
            ; mov dx, $b
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, QWORD rdx) => {
        trace!("JIT helper call arg 2 = {}", stringify!(rdx));
        call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, $a_ty:ident $a:tt, QWORD $b:tt) => {
        trace!("JIT helper call arg 2 = {}", stringify!($b));
        dynasm!($assembler.ops =>
            ; mov rdx, $b
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr, $a_ty $a)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, BYTE sil) => {
        trace!("JIT helper call arg 1 = {}", stringify!(sil));
        call_jit_helper!($(@ $ignore)* $assembler, $addr);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, BYTE $a:tt) => {
        trace!("JIT helper call arg 1 = {}", stringify!($a));
        dynasm!($assembler.ops =>
            ; mov sil, $a
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, WORD si) => {
        trace!("JIT helper call arg 1 = {}", stringify!(si));
        call_jit_helper!($(@ $ignore)* $assembler, $addr);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, WORD $a:tt) => {
        trace!("JIT helper call arg 1 = {}", stringify!($a));
        dynasm!($assembler.ops =>
            ; mov si, $a
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr)
        );
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, QWORD rsi) => {
        trace!("JIT helper call arg 1 = {}", stringify!(rsi));
        call_jit_helper!($(@ $ignore)* $assembler, $addr);
    };
    ($(@ $ignore:ident)* $assembler:expr, $addr:expr, QWORD $a:tt) => {
        trace!("JIT helper call arg 1 = {}", stringify!($a));
        dynasm!($assembler.ops =>
            ; mov rsi, $a
            ;; call_jit_helper!($(@ $ignore)* $assembler, $addr)
        );
    };
    (@ignore $assembler:expr, $addr:expr) => {
        debug!("Adding call in JIT code to {}", stringify!($addr));
        dynasm!($assembler.ops =>
            ; mov rdi, [rbp]
            ; mov rax, QWORD $addr as _
            // ; int3
            ; call rax
            ;; jit_gb_regs_to_rdi!($assembler)
        );
    };
    ($assembler:expr, $addr:expr) => {
        debug!("Adding call in JIT code to {}", stringify!($addr));
        dynasm!($assembler.ops =>
            ; mov rdi, [rbp]
            ; mov rax, QWORD $addr as _
            // ; int3
            ; call rax
            ;; jit_gb_regs_to_rdi!($assembler)
            // ; or rax, rax
            // ; jz >passed
            // ;; jit_return!($assembler, ok)
            // ; passed:
        );
    };
}

macro_rules! jit_set_x86_flags_from_gb {
    ($assembler:expr,) => {};
    ($assembler:expr $(, $flag:ident)+) => {
        dynasm!($assembler.ops =>
            ; mov al, BYTE [reg_ptr + Registers::OFFSET_F]
            ; xor ah, ah
        );
        jit_set_x86_flags_from_gb!(@inner $assembler $(, $flag)+);
        dynasm!($assembler.ops =>
            ; sahf
        );
    };
    (@inner $assembler:expr, zero) => {
        jit_set_x86_flags_from_gb!(@single $assembler, al, ZERO);
    };
    (@inner $assembler:expr, zero $(, $flags:ident)+) => {
        jit_set_x86_flags_from_gb!(@single $assembler, dl, ZERO);
        jit_set_x86_flags_from_gb!(@inner $assembler $(, $flags)+);
    };
    (@inner $assembler:expr, halfcarry) => {
        jit_set_x86_flags_from_gb!(@single $assembler, al, HALF_CARRY);
    };
    (@inner $assembler:expr, halfcarry $(, $flags:ident)+) => {
        jit_set_x86_flags_from_gb!(@single $assembler, dl, HALF_CARRY);
        jit_set_x86_flags_from_gb!(@inner $assembler $(, $flags)+);
    };
    (@inner $assembler:expr, carry) => {
        jit_set_x86_flags_from_gb!(@single $assembler, al, CARRY);
    };
    (@inner $assembler:expr, carry $(, $flags:ident)+) => {
        jit_set_x86_flags_from_gb!(@single $assembler, dl, CARRY);
        jit_set_x86_flags_from_gb!(@inner $assembler $(, $flags)+);
    };
    (@single $assembler:expr, al, $flag:ident) => {
        dynasm!($assembler.ops =>
            ; and al, GBFlags::ZERO.bits() as _
            ; shr al, gb_flag_indexes::$flag.checked_sub(x86_flag_indexes::$flag).unwrap() as _
            ; or ah, al
        );
    };
    (@single $assembler:expr, $reg:tt, $flag:ident) => {
        dynasm!($assembler.ops =>
            ; mov $reg, al
            ; and $reg, GBFlags::ZERO.bits() as _
            ; shr $reg, gb_flag_indexes::$flag.checked_sub(x86_flag_indexes::$flag).unwrap() as _
            ; or ah, $reg
        );
    };
}

/// Sets the four flag bits supported on the GBC CPU.
///
/// * Bit 7: zero flag - set if the result of the operation is zero
///
/// * Bit 6: subtraction flag - set if the operation was a subtraction
///
/// * Bit 5: half-carry flag - set if the operation resulted in bit 4 carrying to bit 5 (or vise versa)
///
/// * Bit 4: carry flag - TODO
macro_rules! jit_set_flags {
    ($assembler:expr ;) => {};
    ($assembler:expr ; $($rest:tt)+) => {
        dynasm!($assembler.ops =>
            ; mov al, BYTE [reg_ptr + Registers::OFFSET_F]
            // ; int3
            ; lahf
            // ; mov [rbp - 0x20], ax
            // ;; call_jit_helper!($assembler, jit_helper_flags_debug, BYTE al, BYTE ah)
            // ; mov ax, [rbp - 0x20]
            ;; jit_set_flags!(@inner $assembler ; $($rest)*)
            // ; mov [rbp - 0x20], ax
            // ;; call_jit_helper!($assembler, jit_helper_flags_debug, BYTE al, BYTE ah)
            // ; mov ax, [rbp - 0x20]
            ; mov BYTE [reg_ptr + Registers::OFFSET_F], al
        );
    };
    (@inner $assembler:expr ;) => { () };
    // Zero flag
    (@inner $assembler:expr ; zero ;) => {
        jit_set_flags!(@check_mask $assembler ; ZERO last);
    };
    (@inner $assembler:expr ; zero ; $($rest:tt)+) => {
        jit_set_flags!(@check_mask $assembler ; ZERO);
        jit_set_flags!(@inner $assembler ; $($rest)+);
    };
    (@inner $assembler:expr ; zero = 1 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; or al, BYTE GBFlags::ZERO.bits() as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    (@inner $assembler:expr ; zero = 0 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; and al, BYTE (!GBFlags::ZERO.bits()) as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    // Subtraction flag
    (@inner $assembler:expr ; sub = 1 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; or al, BYTE GBFlags::SUB.bits() as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    (@inner $assembler:expr ; sub = 0 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; and al, BYTE (!GBFlags::SUB.bits()) as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    // Half-carry flag
    (@inner $assembler:expr ; halfcarry ;) => {
        jit_set_flags!(@check_mask $assembler ; HALF_CARRY last);
    };
    (@inner $assembler:expr ; halfcarry ; $($rest:tt)+) => {
        jit_set_flags!(@check_mask $assembler ; HALF_CARRY);
        jit_set_flags!(@inner $assembler ; $($rest)+);
    };
    (@inner $assembler:expr ; halfcarry = 1 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; or al, BYTE GBFlags::HALF_CARRY.bits() as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    (@inner $assembler:expr ; halfcarry = 0 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; and al, BYTE (!GBFlags::HALF_CARRY.bits()) as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    // Full carry flag
    (@inner $assembler:expr ; carry ;) => {
        jit_set_flags!(@check_mask $assembler ; CARRY last);
    };
    (@inner $assembler:expr ; carry ; $($rest:tt)+) => {
        jit_set_flags!(@check_mask $assembler ; CARRY);
        jit_set_flags!(@inner $assembler ; $($rest)+);
    };
    (@inner $assembler:expr ; carry = 1 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; or al, BYTE GBFlags::CARRY.bits() as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    (@inner $assembler:expr ; carry = 0 ; $($rest:tt)*) => {
        dynasm!($assembler.ops =>
            ; and al, BYTE (!GBFlags::CARRY.bits()) as _
        );
        jit_set_flags!(@inner $assembler ; $($rest)*);
    };
    (@check_mask $assembler:expr ; $bit:ident last) => {
        dynasm!($assembler.ops =>
            ; and ah, X86Flags::$bit.bits() as _
            ; and al, (!GBFlags::$bit.bits()) as _
        );
        let gb_index = gb_flag_indexes::$bit;
        let x86_index = x86_flag_indexes::$bit;
        if x86_index < gb_index {
            dynasm!($assembler.ops =>
                ; shl ah, (gb_index - x86_index) as _
            );
        } else if x86_index > gb_index {
            dynasm!($assembler.ops =>
                ; shl ah, (x86_index - gb_index) as _
            );
        }
        dynasm!($assembler.ops =>
            ; or al, ah
        );
    };
    (@check_mask $assembler:expr ; $bit:ident) => {
        dynasm!($assembler.ops =>
            ; mov dl, ah
            ; and dl, X86Flags::$bit.bits() as _
            ; and al, (!GBFlags::$bit.bits()) as _
        );
        let gb_index = gb_flag_indexes::$bit;
        let x86_index = x86_flag_indexes::$bit;
        if x86_index < gb_index {
            dynasm!($assembler.ops =>
                ; shl dl, gb_index.checked_sub(x86_index).unwrap() as _
            );
        } else if x86_index > gb_index {
            dynasm!($assembler.ops =>
                ; shl dl, x86_index.checked_sub(gb_index).unwrap() as _
            );
        }
        dynasm!($assembler.ops =>
            ; or al, dl
        );
    };
    // (@shift $assembler:expr ; $reg:tt, $current:expr, $target:expr) => {
    //     // The use of .wrapping_sub here is only to convince the compiler that
    //     // the subtraction is fine. We don't do subtractions where this could
    //     // overflow anyways.
    //     if $current < $target {
    //         dynasm!($assembler.ops =>
    //             ; shl $reg, (($target as u8).wrapping_sub($current)) as _
    //         );
    //     } else if $current > $target {
    //         dynasm!($assembler.ops =>
    //             ; shr $reg, (($current as u8).wrapping_sub($target)) as _
    //         );
    //     }
    // };
}

struct JitAssembler<'a> {
    cycles: u32,
    start_pc: u16,
    previous_pc: u16,
    current_pc: u16,
    found_jump: bool,
    debug_info: debuginfo::JitAssemblerDebugInfo,
    ops: dynasmrt::x64::Assembler,
    jit_data: SystemRef<'a>,
    instr_bytes: [u8; 3],
}

impl<'a> JitAssembler<'a> {
    const STACK_SIZE_BYTES: u8 = 64;

    fn new(jit_data: SystemRef<'a>, pc: u16) -> Self {
        Self {
            cycles: 0,
            start_pc: pc,
            previous_pc: pc,
            current_pc: pc,
            found_jump: false,
            debug_info: debuginfo::JitAssemblerDebugInfo::new(pc),
            ops: dynasmrt::x64::Assembler::new().unwrap(),
            jit_data,
            instr_bytes: [0; 3],
        }
    }

    #[inline]
    fn read_next_byte(&mut self) -> Option<u8> {
        let byte = self.jit_data.read(self.current_pc, RwSource::CPU)?;
        self.current_pc = self.current_pc.wrapping_add(1);
        Some(byte)
    }

    #[inline]
    fn get_next_op_byte(&mut self) -> Option<u8> {
        self.previous_pc = self.current_pc;
        let byte = self.read_next_byte()?;
        self.instr_bytes[0] = byte;
        Some(byte)
    }

    #[inline]
    fn get_imm_one_byte(&mut self) -> Option<u8> {
        let byte = self.read_next_byte()?;
        self.instr_bytes[1] = byte;
        Some(byte)
    }

    #[inline]
    fn get_imm_one_byte_signed(&mut self) -> Option<i8> {
        let byte = self.read_next_byte()?;
        self.instr_bytes[1] = byte;
        Some(byte as i8)
    }

    #[inline]
    fn get_imm_two_bytes(&mut self) -> Option<u16> {
        let low_byte = self.read_next_byte()?;
        let high_byte = self.read_next_byte()?;
        self.instr_bytes[1] = low_byte;
        self.instr_bytes[2] = high_byte;
        Some((high_byte as u16) << 8 | (low_byte as u16))
    }

    // #[inline]
    fn advance(&mut self, jump_instr: bool, size: u8, cycles: u8, label: impl std::fmt::Display) {
        debug_assert!(cycles >= 4, "Bad cycle count when JIT'ing {}", label);
        debug_assert!(size >= 1 && size <= 3, "Bad size when JIT'ing {}", label);
        debug_assert_eq!(
            size as u16,
            self.current_pc - self.previous_pc,
            "Incorrect size given when JIT'ing {}",
            label,
        );
        let new_offset = self.ops.offset();
        // let last_offset = self.debug_info.last_offset();
        self.debug_info.add_line(
            self.previous_pc,
            new_offset,
            label,
            self.instr_bytes[0],
            if size > 1 { Some(self.instr_bytes[1]) } else { None },
            if size > 2 { Some(self.instr_bytes[2]) } else { None },
        );
        // if Some(new_offset) == last_offset {
        //     panic!("Instructions must call advance *before* writing any assembly");
        // }
        self.found_jump |= jump_instr;
        self.cycles += cycles as u32;
        jit_set_pc!(self, 0, self.current_pc as i16);
        // self.current_pc_pre_instr = self.current_pc_post_instr;
        // self.current_pc_post_instr += size;
        // self.slice = &self.slice[size as usize..];
    }

    fn assemble(mut self) -> (OwnedJittedFunc, u16) {
        debug!("Assembling JIT code starting at {:#06x}", self.start_pc);

        let start_offset = self.ops.offset();
        jit_enter!(self);

        while !self.found_jump {
            #[cfg(test)]
            call_jit_helper!(self, jit_helper_debug_registers);
            // debug!("Ops before opcode {:#04x}: {:x?}", self.slice[0], self.ops);
            let cycles_before = self.cycles;
            let pc_before = self.current_pc;
            let byte = match self.get_next_op_byte() {
                Some(b) => b,
                None => break,
            };
            debug!("JIT'ing opcode {:#04x} from PC {:#06x}", byte, self.current_pc.wrapping_sub(1));
            match byte {
                0x00 => self.assemble_nop(),

                0x01 => self.assemble_ld_rp_nn(RegPair::BC),
                0x11 => self.assemble_ld_rp_nn(RegPair::DE),
                0x21 => self.assemble_ld_rp_nn(RegPair::HL),
                0x31 => self.assemble_ld_rp_nn(RegPair::SP),

                0x02 => self.assemble_ld_indrp_a(RegPair::BC),
                0x12 => self.assemble_ld_indrp_a(RegPair::DE),

                0x03 => self.assemble_inc_rp(RegPair::BC),
                0x13 => self.assemble_inc_rp(RegPair::DE),
                0x23 => self.assemble_inc_rp(RegPair::HL),
                0x33 => self.assemble_inc_rp(RegPair::SP),

                0x04 => self.assemble_inc_r(Reg::B),
                0x0c => self.assemble_inc_r(Reg::C),
                0x14 => self.assemble_inc_r(Reg::D),
                0x1c => self.assemble_inc_r(Reg::E),
                0x24 => self.assemble_inc_r(Reg::H),
                0x2c => self.assemble_inc_r(Reg::L),
                0x34 => self.assemble_inc_r(Reg::IndHL),
                0x3c => self.assemble_inc_r(Reg::A),

                0x05 => self.assemble_dec_r(Reg::B),
                0x0d => self.assemble_dec_r(Reg::C),
                0x15 => self.assemble_dec_r(Reg::D),
                0x1d => self.assemble_dec_r(Reg::E),
                0x25 => self.assemble_dec_r(Reg::H),
                0x2d => self.assemble_dec_r(Reg::L),
                0x35 => self.assemble_dec_r(Reg::IndHL),
                0x3d => self.assemble_dec_r(Reg::A),

                0x06 => self.assemble_ld_r_n(Reg::B),
                0x0e => self.assemble_ld_r_n(Reg::C),
                0x16 => self.assemble_ld_r_n(Reg::D),
                0x1e => self.assemble_ld_r_n(Reg::E),
                0x26 => self.assemble_ld_r_n(Reg::H),
                0x2e => self.assemble_ld_r_n(Reg::L),
                0x36 => self.assemble_ld_r_n(Reg::IndHL),
                0x3e => self.assemble_ld_r_n(Reg::A),

                0x07 => self.assemble_rlca(),
                0x08 => self.assemble_ld_indnn_sp(),

                0x09 => self.assemble_add_hl_rp(RegPair::BC),
                0x19 => self.assemble_add_hl_rp(RegPair::DE),
                0x29 => self.assemble_add_hl_rp(RegPair::HL),
                0x39 => self.assemble_add_hl_rp(RegPair::SP),

                0x0a => self.assemble_ld_a_indrp(RegPair::BC),
                0x1a => self.assemble_ld_a_indrp(RegPair::DE),

                0x0b => self.assemble_dec_rp(RegPair::BC),
                0x1b => self.assemble_dec_rp(RegPair::DE),
                0x2b => self.assemble_dec_rp(RegPair::HL),
                0x3b => self.assemble_dec_rp(RegPair::SP),

                0x0f => self.assemble_rrca(),

                0x10 => self.assemble_stop(),

                0x17 => self.assemble_rla(),
                0x18 => self.assemble_jr_n(),
                0x1f => self.assemble_rra(),

                0x20 => self.assemble_jr_cc_n(ConditionCode::NotZero),
                0x28 => self.assemble_jr_cc_n(ConditionCode::Zero),
                0x30 => self.assemble_jr_cc_n(ConditionCode::NoCarry),
                0x38 => self.assemble_jr_cc_n(ConditionCode::Carry),

                0x22 => self.assemble_ld_indhlindex_a::<true>(),
                0x2a => self.assemble_ld_a_indhlindex::<true>(),
                0x32 => self.assemble_ld_indhlindex_a::<false>(),
                0x3a => self.assemble_ld_a_indhlindex::<false>(),

                0x27 => self.assemble_daa(),
                0x2f => self.assemble_cpl(),
                0x37 => self.assemble_scf(),
                0x3f => self.assemble_ccf(),

                0x40 => self.assemble_ld_r_r(Reg::B, Reg::B),
                0x41 => self.assemble_ld_r_r(Reg::B, Reg::C),
                0x42 => self.assemble_ld_r_r(Reg::B, Reg::D),
                0x43 => self.assemble_ld_r_r(Reg::B, Reg::E),
                0x44 => self.assemble_ld_r_r(Reg::B, Reg::H),
                0x45 => self.assemble_ld_r_r(Reg::B, Reg::L),
                0x46 => self.assemble_ld_r_r(Reg::B, Reg::IndHL),
                0x47 => self.assemble_ld_r_r(Reg::B, Reg::A),
                0x48 => self.assemble_ld_r_r(Reg::C, Reg::B),
                0x49 => self.assemble_ld_r_r(Reg::C, Reg::C),
                0x4a => self.assemble_ld_r_r(Reg::C, Reg::D),
                0x4b => self.assemble_ld_r_r(Reg::C, Reg::E),
                0x4c => self.assemble_ld_r_r(Reg::C, Reg::H),
                0x4d => self.assemble_ld_r_r(Reg::C, Reg::L),
                0x4e => self.assemble_ld_r_r(Reg::C, Reg::IndHL),
                0x4f => self.assemble_ld_r_r(Reg::C, Reg::A),

                0x50 => self.assemble_ld_r_r(Reg::D, Reg::B),
                0x51 => self.assemble_ld_r_r(Reg::D, Reg::C),
                0x52 => self.assemble_ld_r_r(Reg::D, Reg::D),
                0x53 => self.assemble_ld_r_r(Reg::D, Reg::E),
                0x54 => self.assemble_ld_r_r(Reg::D, Reg::H),
                0x55 => self.assemble_ld_r_r(Reg::D, Reg::L),
                0x56 => self.assemble_ld_r_r(Reg::D, Reg::IndHL),
                0x57 => self.assemble_ld_r_r(Reg::D, Reg::A),
                0x58 => self.assemble_ld_r_r(Reg::E, Reg::B),
                0x59 => self.assemble_ld_r_r(Reg::E, Reg::C),
                0x5a => self.assemble_ld_r_r(Reg::E, Reg::D),
                0x5b => self.assemble_ld_r_r(Reg::E, Reg::E),
                0x5c => self.assemble_ld_r_r(Reg::E, Reg::H),
                0x5d => self.assemble_ld_r_r(Reg::E, Reg::L),
                0x5e => self.assemble_ld_r_r(Reg::E, Reg::IndHL),
                0x5f => self.assemble_ld_r_r(Reg::E, Reg::A),

                0x60 => self.assemble_ld_r_r(Reg::H, Reg::B),
                0x61 => self.assemble_ld_r_r(Reg::H, Reg::C),
                0x62 => self.assemble_ld_r_r(Reg::H, Reg::D),
                0x63 => self.assemble_ld_r_r(Reg::H, Reg::E),
                0x64 => self.assemble_ld_r_r(Reg::H, Reg::H),
                0x65 => self.assemble_ld_r_r(Reg::H, Reg::L),
                0x66 => self.assemble_ld_r_r(Reg::H, Reg::IndHL),
                0x67 => self.assemble_ld_r_r(Reg::H, Reg::A),
                0x68 => self.assemble_ld_r_r(Reg::L, Reg::B),
                0x69 => self.assemble_ld_r_r(Reg::L, Reg::C),
                0x6a => self.assemble_ld_r_r(Reg::L, Reg::D),
                0x6b => self.assemble_ld_r_r(Reg::L, Reg::E),
                0x6c => self.assemble_ld_r_r(Reg::L, Reg::H),
                0x6d => self.assemble_ld_r_r(Reg::L, Reg::L),
                0x6e => self.assemble_ld_r_r(Reg::L, Reg::IndHL),
                0x6f => self.assemble_ld_r_r(Reg::L, Reg::A),

                0x70 => self.assemble_ld_r_r(Reg::IndHL, Reg::B),
                0x71 => self.assemble_ld_r_r(Reg::IndHL, Reg::C),
                0x72 => self.assemble_ld_r_r(Reg::IndHL, Reg::D),
                0x73 => self.assemble_ld_r_r(Reg::IndHL, Reg::E),
                0x74 => self.assemble_ld_r_r(Reg::IndHL, Reg::H),
                0x75 => self.assemble_ld_r_r(Reg::IndHL, Reg::L),
                0x76 => self.assemble_halt(),
                0x77 => self.assemble_ld_r_r(Reg::IndHL, Reg::A),
                0x78 => self.assemble_ld_r_r(Reg::A, Reg::B),
                0x79 => self.assemble_ld_r_r(Reg::A, Reg::C),
                0x7a => self.assemble_ld_r_r(Reg::A, Reg::D),
                0x7b => self.assemble_ld_r_r(Reg::A, Reg::E),
                0x7c => self.assemble_ld_r_r(Reg::A, Reg::H),
                0x7d => self.assemble_ld_r_r(Reg::A, Reg::L),
                0x7e => self.assemble_ld_r_r(Reg::A, Reg::IndHL),
                0x7f => self.assemble_ld_r_r(Reg::A, Reg::A),

                0x80 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::B),
                0x81 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::C),
                0x82 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::D),
                0x83 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::E),
                0x84 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::H),
                0x85 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::L),
                0x86 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::IndHL),
                0x87 => self.assemble_aluop_a_r(ALUOp::ADD, Reg::A),

                0x88 => self.assemble_aluop_a_r(ALUOp::ADC, Reg::B),
                0x89 => self.assemble_aluop_a_r(ALUOp::ADC, Reg::C),
                0x8a => self.assemble_aluop_a_r(ALUOp::ADC, Reg::D),
                0x8b => self.assemble_aluop_a_r(ALUOp::ADC, Reg::E),
                0x8c => self.assemble_aluop_a_r(ALUOp::ADC, Reg::H),
                0x8d => self.assemble_aluop_a_r(ALUOp::ADC, Reg::L),
                0x8e => self.assemble_aluop_a_r(ALUOp::ADC, Reg::IndHL),
                0x8f => self.assemble_aluop_a_r(ALUOp::ADC, Reg::A),

                0x90 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::B),
                0x91 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::C),
                0x92 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::D),
                0x93 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::E),
                0x94 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::H),
                0x95 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::L),
                0x96 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::IndHL),
                0x97 => self.assemble_aluop_a_r(ALUOp::SUB, Reg::A),

                0x98 => self.assemble_aluop_a_r(ALUOp::SBC, Reg::B),
                0x99 => self.assemble_aluop_a_r(ALUOp::SBC, Reg::C),
                0x9a => self.assemble_aluop_a_r(ALUOp::SBC, Reg::D),
                0x9b => self.assemble_aluop_a_r(ALUOp::SBC, Reg::E),
                0x9c => self.assemble_aluop_a_r(ALUOp::SBC, Reg::H),
                0x9d => self.assemble_aluop_a_r(ALUOp::SBC, Reg::L),
                0x9e => self.assemble_aluop_a_r(ALUOp::SBC, Reg::IndHL),
                0x9f => self.assemble_aluop_a_r(ALUOp::SBC, Reg::A),

                0xa0 => self.assemble_aluop_a_r(ALUOp::AND, Reg::B),
                0xa1 => self.assemble_aluop_a_r(ALUOp::AND, Reg::C),
                0xa2 => self.assemble_aluop_a_r(ALUOp::AND, Reg::D),
                0xa3 => self.assemble_aluop_a_r(ALUOp::AND, Reg::E),
                0xa4 => self.assemble_aluop_a_r(ALUOp::AND, Reg::H),
                0xa5 => self.assemble_aluop_a_r(ALUOp::AND, Reg::L),
                0xa6 => self.assemble_aluop_a_r(ALUOp::AND, Reg::IndHL),
                0xa7 => self.assemble_aluop_a_r(ALUOp::AND, Reg::A),

                0xa8 => self.assemble_aluop_a_r(ALUOp::XOR, Reg::B),
                0xa9 => self.assemble_aluop_a_r(ALUOp::XOR, Reg::C),
                0xaa => self.assemble_aluop_a_r(ALUOp::XOR, Reg::D),
                0xab => self.assemble_aluop_a_r(ALUOp::XOR, Reg::E),
                0xac => self.assemble_aluop_a_r(ALUOp::XOR, Reg::H),
                0xad => self.assemble_aluop_a_r(ALUOp::XOR, Reg::L),
                0xae => self.assemble_aluop_a_r(ALUOp::XOR, Reg::IndHL),
                0xaf => self.assemble_aluop_a_r(ALUOp::XOR, Reg::A),

                0xb0 => self.assemble_aluop_a_r(ALUOp::OR, Reg::B),
                0xb1 => self.assemble_aluop_a_r(ALUOp::OR, Reg::C),
                0xb2 => self.assemble_aluop_a_r(ALUOp::OR, Reg::D),
                0xb3 => self.assemble_aluop_a_r(ALUOp::OR, Reg::E),
                0xb4 => self.assemble_aluop_a_r(ALUOp::OR, Reg::H),
                0xb5 => self.assemble_aluop_a_r(ALUOp::OR, Reg::L),
                0xb6 => self.assemble_aluop_a_r(ALUOp::OR, Reg::IndHL),
                0xb7 => self.assemble_aluop_a_r(ALUOp::OR, Reg::A),

                0xb8 => self.assemble_aluop_a_r(ALUOp::CP, Reg::B),
                0xb9 => self.assemble_aluop_a_r(ALUOp::CP, Reg::C),
                0xba => self.assemble_aluop_a_r(ALUOp::CP, Reg::D),
                0xbb => self.assemble_aluop_a_r(ALUOp::CP, Reg::E),
                0xbc => self.assemble_aluop_a_r(ALUOp::CP, Reg::H),
                0xbd => self.assemble_aluop_a_r(ALUOp::CP, Reg::L),
                0xbe => self.assemble_aluop_a_r(ALUOp::CP, Reg::IndHL),
                0xbf => self.assemble_aluop_a_r(ALUOp::CP, Reg::A),

                0xc0 => self.assemble_ret_cc(ConditionCode::NotZero),
                0xc8 => self.assemble_ret_cc(ConditionCode::Zero),
                0xd0 => self.assemble_ret_cc(ConditionCode::NoCarry),
                0xd8 => self.assemble_ret_cc(ConditionCode::Carry),

                0xc1 => self.assemble_pop_rp(RegPair::BC),
                0xd1 => self.assemble_pop_rp(RegPair::DE),
                0xe1 => self.assemble_pop_rp(RegPair::HL),
                0xf1 => self.assemble_pop_rp(RegPair::AF),

                0xc2 => self.assemble_jp_cc_nn(ConditionCode::NotZero),
                0xca => self.assemble_jp_cc_nn(ConditionCode::Zero),
                0xd2 => self.assemble_jp_cc_nn(ConditionCode::NoCarry),
                0xda => self.assemble_jp_cc_nn(ConditionCode::Carry),

                0xc3 => self.assemble_jp_nn(),

                0xc4 => self.assemble_call_cc_nn(ConditionCode::NotZero),
                0xcc => self.assemble_call_cc_nn(ConditionCode::Zero),
                0xd4 => self.assemble_call_cc_nn(ConditionCode::NoCarry),
                0xdc => self.assemble_call_cc_nn(ConditionCode::Carry),

                0xc5 => self.assemble_push_rp(RegPair::BC),
                0xd5 => self.assemble_push_rp(RegPair::DE),
                0xe5 => self.assemble_push_rp(RegPair::HL),
                0xf5 => self.assemble_push_rp(RegPair::AF),

                0xc6 => self.assemble_aluop_a_n(ALUOp::ADD),
                0xce => self.assemble_aluop_a_n(ALUOp::ADC),
                0xd6 => self.assemble_aluop_a_n(ALUOp::SUB),
                0xde => self.assemble_aluop_a_n(ALUOp::SBC),
                0xe6 => self.assemble_aluop_a_n(ALUOp::AND),
                0xee => self.assemble_aluop_a_n(ALUOp::XOR),
                0xf6 => self.assemble_aluop_a_n(ALUOp::OR),
                0xfe => self.assemble_aluop_a_n(ALUOp::CP),

                0xc7 => self.assemble_rst(0x00),
                0xcf => self.assemble_rst(0x08),
                0xd7 => self.assemble_rst(0x10),
                0xdf => self.assemble_rst(0x18),
                0xe7 => self.assemble_rst(0x20),
                0xef => self.assemble_rst(0x28),
                0xf7 => self.assemble_rst(0x30),
                0xff => self.assemble_rst(0x38),

                0xc9 => self.assemble_ret(),
                0xcb => self.assemble_cb_instr(),
                0xcd => self.assemble_call_nn(),

                0xe0 => self.assemble_ldh_indn_a(),
                0xe2 => self.assemble_ldh_indc_a(),
                0xe8 => self.assemble_add_sp_d(),
                0xe9 => self.assemble_jp_hl(),
                0xea => self.assemble_ld_indnn_a(),
                0xf0 => self.assemble_ldh_a_indn(),
                0xf2 => self.assemble_ldh_a_indc(),
                0xf3 => self.assemble_di(),
                0xf8 => self.assemble_ld_hl_sp_n(),
                0xf9 => self.assemble_ld_sp_hl(),
                0xfa => self.assemble_ld_a_indnn(),
                0xfb => self.assemble_ei(),

                opcode => todo!("Don't know how to assemble opcode {:#04x}", opcode),
            }
            debug_assert!(
                self.current_pc != pc_before,
                "PC didn't change! Before was {:#06x}, after is {:#06x}",
                pc_before, self.current_pc,
            );
            debug_assert!(self.cycles != cycles_before);
        }
        self.debug_info.set_epilogue_offset(self.ops.offset());
        jit_return!(self, ok);
        debug!("Ops before finalize: {:02x?}", self.ops);
        let mut ojf = OwnedJittedFunc::new(self.ops, start_offset);

        // #[cfg(not(test))]
        {
            // println!("Debug info: {:#06x?}", self.debug_info);
            let debug_info = self.debug_info.compile(&ojf);
            if let Err(e) = debug_info.as_ref() {
                warn!("Failed to create debug info for JIT block: {}", e);
            }
            ojf.debug_obj_file = debug_info.ok();
        }

        (ojf, self.current_pc)
    }

    fn assemble_nop(&mut self) {
        self.advance(false, 1, 4, "NOP");
    }

    fn assemble_stop(&mut self) {
        // STOP instruction eats the next byte for some reason.
        let _ = self.get_imm_one_byte().unwrap();
        self.advance(true, 2, 4, "STOP");
        call_jit_helper!(self, jit_helper_stop);
    }

    fn assemble_aluop_a_n(&mut self, op: ALUOp) {
        let imm = self.get_imm_one_byte().unwrap();
        self.advance(false, 2, 8, format_args!("{} A, {:#04x}", op, imm));

        match op {
            ALUOp::ADD => {
                dynasm!(self.ops =>
                        ; add BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry; carry;)
                );
            }
            ALUOp::ADC => {
                dynasm!(self.ops =>
                        ;; jit_set_x86_flags_from_gb!(self, zero, halfcarry, carry)
                        ; adc BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry; carry;)
                );
            }
            ALUOp::SUB => {
                dynasm!(self.ops =>
                        ; sub BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
            ALUOp::SBC => {
                dynasm!(self.ops =>
                        ;; jit_set_x86_flags_from_gb!(self, zero, halfcarry, carry)
                        ; sbb BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
            ALUOp::AND => {
                dynasm!(self.ops =>
                        ; and BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 1; carry = 0;)
                );
            }
            ALUOp::XOR => {
                dynasm!(self.ops =>
                        ; xor BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry = 0;)
                );
            }
            ALUOp::OR => {
                dynasm!(self.ops =>
                        ; or BYTE [reg_ptr + Registers::OFFSET_A], imm as i8
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry = 0;)
                );
            }
            ALUOp::CP => {
                dynasm!(self.ops =>
                        ; mov al, [reg_ptr + Registers::OFFSET_A]
                        ; sub al, imm as i8
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
        }
    }

    fn assemble_aluop_a_r(&mut self, op: ALUOp, reg: Reg) {
        let cycle_count = match reg {
            Reg::IndHL => 8,
            _ => 4,
        };
        self.advance(false, 1, cycle_count, format_args!("{} A, {}", op, reg));

        if let Reg::IndHL = reg {
            dynasm!(self.ops =>
                    ; mov ah, [reg_ptr + Registers::OFFSET_H]
                    ; mov al, [reg_ptr + Registers::OFFSET_L]
                    ; mov [rbp - 0x10], rax
                    ; lea rdx, [rbp - 0x08]
                    ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                    ; mov al, BYTE [rbp - 0x08]
            );
        } else {
            dynasm!(self.ops =>
                    ; mov al, [reg_ptr + reg.offset()]
            );
        }
        match op {
            ALUOp::ADD => {
                dynasm!(self.ops =>
                        ; add BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry; carry;)
                );
            }
            ALUOp::ADC => {
                dynasm!(self.ops =>
                        ; push rax
                        ;; jit_set_x86_flags_from_gb!(self, zero, halfcarry, carry)
                        ; pop rax
                        ; adc BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry; carry;)
                );
            }
            ALUOp::SUB => {
                dynasm!(self.ops =>
                        ; sub BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
            ALUOp::SBC => {
                dynasm!(self.ops =>
                        ; push rax
                        ;; jit_set_x86_flags_from_gb!(self, zero, halfcarry, carry)
                        ; pop rax
                        ; sbb BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
            ALUOp::AND => {
                dynasm!(self.ops =>
                        ; and BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 1; carry = 0;)
                );
            }
            ALUOp::XOR => {
                dynasm!(self.ops =>
                        ; xor BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry = 0;)
                );
            }
            ALUOp::OR => {
                dynasm!(self.ops =>
                        ; or BYTE [reg_ptr + Registers::OFFSET_A], al
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry = 0;)
                );
            }
            ALUOp::CP => {
                dynasm!(self.ops =>
                        ; mov ah, [reg_ptr + Registers::OFFSET_A]
                        ; sub ah, al
                        ;; jit_set_flags!(self; zero; sub = 1; halfcarry; carry;)
                );
            }
        }
    }

    fn assemble_add_hl_rp(&mut self, pair: RegPair) {
        self.advance(false, 1, 8, format_args!("ADD HL, {}", pair));

        dynasm!(self.ops =>
                ; mov al, BYTE [reg_ptr + Registers::OFFSET_L]
                ; mov ah, BYTE [reg_ptr + Registers::OFFSET_H]
                ; mov dl, BYTE [reg_ptr + pair.offset_lsb()]
                ; mov dh, BYTE [reg_ptr + pair.offset_msb()]
                ; add ax, dx
                ; mov BYTE [reg_ptr + Registers::OFFSET_L], al
                ; mov BYTE [reg_ptr + Registers::OFFSET_H], ah
        );
        jit_set_flags!(self; sub = 0; halfcarry; carry;);
    }

    fn assemble_add_sp_d(&mut self) {
        let imm = self.get_imm_one_byte_signed().unwrap();
        self.advance(false, 2, 16, format_args!("ADD SP, {:#04x}", imm));

        dynasm!(self.ops =>
                ; add WORD [reg_ptr + Registers::OFFSET_SP], imm as i16
        );
        jit_set_flags!(self; zero = 0; sub = 0; halfcarry; carry;);
    }

    fn assemble_call_nn(&mut self) {
        let target = self.get_imm_two_bytes().unwrap();
        self.advance(true, 3, 24, format_args!("CALL {:#06x}", target));

        let next_pc = self.current_pc;
        self.jit_helper_push_imm(next_pc);
        jit_set_pc!(self, 0, target as i16);
    }

    fn assemble_call_cc_nn(&mut self, cc: ConditionCode) {
        let target = self.get_imm_two_bytes().unwrap();
        self.advance(true, 3, 12, format_args!("CALL {}, {:#06x}", cc, target));

        jit_check_cc!(
            self,
            cc,
            {
                let next_pc = self.current_pc;
                self.jit_helper_push_imm(next_pc);
                jit_set_pc!(self, 12, target as i16)
            }
        );
    }

    fn assemble_cb_instr(&mut self) {
        let op_byte = self.get_imm_one_byte().unwrap();
        let reg = Reg::try_from(op_byte & 0x07).unwrap();
        let op = CBOp::try_from(op_byte >> 3).unwrap();
        let cycle_count = match reg {
            Reg::IndHL => 16,
            _ => 8,
        };
        self.advance(false, 2, cycle_count, format_args!("{} {}", op, reg));

        // put target in `al`
        match reg {
            Reg::IndHL => {
                dynasm!(self.ops =>
                        ; mov ah, [reg_ptr + Registers::OFFSET_H]
                        ; mov al, [reg_ptr + Registers::OFFSET_L]
                        ; lea rdx, [rbp - 0x8]
                        ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                        ; mov al, [rbp - 0x8]
                );
            }
            _ => {
                dynasm!(self.ops =>
                        ; mov al, [reg_ptr + reg.offset()]
                );
            }
        }

        // The BIT operation doesn't write back the result, so we need to be
        // able to skip that.
        let mut write = true;

        // Do operation, leaving the result in `al`. Note that `jit_set_flags!`
        // overwrites that register.
        match op {
            CBOp::RLC => {
                dynasm!(self.ops =>
                        ; rol al, 1
                        // The x86 `rol` instruction doesn't set the zero flag,
                        // so we have to do that ourselves.
                        ; lahf
                        ; cmp al, 0
                        ; jne >rlc_not_zero
                        ; or ah, X86Flags::ZERO.bits() as i8
                        ; rlc_not_zero:
                        ; sahf
                        ; push rax
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry;)
                        ; pop rax
                );
            }
            CBOp::RRC => {
                dynasm!(self.ops =>
                        ; ror al, 1
                        // The x86 `ror` instruction doesn't set the zero flag,
                        // so we have to do that ourselves.
                        ; lahf
                        ; cmp al, 0
                        ; jne >rrc_not_zero
                        ; or ah, X86Flags::ZERO.bits() as i8
                        ; rrc_not_zero:
                        ; sahf
                        ; push rax
                        ;; jit_set_flags!(self; zero; sub = 0; halfcarry = 0; carry;)
                        ; pop rax
                );
            }
            CBOp::RL => {
                // Roll left through the carry bit. Loading to/from the x86
                // registers takes a lot of code, so we'll set the flags
                // manually.
                dynasm!(self.ops =>
                        ; mov ah, [reg_ptr + Registers::OFFSET_F]
                        ; and ah, GBFlags::CARRY.bits() as _
                        // move the carry bit to the left-most bit of `ah`
                        ; shl ah, 7u8.checked_sub(gb_flag_indexes::CARRY).unwrap() as _
                        // 16-bit roll left, with `ah` holding our fake carry flag.
                        ; rol ax, 1
                        // Zero out the `dl` register for the new flags value.
                        ; xor dl, dl
                        ; cmp al, 0
                        ; jnz >rl_zero_done
                        ; or dl, GBFlags::ZERO.bits() as _
                        ; rl_zero_done:
                        // If the top bit was rotated into the carry, `ah` is non-zero.
                        ; cmp ah, 0
                        ; jz >rl_carry_done
                        ; or dl, GBFlags::CARRY.bits() as _
                        ; rl_carry_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], dl
                );
            }
            CBOp::RR => {
                dynasm!(self.ops =>
                        ; mov ah, [reg_ptr + Registers::OFFSET_F]
                        ; and ah, GBFlags::CARRY.bits() as _
                        // move the carry bit to the right-most bit of `ah`
                        ; shr ah, gb_flag_indexes::CARRY as _
                        // 16-bit roll right, with `ah` holding our fake carry flag.
                        ; ror ax, 1
                        // Zero out the `dl` register for the new flags value.
                        ; xor dl, dl
                        ; cmp al, 0
                        ; jnz >rr_zero_done
                        ; or dl, GBFlags::ZERO.bits() as _
                        ; rr_zero_done:
                        // If the top bit was rotated into the carry, `ah` is non-zero.
                        ; cmp ah, 0
                        ; jz >rr_carry_done
                        ; or dl, GBFlags::CARRY.bits() as _
                        ; rr_carry_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], dl
                );
            }
            CBOp::SLA => {
                // Shift `al` left one bit, putting the previous top bit into
                // the carry flag.
                dynasm!(self.ops =>
                        ; xor ah, ah
                        ; shl ax, 1
                        ; shl ah, gb_flag_indexes::CARRY as _
                        ; cmp al, 0
                        ; jne >sla_zero_done
                        ; or ah, GBFlags::ZERO.bits() as _
                        ; sla_zero_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], ah
                );
            }
            CBOp::SRA => {
                // Shift `al` right one bit, filling in with the previous high
                // bit. The previous low bit goes into the carry flag.
                dynasm!(self.ops =>
                        ; mov ah, al
                        ; xor al, al
                        ; sar ax, 1
                        ; shr al, 7u8.checked_sub(gb_flag_indexes::CARRY).unwrap() as _
                        ; cmp ah, 0
                        ; jne >sra_zero_done
                        ; or al, GBFlags::ZERO.bits() as _
                        ; sra_zero_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], al
                        ; mov al, ah
                );
            }
            CBOp::SWAP => {
                dynasm!(self.ops =>
                        ; mov ah, al
                        ; and ah, 0xf0u8 as _
                        ; and al, 0x0fu8 as _
                        ; shr ah, 4
                        ; shl al, 4
                        ; or al, ah
                        ; xor dl, dl
                        ; cmp al, 0
                        ; jne >swap_zero_done
                        ; or dl, GBFlags::ZERO.bits() as _
                        ; swap_zero_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], dl
                );
            }
            CBOp::SRL => {
                // Shift `al` right one bit, filling in with zero. The previous
                // low bit goes into the carry flag.
                dynasm!(self.ops =>
                        ; mov ah, al
                        ; xor al, al
                        ; shr ax, 1
                        ; shr al, 7u8.checked_sub(gb_flag_indexes::CARRY).unwrap() as _
                        ; cmp ah, 0
                        ; jne >sra_zero_done
                        ; or al, GBFlags::ZERO.bits() as _
                        ; sra_zero_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], al
                        ; mov al, ah
                );
            }
            CBOp::BIT(n) => {
                // Set the zero flag if bit `n` of `al` is zero. Also sets the half-carry flag to 1.
                write = false;
                dynasm!(self.ops =>
                        ; mov ah, [reg_ptr + Registers::OFFSET_F]
                        ; or ah, GBFlags::HALF_CARRY.bits() as _
                        ; and ah, !GBFlags::ZERO.bits() as _
                );
                if n > 0 {
                    dynasm!(self.ops =>
                            ; shr al, n as _
                    );
                }
                dynasm!(self.ops =>
                        ; and al, 0x01 as _
                        ; cmp al, 0
                        ; jne >bit_zero_done
                        ; or ah, GBFlags::ZERO.bits() as _
                        ; bit_zero_done:
                        ; mov [reg_ptr + Registers::OFFSET_F], ah
                );
            }
            CBOp::RES(n) => {
                dynasm!(self.ops =>
                        ; and al, (!(1u8 << n)) as i8
                );
            }
            CBOp::SET(n) => {
                dynasm!(self.ops =>
                        ; or al, (1u8 << n) as i8
                );
            }
        }

        // move the result from `al` to the destination.
        match (write, reg) {
            (true, Reg::IndHL) => {
                dynasm!(self.ops =>
                        ; mov dl, al
                        ; mov ah, [reg_ptr + Registers::OFFSET_H]
                        ; mov al, [reg_ptr + Registers::OFFSET_L]
                        ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE dl)
                );
            }
            (true, _) => {
                dynasm!(self.ops =>
                        ; mov [reg_ptr + reg.offset()], al
                );
            }
            (false, _) => (),
        }

    }

    fn assemble_ccf(&mut self) {
        self.advance(false, 1, 4, "CCF");

        dynasm!(self.ops =>
                ; xor [reg_ptr + Registers::OFFSET_F], GBFlags::CARRY.bits() as i8
        );
    }
    fn assemble_cpl(&mut self) {
        self.advance(false, 1, 4, "CPL");

        dynasm!(self.ops =>
                ; xor BYTE [reg_ptr + Registers::OFFSET_A], 0xff_u8 as i8
                ; or [reg_ptr + Registers::OFFSET_F], (GBFlags::SUB | GBFlags::HALF_CARRY).bits() as _
        );
    }

    fn assemble_daa(&mut self) {
        self.advance(false, 1, 4, "DAA");

        call_jit_helper!(self, jit_helper_daa);
    }

    fn assemble_dec_r(&mut self, reg: Reg) {
        if let Reg::IndHL = reg {
            self.advance(false, 1, 12, "DEC (HL)");
            dynasm!(self.ops =>
                    // Store address to read from in ax, and save it on the stack.
                    ; mov al, BYTE [reg_ptr + Registers::OFFSET_L]
                    ; mov ah, BYTE [reg_ptr + Registers::OFFSET_H]
                    // Set up RDX to store pointer to read data.
                    // ; xor rdx, rdx
                    // ; mov [rbp - 8], rdx
                    ; lea rdx, [rbp - 8]
                    // Actually read the memory.
                    ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                    ; mov rdx, [rbp - 8]
                    ; sub dl, 1
                    ; mov [rbp - 8], dl
                    ;; jit_set_flags!(self; zero; sub = 1; halfcarry;)
                    // Re-load address
                    ; mov al, BYTE [reg_ptr + Registers::OFFSET_L]
                    ; mov ah, BYTE [reg_ptr + Registers::OFFSET_H]
                    ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE [rbp - 8])
            );
        } else {
            self.advance(false, 1, 4, format_args!("DEC {}", reg));

            dynasm!(self.ops =>
                    ; mov al, BYTE [reg_ptr + reg.offset()]
                    ; sub al, 1
                    ; mov BYTE [reg_ptr + reg.offset()], al
            );
            jit_set_flags!(
                self;
                zero;
                sub = 1;
                halfcarry;
            );
        }
    }

    fn assemble_dec_rp(&mut self, pair: RegPair) {
        self.advance(false, 1, 8, format_args!("DEC {}", pair));

        dynasm!(self.ops =>
                ; mov al, BYTE [reg_ptr + pair.offset_lsb()]
                ; mov ah, BYTE [reg_ptr + pair.offset_msb()]
                ; sub ax, 1
                ; mov BYTE [reg_ptr + pair.offset_lsb()], al
                ; mov BYTE [reg_ptr + pair.offset_msb()], ah
        );
    }

    fn assemble_di(&mut self) {
        self.advance(false, 1, 4, "DI");

        dynasm!(self.ops =>
                ; mov BYTE [reg_ptr + Registers::OFFSET_IME], 0
        );
    }

    fn assemble_ei(&mut self) {
        self.advance(true, 1, 4, "EI");

        dynasm!(self.ops =>
                ; mov BYTE [reg_ptr + Registers::OFFSET_IME], 1
        );
    }

    fn assemble_halt(&mut self) {
        self.advance(true, 1, 4, "HALT");

        // TODO: halting the CPU should do something.
    }

    fn assemble_inc_r(&mut self, reg: Reg) {
        if let Reg::IndHL = reg {
            self.advance(false, 1, 12, "INC (HL)");
            dynasm!(self.ops =>
                    // Store address to read from in ax, and save it on the stack.
                    ; mov al, BYTE [reg_ptr + Registers::OFFSET_L]
                    ; mov ah, BYTE [reg_ptr + Registers::OFFSET_H]
                    ; mov [rbp - 0x10], rax
                    // Set up RDX to store pointer to read data.
                    // ; xor rdx, rdx
                    // ; mov [rbp - 8], rdx
                    ; lea rdx, [rbp - 8]
                    // Actually read the memory.
                    ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                    ; add BYTE [rbp - 8], 1
                    ;; jit_set_flags!(self; zero; sub = 0; halfcarry;)
                    // Restore pointer to read/write address from stack.
                    ; mov rax, [rbp - 16]
                    ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE [rbp - 8])
            );
        } else {
            self.advance(false, 1, 4, format_args!("INC {}", reg));

            dynasm!(self.ops =>
                    ; mov al, BYTE [reg_ptr + reg.offset()]
                    ; add al, 1
                    ; mov BYTE [reg_ptr + reg.offset()], al
            );
            jit_set_flags!(self; zero; sub = 0; halfcarry;);
        }
    }

    fn assemble_inc_rp(&mut self, pair: RegPair) {
        self.advance(false, 1, 8, format_args!("INC {}", pair));
        dynasm!(self.ops =>
                ; mov al, BYTE [reg_ptr + pair.offset_lsb()]
                ; mov ah, BYTE [reg_ptr + pair.offset_msb()]
                ; add ax, 1
                ; mov BYTE [reg_ptr + pair.offset_lsb()], al
                ; mov BYTE [reg_ptr + pair.offset_msb()], ah
        );
    }

    fn assemble_jr_cc_n(&mut self, cc: ConditionCode) {
        let n = self.get_imm_one_byte_signed().unwrap();
        let target = self.current_pc.wrapping_add(n as i16 as u16);
        self.advance(true, 2, 8, format_args!("JR {}, {:+}\t; -> {:#06x}", cc, n, target));

        jit_check_cc!(self, cc, {
            jit_set_pc!(self, 4, target as i16);
        });
    }

    fn assemble_jr_n(&mut self) {
        let n = self.get_imm_one_byte_signed().unwrap();
        let target = self.current_pc.wrapping_add(n as i16 as u16);
        self.advance(true, 2, 12, format_args!("JR {:+}\t; -> {:#06x}", n, target));

        jit_set_pc!(self, 0, target as i16);
    }

    fn assemble_ld_rp_nn(&mut self, pair: RegPair) {
        let word = self.get_imm_two_bytes().unwrap();
        let low_byte = word as u8;
        let high_byte = (word >> 8) as u8;
        self.advance(false, 3, 12, format_args!("LD {}, {:#06x}", pair, word));

        dynasm!(self.ops =>
                ; mov BYTE [reg_ptr + pair.offset_msb()], high_byte as _
                ; mov BYTE [reg_ptr + pair.offset_lsb()], low_byte as _
        );
    }

    fn assemble_ld_a_indhlindex<const PLUS: bool>(&mut self) {
        if PLUS {
            self.advance(false, 1, 8, "LD A, (HL++)");
        } else {
            self.advance(false, 1, 8, "LD, A (HL--)");
        }

        dynasm!(self.ops =>
                ; mov ah, [reg_ptr + Registers::OFFSET_H]
                ; mov al, [reg_ptr + Registers::OFFSET_L]
                ; mov [rbp - 0x10], rax
                ; lea rdx, [rbp - 0x08]
                ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                ; mov rax, [rbp - 0x08]
                ; mov [reg_ptr + Registers::OFFSET_A], al
                ; mov rax, [rbp - 0x10]
        );
        if PLUS {
            dynasm!(self.ops =>
                    ; inc ax
            );
        } else {
            dynasm!(self.ops =>
                    ; dec ax
            );
        }
        dynasm!(self.ops =>
                ; mov [reg_ptr + Registers::OFFSET_H], ah
                ; mov [reg_ptr + Registers::OFFSET_L], al
        );
    }

    fn assemble_ld_a_indnn(&mut self) {
        let addr = self.get_imm_two_bytes().unwrap();
        self.advance(false, 3, 16, format_args!("LD A, ({:#06x})", addr));

        dynasm!(self.ops =>
                ; lea rdx, [reg_ptr + Registers::OFFSET_A]
                ;; call_jit_helper!(self, jit_helper_read, WORD (addr as i16), QWORD rdx)
        );
    }

    fn assemble_ld_a_indrp(&mut self, pair: RegPair) {
        self.advance(false, 1, 8, format_args!("LD A, ({})", pair));

        dynasm!(self.ops =>
                ; mov ah, [reg_ptr + pair.offset_msb()]
                ; mov al, [reg_ptr + pair.offset_lsb()]
                ; mov BYTE [rbp - 8], 0x00
                ; lea rdx, [rbp - 0x8]
                ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                ; mov al, [rbp - 0x8]
                ; mov [reg_ptr + Registers::OFFSET_A], al
        );
    }

    fn assemble_ld_hl_sp_n(&mut self) {
        let offset = self.get_imm_one_byte_signed().unwrap() as i16;

        self.advance(
            false,
            2,
            12,
            format_args!(
                "LD HL, SP {} {}",
                if offset < 0 { "-" } else { "+" },
                offset.abs(),
            ),
        );

        dynasm!(self.ops =>
                ; mov ax, [reg_ptr + Registers::OFFSET_SP]
                ; add ax, offset
                ; mov [reg_ptr + Registers::OFFSET_H], ah
                ; mov [reg_ptr + Registers::OFFSET_L], al
        );
    }

    fn assemble_ld_indhlindex_a<const PLUS: bool>(&mut self) {
        if PLUS {
            self.advance(false, 1, 8, "LD (HL++), A");
        } else {
            self.advance(false, 1, 8, "LD (HL--), A");
        }

        dynasm!(self.ops =>
                ; mov ah, [reg_ptr + Registers::OFFSET_H]
                ; mov al, [reg_ptr + Registers::OFFSET_L]
                ; mov [rbp - 0x08], rax
                ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE [reg_ptr + Registers::OFFSET_A])
                ; mov rax, [rbp - 0x08]
        );
        if PLUS {
            dynasm!(self.ops =>
                    ; inc ax
            );
        } else {
            dynasm!(self.ops =>
                    ; dec ax
            );
        }
        dynasm!(self.ops =>
                ; mov [reg_ptr + Registers::OFFSET_H], ah
                ; mov [reg_ptr + Registers::OFFSET_L], al
        );
    }

    fn assemble_ld_indnn_sp(&mut self) {
        let addr = self.get_imm_two_bytes().unwrap();
        self.advance(false, 3, 20, format_args!("LD ({:#06x}), SP", addr));

        call_jit_helper!(self, jit_helper_write, WORD (addr as i16), BYTE [reg_ptr + RegPair::SP.offset_msb()]);
        call_jit_helper!(self, jit_helper_write, WORD ((addr + 1) as i16), BYTE [reg_ptr + RegPair::SP.offset_msb()]);
    }

    fn assemble_ld_indnn_a(&mut self) {
        let addr = self.get_imm_two_bytes().unwrap();
        self.advance(false, 3, 16, format_args!("LD ({:#06x}), A", addr));

        call_jit_helper!(self, jit_helper_write, WORD (addr as i16), BYTE [reg_ptr + Registers::OFFSET_A]);
    }

    fn assemble_ld_indrp_a(&mut self, pair: RegPair) {
        self.advance(false, 1, 8, format_args!("LD ({}), A", pair));

        dynasm!(self.ops =>
                ; mov ah, [reg_ptr + pair.offset_msb()]
                ; mov al, [reg_ptr + pair.offset_lsb()]
                ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE [reg_ptr + Registers::OFFSET_A])
        );
    }

    fn assemble_ld_r_n(&mut self, reg: Reg) {
        let n = self.get_imm_one_byte().unwrap();
        let cycle_count = match reg {
            Reg::IndHL => 12,
            _ => 8,
        };
        self.advance(false, 2, cycle_count, format_args!("LD {}, {:#04x}", reg, n));

        if let Reg::IndHL = reg {
            dynasm!(self.ops =>
                    ; mov ah, [reg_ptr + Registers::OFFSET_H]
                    ; mov al, [reg_ptr + Registers::OFFSET_L]
                    ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE (n as i8))
            );
        } else {
            dynasm!(self.ops =>
                    ; mov BYTE [reg_ptr + reg.offset()], n as i8
            );
        }
    }

    fn assemble_ld_r_r(&mut self, left: Reg, right: Reg) {
        let cycle_count = match (left, right) {
            (Reg::IndHL, Reg::IndHL) => panic!("`LD (HL), (HL)` is not a valid instruction"),
            (Reg::IndHL, _) | (_, Reg::IndHL) => 8,
            _ => 4,
        };
        self.advance(false, 1, cycle_count, format_args!("LD {}, {}", left, right));

        if left != right {
            return;
        }

        if let Reg::IndHL = right {
            dynasm!(self.ops =>
                ; mov ah, [reg_ptr + Registers::OFFSET_H]
                ; mov al, [reg_ptr + Registers::OFFSET_L]
                ; lea rdx, [rbp - 0x8]
                ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
                ; mov al, BYTE [rbp - 0x8]
            );
        } else {
            dynasm!(self.ops =>
                    ; mov al, [reg_ptr + right.offset()]
            );
        }
        if let Reg::IndHL = left {
            dynasm!(self.ops =>
                    ; mov dh, [reg_ptr + Registers::OFFSET_H]
                    ; mov dl, [reg_ptr + Registers::OFFSET_L]
                    ;; call_jit_helper!(self, jit_helper_read, WORD dx, BYTE al)
            );
        } else {
            dynasm!(self.ops =>
                    ; mov [reg_ptr + right.offset()], al
            );
        }
    }

    fn assemble_ld_sp_hl(&mut self) {
        self.advance(false, 1, 8, "LD SP, HL");

        dynasm!(self.ops =>
                ; mov ah, [reg_ptr + Registers::OFFSET_H]
                ; mov al, [reg_ptr + Registers::OFFSET_L]
                ; mov [reg_ptr + Registers::OFFSET_SP], ax
        );
    }

    fn assemble_ldh_a_indc(&mut self) {
        self.advance(false, 1, 8, "LDH A, (C)");

        dynasm!(self.ops =>
                ; mov ah, 0xffu8 as i8
                ; mov al, [reg_ptr + Registers::OFFSET_C]
                ; lea rdx, [reg_ptr + Registers::OFFSET_A]
                ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
        );
    }

    fn assemble_ldh_a_indn(&mut self) {
        let imm = self.get_imm_one_byte().unwrap();
        self.advance(false, 2, 12, format_args!("LDH A, ({:#04x})", imm));

        dynasm!(self.ops =>
                ; lea rdx, [reg_ptr + Registers::OFFSET_A]
                ;; call_jit_helper!(self, jit_helper_read, WORD ((0xff00 | (imm as u16)) as i16), QWORD rdx)
        );
    }

    fn assemble_ldh_indc_a(&mut self) {
        self.advance(false, 1, 8, "LDH (C), A");

        dynasm!(self.ops =>
                ; mov ah, 0xffu8 as i8
                ; mov al, [reg_ptr + Registers::OFFSET_C]
                ;; call_jit_helper!(self, jit_helper_write, WORD ax, BYTE [reg_ptr + Registers::OFFSET_A])
        );
    }

    fn assemble_ldh_indn_a(&mut self) {
        let imm = self.get_imm_one_byte().unwrap();
        if imm == 0x46 {
            // Starting DMA will temporarily invalidate the memory map, so we'll
            // stop assembling here.
            self.advance(true, 2, 12, format_args!("LDH (0x46), A"));
        } else {
            self.advance(false, 2, 12, format_args!("LDH ({:#04x}), A", imm));
        }

        call_jit_helper!(
            self,
            jit_helper_write,
            WORD ((0xff00 | (imm as u16)) as i16),
            BYTE [reg_ptr + Registers::OFFSET_A]
        );
    }

    fn assemble_jp_cc_nn(&mut self, cc: ConditionCode) {
        let target = self.get_imm_two_bytes().unwrap();
        self.advance(true, 3, 12, format_args!("JP {:#06x}", target));

        jit_check_cc!(self, cc, {
            jit_set_pc!(self, 4, target as i16);
        });
    }

    fn assemble_jp_hl(&mut self) {
        self.advance(true, 1, 4, "JP HL");

        dynasm!(self.ops =>
                ; mov al, [reg_ptr + Registers::OFFSET_L]
                ; mov ah, [reg_ptr + Registers::OFFSET_H]
        );
        jit_set_pc!(self, 0, ax)
    }

    fn assemble_jp_nn(&mut self) {
        let dest = self.get_imm_two_bytes().unwrap();
        self.advance(true, 3, 16, format_args!("JP {:#06x}", dest));

        jit_set_pc!(self, 0, dest as i16);
    }

    fn assemble_pop_rp(&mut self, pair: RegPair) {
        self.advance(false, 1, 12, format_args!("POP {}", pair));

        dynasm!(self.ops =>
                ;; self.jit_helper_pop_to_ax()
                ; mov BYTE [reg_ptr + pair.offset_msb()], ah
                ; mov BYTE [reg_ptr + pair.offset_msb()], al
        );
    }

    fn assemble_push_rp(&mut self, pair: RegPair) {
        self.advance(false, 1, 16, format_args!("PUSH {}", pair));

        dynasm!(self.ops =>
                ; mov ah, BYTE [reg_ptr + pair.offset_msb()]
                ; mov al, BYTE [reg_ptr + pair.offset_msb()]
                ;; self.jit_helper_push_ax()
        );
    }

    fn assemble_ret(&mut self) {
        self.advance(true, 1, 16, "RET");

        self.jit_helper_pop_to_ax();
        jit_set_pc!(self, 0, ax);
    }

    fn assemble_ret_cc(&mut self, cc: ConditionCode) {
        self.advance(true, 1, 8, format_args!("RET {}", cc));

        jit_check_cc!(self, cc, {
            self.jit_helper_pop_to_ax();
            jit_set_pc!(self, 12, ax);
        });
    }

    fn assemble_rla(&mut self) {
        self.advance(false, 1, 4, "RLA");

        // The RLA instruction rotates left through the carry flag.
        //
        // --- C <---  bbbbbbbb  <--
        // |                         |
        // ------------- > ----------
        //
        // The bits in the register are all rotated left, the 7th bit goes to
        // the carry flag and the carry flag goes to bit 0.

        dynasm!(self.ops =>
                ;; jit_set_x86_flags_from_gb!(self, carry)
                // Rotate A through the carry flag
                ; mov al, [reg_ptr + Registers::OFFSET_A]
                ; rcl al, 1
                ; mov [reg_ptr + Registers::OFFSET_A], al
                // Put the new x86 carry bit into the GB carry bit
                ;; jit_set_flags!(self; carry;)
        );
    }

    fn assemble_rlca(&mut self) {
        self.advance(false, 1, 4, "RLCA");

        // RLCA rotates the A register to the left one place. The 7th bit is put
        // back into the 0 position. The 7th bit also goes to the carry flag.

        dynasm!(self.ops =>
                ; mov al, BYTE [reg_ptr + Registers::OFFSET_A]
                ; rol al, 1
                ; mov BYTE [reg_ptr + Registers::OFFSET_A], al
                ;; jit_set_flags!(self; carry;)
        );
    }

    fn assemble_rra(&mut self) {
        self.advance(false, 1, 4, "RRA");

        // The RRA rotates right through the  carry flag

        //  ---> bbbbbbbb ---> C --
        // |                       |
        //  ----------- < ---------

        // The register is shifted right by one, the 0 bit goes to the carry
        // flag,  and  the carry flag goes to bit 7.  Flags apart  from  the
        // carry flag are as for RLCA.

        dynasm!(self.ops =>
                // Set the x86 carry bit to the GB carry bit
                ;; jit_debugger!(self)
                ; mov ah, [reg_ptr + Registers::OFFSET_F]
                ; and ah, (!GBFlags::CARRY.bits()) as _
                ; shr ah, gb_flag_indexes::CARRY.checked_sub(x86_flag_indexes::CARRY).unwrap() as _
                ; sahf
                // Rotate A through the carry flag
                ; mov al, [reg_ptr + Registers::OFFSET_A]
                ; rcr al, 1
                ; mov [reg_ptr + Registers::OFFSET_A], al
                // Put the new x86 carry bit into the GB carry bit
                ;; jit_set_flags!(self; carry;)
        );
    }

    fn assemble_rrca(&mut self) {
        self.advance(false, 1, 4, "RRCA");

        // This rotates the register right in a similar way to RLCA.
        // Symbolically:
        //
        //  ---> bbbbbbbb ---> C
        // |              |
        //  ------ < -----
        //
        // The register is shifted right by one, and the 0 bit goes to the carry
        // flag and to the 7th bit. Flags apart from the carry are as for RLCA.

        dynasm!(self.ops =>
                ; ror BYTE [reg_ptr + Registers::OFFSET_A], 1
                ;; jit_set_flags!(self; carry;)
        );
    }

    fn assemble_rst(&mut self, offset: u8) {
        self.advance(true, 1, 16, format_args!("RST {:#04x}", offset));
        let target = offset as u16;

        dynasm!(self.ops =>
                ; mov ax, self.current_pc as i16
        );
        self.jit_helper_push_ax();
        jit_set_pc!(self, 0, target as i16);
    }

    fn assemble_scf(&mut self) {
        self.advance(false, 1, 4, "SCF");

        dynasm!(self.ops =>
                ; or [reg_ptr + Registers::OFFSET_F], GBFlags::CARRY.bits() as i8
        );
    }

    // Pops the low byte from the stack, then the high byte. Words are stored LSB first.
    fn jit_helper_pop_to_ax(&mut self) {
        dynasm!(self.ops =>
            ; mov ax, [reg_ptr + Registers::OFFSET_SP]
            ; lea rdx, [rbp - 8]
            ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
            ; mov ax, [reg_ptr + Registers::OFFSET_SP]
            ; add WORD [reg_ptr + Registers::OFFSET_SP], 1
            ; mov ax, [reg_ptr + Registers::OFFSET_SP]
            ; lea rdx, [rbp - 9]
            ;; call_jit_helper!(self, jit_helper_read, WORD ax, QWORD rdx)
            ; mov ah, [rbp - 9]
            ; mov al, [rbp - 8]
            ; add WORD [reg_ptr + Registers::OFFSET_SP], 1
        );
    }

    // Push the high byte to the stack, then the low byte. Words are stored LSB first.
    fn jit_helper_push_ax(&mut self) {
        dynasm!(self.ops =>
            ; sub WORD [reg_ptr + Registers::OFFSET_SP], 1
            ; mov WORD [rbp - 0x08], ax
            ;; call_jit_helper!(@ignore self, jit_helper_write, WORD [reg_ptr + Registers::OFFSET_SP], BYTE ah)
            ; sub WORD [reg_ptr + Registers::OFFSET_SP], 1
            ; mov ax, WORD [rbp - 0x08]
            ;; call_jit_helper!(self, jit_helper_write, WORD [reg_ptr + Registers::OFFSET_SP], BYTE al)
        );
    }

    // Push the high byte to the stack, then the low byte. Words are stored LSB first.
    fn jit_helper_push_imm(&mut self, word: u16) {
        dynasm!(self.ops =>
            ; sub WORD [reg_ptr + Registers::OFFSET_SP], 1
            ;; call_jit_helper!(@ignore self, jit_helper_write, WORD [reg_ptr + Registers::OFFSET_SP], BYTE ((word >> 8) as u8 as i8))
            ; sub WORD [reg_ptr + Registers::OFFSET_SP], 1
            ;; call_jit_helper!(self, jit_helper_write, WORD [reg_ptr + Registers::OFFSET_SP], BYTE (word as u8 as i8))
        );
    }
}

extern "C" fn jit_helper_write(jit_data: &mut SystemRef, address: u16, data: u8) -> bool {
    trace!("CPU is writing {:#04x} to {:#06x}", data, address);
    jit_data.write(address, data, RwSource::CPU);
    false
}

#[must_use]
extern "C" fn jit_helper_read(jit_data: &mut SystemRef, address: u16, out_ptr: &mut MaybeUninit<u8>) -> bool {
    trace!("CPU is reading address {:#06x} (out_ptr is {:p})", address, out_ptr);
    let data_opt = jit_data.read(address, RwSource::CPU);
    if let Some(data) = data_opt {
        trace!("CPU read {:#04x} from address {:#06x}", data, address);
        out_ptr.write(data);
    }
    false
}

/// Adjusts the `A` register for Binary-Coded Decimal arithmatic.
extern "C" fn jit_helper_daa(jit_data: &mut SystemRef) -> bool {
    let regs = &mut jit_data.cpu.registers;
    let high = (regs.a >> 4) & 0xf;
    let low = regs.a & 0xf;
    let n = regs.f.contains(GBFlags::SUB);
    let c = regs.f.contains(GBFlags::CARRY);
    let h = regs.f.contains(GBFlags::HALF_CARRY);
    let (added, new_c) = match (n, c, high, h, low) {
        (false, false, 0x0..=0x9, false, 0x0..=0x9) => (0x00, false),
        (false, false, 0x0..=0x8, false, 0xA..=0xF) => (0x06, false),
        (false, false, 0x0..=0x9, true,  0x0..=0x3) => (0x06, false),
        (false, false, 0xA..=0xF, false, 0x0..=0x9) => (0x60, true),
        (false, false, 0x9..=0xF, false, 0xA..=0xF) => (0x66, true),
        (false, false, 0xA..=0xF, true,  0x0..=0x3) => (0x66, true),
        (false, true,  0x0..=0x2, false, 0x0..=0x9) => (0x60, true),
        (false, true,  0x0..=0x2, false, 0xA..=0xF) => (0x66, true),
        (false, true,  0x0..=0x3, true,  0x0..=0x3) => (0x66, true),
        (true, false,  0x0..=0x9, false, 0x0..=0x9) => (0x00, false),
        (true, false,  0x0..=0x8, true,  0x6..=0xF) => (0xFA, false),
        (true, true,   0x7..=0xF, false, 0x0..=0x9) => (0xA0, true),
        (true, true,   0x6..=0xF, true,  0x6..=0xF) => (0x9A, true),
        (_, _, 0x0..=0xf, _, 0x0..=0xf) => todo!(),
        (_, _, 0x10.., _, _) | (_, _, _, _, 0x10..) => unreachable!(),
    };
    regs.a += added;
    regs.f.set(GBFlags::CARRY, new_c);
    false
}

extern "C" fn jit_helper_stop(_jit_data: &mut SystemRef) -> bool {
    // info!("Found STOP instruction");
    true
}

#[allow(unused)]
extern "C" fn jit_helper_debug_registers(jit_data: &mut SystemRef) -> bool {
    println!("Current CPU registers: {:?}", jit_data.cpu.registers);
    false
}

// extern "C" fn jit_helper_flags_debug(_jit_data: &mut SystemRef, gb_flags: u8, x86_flags: u8) -> bool {
//     let gb_flags = GBFlags::from_bits_truncate(gb_flags);
//     let x86_flags = X86Flags::from_bits_truncate(x86_flags);
//     debug!("GB flags:  {:?}", gb_flags);
//     debug!("X86 flags: {:?}", x86_flags);
//     false
// }
