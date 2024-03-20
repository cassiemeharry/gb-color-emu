// #![allow(unused)]

use log::*;
use std::{fmt, num::NonZeroU32};

#[cfg(test)]
mod tests;
pub mod ppu;

use self::ppu::PPU;

pinout! {
    pub struct SM83Pinout {
        in p00: bool,
        in p01: bool,
        in p02: bool,
        in p03: bool,
        in p10: bool,
        in p11: bool,
        in p12: bool,
        in p13: bool,
        // unk phi: bool,
        out wr: bool,
        out rd: bool,
        out cs: bool,
        out address: u16,
        inout data: u8,
        out reset: bool,
        out vin: bool,
        out so1: bool,
        out so2: bool,
        out nc: bool,
        in nmi: bool,
        // in mcs0: bool,
        // in mcs1: bool,
        // Memory cluster
        out mwr: bool,
        out mrd: bool,
        out cs1: bool,
        out maddress: u16,
        inout mdata: u8,

        // LCD Controller
        out lcd_r: u8,
        out lcd_g: u8,
        out lcd_b: u8,
        out lcd_push: bool,
        in lcd_accepted: bool,
        in hblank: bool,
        in vblank: bool,
    }
}

impl SM83Pinout {
    pub fn new() -> Self {
        let mut pins = Self::default();
        // Set normally high pins
        pins.set_wr(true);
        pins.set_rd(true);
        pins.set_cs(true);
        pins.set_reset(true);
        pins.set_nmi(true);
        // pins.set_mcs0(true);
        // pins.set_mcs1(true);
        pins.set_cs1(true);
        pins.set_mrd(true);
        pins.set_mwr(true);
        pins
    }

    fn wram_read_start(&mut self, address: u16) {
        trace!("Setting CPU pins for WRAM read with address {:#06x}", address);
        self.set_cs1(false);
        self.set_mrd(false);
        self.set_mwr(true);
        self.set_maddress(address);
        self.set_mdata__opt(None);
    }

    fn wram_read_finish(&mut self) -> u8 {
        let data = self.get_mdata();
        trace!("Setting CPU pins for end of WRAM read with data {:#04x}", data);
        self.set_cs1(true);
        self.set_mrd(true);
        self.set_mwr(true);
        data
    }

    fn wram_write_addr(&mut self, address: u16) {
        trace!("Setting CPU pins for WRAM write with address {:#06x}", address);
        self.set_cs1(false);
        self.set_mrd(true);
        self.set_mwr(false);
        self.set_maddress(address);
    }

    fn wram_write_data(&mut self, data: u8) {
        trace!("Setting CPU pins for WRAM write with data {:#04x}", data);
        self.set_cs1(false);
        self.set_mrd(true);
        self.set_mwr(false);
        self.set_mdata(data);
    }

    fn wram_write_finish(&mut self) {
        trace!("Setting CPU pins for end of WRAM write");
        self.set_cs1(true);
        self.set_mrd(true);
        self.set_mwr(true);
    }

    fn cart_rw_enable(&mut self, enable: bool) {
        self.set_cs(!enable);
    }

    fn cart_read_start(&mut self, address: u16) {
        trace!("Setting CPU pins for Cart read with address {:#06x}", address);
        self.set_cs(false);
        self.set_rd(false);
        self.set_wr(true);
        self.set_address(address);
    }

    fn cart_read_finish(&mut self) -> u8 {
        let data = self.get_data();
        trace!("Setting CPU pins for end of Cart read with data {:#04x}", data);
        self.set_cs(true);
        self.set_rd(true);
        self.set_wr(true);
        data
    }

    fn cart_write_addr(&mut self, address: u16) {
        trace!("Setting CPU pins for Cart write with address {:#06x}", address);
        self.set_cs(false);
        self.set_rd(true);
        self.set_wr(false);
        self.set_address(address);
    }

    fn cart_write_data(&mut self, data: u8) {
        trace!("Setting CPU pins for Cart write with data {:#04x}", data);
        self.set_cs(false);
        self.set_rd(true);
        self.set_wr(false);
        self.set_data(data);
    }

    fn cart_write_finish(&mut self) {
        trace!("Setting CPU pins for end of Cart write");
        self.set_cs(true);
        self.set_rd(true);
        self.set_wr(true);
        self.release_data();
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum R {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    IndHL = 6,
    A = 7
}

const TABLE_R: [R; 8] = [
    R::B, R::C, R::D, R::E,
    R::H, R::L, R::IndHL, R::A,
];

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum Rp1 {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

const TABLE_RP: [Rp1; 4] = [Rp1::BC, Rp1::DE, Rp1::HL, Rp1::SP];

// #[repr(u8)]
// #[derive(Copy, Clone, Debug)]
// enum Rp2 {
//     BC = 0,
//     DE = 1,
//     HL = 2,
//     AF = 3,
// }

// const TABLE_RP2: [Rp2; 4] = [Rp2::BC, Rp2::DE, Rp2::HL, Rp2::AF];

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum CC {
    NZ = 0,
    Z = 1,
    NC = 2,
    C = 3,
    PO = 4,
    PE = 5,
    P = 6,
    M = 7,
}

const TABLE_CC: [CC; 8] = [CC::NZ, CC::Z, CC::NC, CC::C, CC::PO, CC::PE, CC::P, CC::M];

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum ALU {
    ADD_A = 0,
    ADC_A = 1,
    SUB = 2,
    SBC_A = 3,
    AND = 4,
    XOR = 5,
    OR = 6,
    CP = 7,
}

const TABLE_ALU: [ALU; 8] = [
    ALU::ADD_A, ALU::ADC_A, ALU::SUB, ALU::SBC_A,
    ALU::AND, ALU::XOR, ALU::OR, ALU::CP,
];

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum Rot {
    RLC = 0,
    RRC = 1,
    RL = 2,
    RR = 3,
    SLA = 4,
    SRA = 5,
    SLL = 6,
    SRL = 7,
}

const TABLE_ROT: [Rot; 8] = [
    Rot::RLC, Rot::RRC, Rot::RL, Rot::RR,
    Rot::SLA, Rot::SRA, Rot::SLL, Rot::SRL,
];

// #[repr(u8)]
// #[derive(Copy, Clone, Debug)]
// enum InterruptMode {
//     Zero = 0,
//     ZeroOne = 1,
//     One = 2,
//     Two = 3,
// }

// const TABLE_IM: [InterruptMode; 8] = [
//     InterruptMode::Zero, InterruptMode::ZeroOne, InterruptMode::One, InterruptMode::Two,
//     InterruptMode::Zero, InterruptMode::ZeroOne, InterruptMode::One, InterruptMode::Two,
// ];

#[derive(Clone, PartialEq, Eq)]
pub struct SM83Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
    pub flags: u8,
}

impl fmt::Debug for SM83Registers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SM83Registers")
            .field("a", &format_args!("{:#04x}", self.a))
            .field("b", &format_args!("{:#04x}", self.b))
            .field("c", &format_args!("{:#04x}", self.c))
            .field("d", &format_args!("{:#04x}", self.d))
            .field("e", &format_args!("{:#04x}", self.e))
            .field("h", &format_args!("{:#04x}", self.h))
            .field("l", &format_args!("{:#04x}", self.l))
            .field("sp", &format_args!("{:#06x}", self.sp))
            .field("pc", &format_args!("{:#06x}", self.pc))
            .field("flags", &format_args!("{:#04x}", self.flags))
            .finish()
    }
}

macro_rules! def_flag {
    ($bit:literal, $name:ident) => {
        paste::paste! {
            #[inline(always)]
            pub fn [<get_ $name _flag>](&self) -> bool {
                (self.get_flags() >> $bit) & 1 == 1
            }

            #[inline(always)]
            pub fn [<set_ $name _flag>](&mut self, value: bool) {
                let prev_value = self.get_flags();
                const MASK: u8 = !(1 << $bit);
                let new_value = (prev_value & MASK) | ((value as u8) << $bit);
                self.set_flags(new_value);
            }
        }
    }
}

impl SM83Registers {
    pub fn new() -> Self {
        Self {
            a: 0,
            flags: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0,
        }
    }

    #[inline(always)]
    pub fn get_flags(&self) -> u8 {
        self.flags
    }

    #[inline(always)]
    pub fn set_flags(&mut self, value: u8) {
        self.flags = value;
    }

    #[inline(always)]
    pub fn get_af(&self) -> u16 {
        ((self.a as u16) << 8) | (self.get_flags() as u16)
    }

    #[inline(always)]
    pub fn set_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.set_flags(value as u8);
    }

    #[inline(always)]
    pub fn get_bc(&self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    #[inline(always)]
    pub fn set_bc(&mut self, value: u16) {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
    }

    #[inline(always)]
    pub fn get_de(&self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

    #[inline(always)]
    pub fn set_de(&mut self, value: u16) {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
    }

    #[inline(always)]
    pub fn get_hl(&self) -> u16 {
        ((self.h as u16) << 8) | (self.l as u16)
    }

    #[inline(always)]
    pub fn set_hl(&mut self, value: u16) {
        self.h = (value >> 8) as u8;
        self.l = value as u8;
    }

    #[inline(always)]
    pub fn get_sp(&self) -> u16 {
        self.sp
    }

    #[inline(always)]
    pub fn set_sp(&mut self, value: u16) {
        self.sp = value;
    }

    def_flag!(4, carry);
    def_flag!(5, half_carry);
    def_flag!(6, subtract);
    def_flag!(7, zero);
}

struct FlagDebug(u8);

impl fmt::Debug for FlagDebug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut list = f.debug_list();
        let carry = format_args!("C");
        if self.0 & 0b0000_0001 != 0 {
            list.entry(&carry);
        }
        let subtract = format_args!("N");
        if self.0 & 0b0000_0010 != 0 {
            list.entry(&subtract);
        }
        let half_carry = format_args!("H");
        if self.0 & 0b0001_0000 != 0 {
            list.entry(&half_carry);
        }
        let zero = format_args!("Z");
        if self.0 & 0b0100_0000 != 0 {
            list.entry(&zero);
        }
        list.finish()
    }
}

#[derive(Copy, Clone, Debug)]
enum MemoryMappedAddr {
    Cart { mapped: u16 },
    VideoRAM { mapped: u16 },
    WorkRAM { mapped: u16 },
    OAM { mapped: u8 },
    ForbiddenRange { raw: u16 },
    IO { mapped: u8 },
    HighRAM { mapped: u8 },
    InterruptEnableRegister,
}

#[derive(Copy, Clone, Debug)]
enum RWState {
    None,
    ReadingAddr { addr: MemoryMappedAddr },
    ReadingWait { addr: MemoryMappedAddr },
    ReadingDone { addr: MemoryMappedAddr, data: u8 },
    WritingAddrNoData { addr: MemoryMappedAddr },
    WritingAddrWithData { addr: MemoryMappedAddr, data: u8 },
    WritingData { addr: MemoryMappedAddr, data: u8 },
    WritingDone { addr: MemoryMappedAddr, data: u8 },
}

impl MemoryMappedAddr {
    fn map(addr: u16) -> Self {
        match addr {
            0x0000..=0x7fff => Self::Cart { mapped: addr },
            0x8000..=0x9fff => Self::VideoRAM { mapped: addr - 0x8000 },
            0xa000..=0xbfff => Self::Cart { mapped: addr },
            0xc000..=0xfdff => Self::WorkRAM { mapped: addr - 0xc000 },
            0xfe00..=0xfe9f => Self::OAM { mapped: (addr - 0xfe00) as u8 },
            0xfea0..=0xfeff => Self::ForbiddenRange { raw: addr },
            0xff00..=0xff7f => Self::IO { mapped: (addr - 0xff00) as u8 },
            0xff80..=0xfffe => Self::HighRAM { mapped: (addr - 0xff80) as u8 },
            0xffff => Self::InterruptEnableRegister,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Mode {
    Run,
    Stop,
    Halt { sleep_timer: Option<NonZeroU32> },
}

#[derive(Copy, Clone)]
struct DMATransfer {
    cycle_counter: u16,
    base_address: u16,
    rw_state: RWState,
}

impl fmt::Debug for DMATransfer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("DMATransfer")
            .field("cycle_counter", &self.cycle_counter)
            .field("base_address", &format_args!("{:#06x}", self.base_address))
            .field("rw_state", &self.rw_state)
            .finish()
    }
}

impl DMATransfer {
    fn new(base_address_high: u8) -> Self {
        let base_address = (base_address_high as u16) << 8;
        info!("Triggering DMA transfer from {:#06x} to {:#06x}", base_address, 0xfe00);
        assert!(base_address_high <= 0xdf);
        Self {
            cycle_counter: 0,
            base_address: base_address,
            rw_state: RWState::None,
        }
    }
}

#[derive(Clone)]
pub struct SM83 {
    pub(crate) regs: SM83Registers,
    interrupt_master_enable: bool,
    // interrupt_vector: u8,
    interrupt_enable: u8,
    interrupt_f_reg: u8,
    refresh_counter: u8,
    rw_state: RWState,
    // Lower four bits are a cycle counter for this instruction.
    // If `opcode & 0xf == 0`, we need to start loading in a new insruction.
    opcode: u32,
    immediates: Option<(u8, Option<u8>)>,
    cycle_counter: u64,
    high_ram: Vec<u8>,
    high_speed: bool,
    speed_switch_requested: bool,
    speed_switch_cycles_remaining: u16,
    mode: Mode,
    div_counter: u16,
    joypad_select_action_buttons: bool,
    ppu: PPU,
    dma: Option<DMATransfer>,
}

impl fmt::Debug for SM83 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SM83")
            .field("registers", &self.regs)
            // .field("interrupt_vector", &format_args!("{:#04x}", self.interrupt_vector))
            .field("interrupt_enable", &format_args!("{:#04x}", self.interrupt_enable))
            .field("refresh_counter", &format_args!("{:#04x}", self.refresh_counter))
            .field("rw_state", &self.rw_state)
            .field("opcode[prefix]", &format_args!("{:#04x}", self.opcode >> 12))
            .field("opcode[instr]", &format_args!("{:#04x}", (self.opcode >> 4) & 0xFF))
            .field("opcode[tick]", &format_args!("{}", self.opcode & 0xF))
            .field("immediates", &self.immediates)
            .field("cycle_counter", &self.cycle_counter)
            .field("ppu", &self.ppu)
            .field("dma", &self.dma)
            .finish()
    }
}

impl SM83 {
    pub fn new() -> Self {
        Self {
            regs: SM83Registers::new(),
            interrupt_master_enable: true,
            // interrupt_vector: 0,
            interrupt_enable: 0,
            interrupt_f_reg: 0,
            refresh_counter: 0,
            rw_state: RWState::None,
            opcode: 0,
            immediates: None,
            cycle_counter: 0,
            high_ram: vec![0x00; 0xff - 0x80],
            high_speed: false,
            speed_switch_requested: false,
            speed_switch_cycles_remaining: 0,
            mode: Mode::Run,
            div_counter: 0,
            joypad_select_action_buttons: false,
            ppu: PPU::default(),
            dma: None,
        }
    }

    pub fn get_registers(&self) -> SM83Registers {
        self.regs.clone()
    }

    #[deny(unreachable_patterns)]
    pub fn step_cycle(&mut self, pins: &mut SM83Pinout) {
        // Memory takes 160ns (or 190?) minimum to read/write. The CGB CPU runs
        // at 8.388608 MHz, which gives us a cycle time of 1.19209289551e-7
        // seconds (119.209 ns). This implies that memory access take three
        // cycles from the CPU's perspective:
        //
        // * At the end of the first cycle, the CPU sets the pins up.
        // * During the second cycle, the memory chip is processing the access.
        // * At the beginning of the third cycle, the data is available on the pins from memory.

        if self.speed_switch_cycles_remaining > 0 {
            trace!("CPU halted during speed switch");
            self.speed_switch_cycles_remaining -= 1;
            if self.speed_switch_cycles_remaining == 0 {
                self.mode = Mode::Run;
            }
            return;
        }

        self.handle_interrupts(pins);

        match self.mode {
            Mode::Halt { sleep_timer } => {
                trace!("CPU is halted this cycle");
                if let Some(st) = sleep_timer {
                    self.mode = match NonZeroU32::new(st.get() - 1) {
                        Some(sleep_timer) => Mode::Halt { sleep_timer: Some(sleep_timer) },
                        None => Mode::Run,
                    };
                }
                return;
            }
            Mode::Stop => {
                trace!("CPU stopped this cycle");
                return;
            }
            Mode::Run => (),
        }
        self.div_counter = self.div_counter.wrapping_add(1);

        self.handle_rw_in::<false>(pins);
        self.handle_rw_in::<true>(pins);

        let tick = self.opcode as u8;
        let opcode = (self.opcode >> 8) as u8;
        let prefix = (self.opcode >> 16) as u8;

        let x = opcode >> 6;
        let y = (opcode >> 3) & 0b111;
        let z = opcode & 0b111;
        let p = y >> 1;
        // let q = (y & 1) == 1;

        let mut next_instr = false;
        trace!("Stepping CPU with prefix {:#04x}, opcode {:#04x}, tick {} @ pc {:#06x}", prefix, opcode, tick, self.regs.pc);
        if tick == 3 || (prefix != 0x00 && tick == 7) {
            debug!(
                "Running instruction `{}` (opcode {:#06x}), @ pc {:#06x}",
                display_opcode(prefix, opcode),
                self.opcode >> 8,
                self.regs.pc,
            );
            debug!(
                "with CPU registers: A={:02x} B={:02x} C={:02x} D={:02x} E={:02x}, H={:02x}, L={:02x}, SP={:04x}, PC={:04x}, F={:?}",
                self.regs.a, self.regs.b, self.regs.c, self.regs.d, self.regs.e,
                self.regs.h, self.regs.l, self.regs.sp, self.regs.pc, FlagDebug(self.regs.flags),
            );
        }
        match (prefix, opcode, tick) {
            (0x00, _, 0) | (0xcb, _, 4) => self.mem_read_start(self.regs.pc),
            (0x00, _, 1) | (0xcb, _, 5) => self.incr_pc(),
            (0x00, _, 2) | (0xcb, _, 6) => {
                let opcode_byte = self.mem_read_result() as u32;
                trace!("Setting opcode byte to {:#04x}", opcode_byte);
                self.opcode |= opcode_byte << 8;
            }
            // NOP
            (0, 0x00, 3) => next_instr = true,
            (0, 0x00, 4..) => unreachable!(),
            // LD rp, nn
            (0, 0x01 | 0x11 | 0x21 | 0x31, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 4) => self.incr_pc(),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 6) => self.mem_read_start(self.regs.pc),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 7) => self.incr_pc(),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0x01 | 0x11 | 0x21 | 0x31, 9) => {
                let nn = self.get_immediate_nn();
                self.set_rp(TABLE_RP[p as usize], nn);
                next_instr = true;
            }
            (0, 0x01 | 0x11 | 0x21 | 0x31, 10..) => unreachable!(),
            // LD (BC), A
            (0, 0x02, 3) => self.mem_write_start(self.regs.get_bc(), self.regs.a),
            (0, 0x02, 4) => (),
            (0, 0x02, 5) => (),
            (0, 0x02, 6) => next_instr = true,
            (0, 0x02, 7..) => unreachable!(),
            // INC rp
            (0, 0x03 | 0x13 | 0x23 | 0x33, 3) => (),
            (0, 0x03 | 0x13 | 0x23 | 0x33, 4) => {
                let rp = TABLE_RP[p as usize];
                let value = self.get_rp(rp);
                self.set_rp(rp, value.wrapping_add(1));
            }
            (0, 0x03 | 0x13 | 0x23 | 0x33, 5) => next_instr = true,
            (0, 0x03 | 0x13 | 0x23 | 0x33, 6..) => unreachable!(),
            // INC r
            (0, 0x04 | 0x0c | 0x14 | 0x1c | 0x24 | 0x2c | 0x3c, 3) => {
                let r = TABLE_R[y as usize];
                let prev_value = self.get_r(r);
                let new_value = prev_value.wrapping_add(1);
                self.regs.set_half_carry_flag(prev_value & 0xf == 0xf);
                self.regs.set_subtract_flag(false);
                self.regs.set_zero_flag(new_value == 0x00);
                self.set_r(r, new_value);
                next_instr = true;
            },
            (0, 0x04 | 0x0c | 0x14 | 0x1c | 0x24 | 0x2c | 0x3c, 4..) => unreachable!(),
            // DEC r
            (0, 0x05 | 0x0d | 0x15 | 0x1d | 0x25 | 0x2d | 0x3d, 3) => {
                let r = TABLE_R[y as usize];
                let prev_value = self.get_r(r);
                let new_value = prev_value.wrapping_sub(1);
                self.regs.set_half_carry_flag(prev_value & 0xf == 0);
                self.regs.set_subtract_flag(true);
                self.regs.set_zero_flag(new_value == 0x00);
                self.set_r(r, new_value);
                next_instr = true;
            },
            (0, 0x05 | 0x0d | 0x15 | 0x1d | 0x25 | 0x2d | 0x3d, 4..) => unreachable!(),
            // LD r, n
            (0, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e, 4) => self.incr_pc(),
            (0, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e, 6) => {
                let n = self.get_immediate_n();
                self.set_r(TABLE_R[y as usize], n);
                next_instr = true;
            }
            (0, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e, 7..) => unreachable!(),
            // RLCA
            (0, 0x07, 3) => {
                let prev_a = self.regs.a;
                self.regs.a = prev_a << 1 | prev_a >> 7;
                self.regs.set_carry_flag(prev_a >> 7 == 1);
                next_instr = true;
            }
            (0, 0x07, 4..) => unreachable!(),
            // GameBoy change: LD (nn), SP
            (0, 0x08, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x08, 4) => self.incr_pc(),
            (0, 0x08, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0x08, 6) => self.mem_read_start(self.regs.pc),
            (0, 0x08, 7) => self.incr_pc(),
            (0, 0x08, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0x08, 9) => {
                let nn = self.get_immediate_nn();
                self.mem_write_start(nn, self.regs.sp as u8);
            }
            (0, 0x08, 10) => (),
            (0, 0x08, 11) => (),
            (0, 0x08, 12) => {
                let nn = self.get_immediate_nn();
                let value = (self.regs.sp >> 8) as u8;
                self.mem_write_start(nn.wrapping_add(1), value);
            }
            (0, 0x08, 13) => (),
            (0, 0x08, 14) => next_instr = true,
            (0, 0x08, 15) => unreachable!(),
            // ADD HL, rp
            (0, 0x09 | 0x19 | 0x29 | 0x39, 3..=9) => (),
            (0, 0x09 | 0x19 | 0x29 | 0x39, 10) => {
                let rp = TABLE_RP[p as usize];
                let value = self.get_rp(rp);
                let hl = self.regs.get_hl();
                let new_value = hl.wrapping_add(value);
                self.regs.set_hl(new_value);
                next_instr = true;
            }
            (0, 0x09 | 0x19 | 0x29 | 0x39, 11..) => unreachable!(),
            // LD A, (BC)
            (0, 0x0a, 3) => self.mem_read_start(self.regs.get_bc()),
            (0, 0x0a, 4) => (),
            (0, 0x0a, 5) => self.regs.a = self.mem_read_result(),
            (0, 0x0a, 6) => next_instr = true,
            (0, 0x0a, 7..) => unreachable!(),
            // DEC rp
            (0, 0x0b | 0x1b | 0x2b | 0x3b, 3) => (),
            (0, 0x0b | 0x1b | 0x2b | 0x3b, 4) => {
                let rp = TABLE_RP[p as usize];
                let value = self.get_rp(rp);
                self.set_rp(rp, value.wrapping_sub(1));
            }
            (0, 0x0b | 0x1b | 0x2b | 0x3b, 5) => next_instr = true,
            (0, 0x0b | 0x1b | 0x2b | 0x3b, 6..) => unreachable!(),
            // RRCA
            (0, 0x0f, 3) => {
                let prev_a = self.regs.a;
                self.regs.a = prev_a >> 1 | ((prev_a & 1) << 7);
                self.regs.set_carry_flag((prev_a & 1) == 1);
                next_instr = true;
            }
            (0, 0x0f, 4..) => unreachable!(),
            // GameBoy change: STOP
            //
            // The way this instruction works is poorly documented, so punt on
            // it for now.
            //
            // See https://gbdev.io/pandocs/imgs/gb_stop.png
            (0, 0x10, 3) => {
                let is_button_held_and_selected = false; // TODO
                let interrupt_pending = (self.interrupt_enable & self.interrupt_f_reg) != 0;
                match (is_button_held_and_selected, self.speed_switch_requested, interrupt_pending) {
                    (true, _, true) => {
                        // STOP is a 1-byte opcode, mode doesn't change, DIV doesn't reset.
                        todo!()
                    }
                    (true, _, false) => {
                        // STOP is a 2-byte opcode, HALT mode is entered, DIV is not reset.
                        info!("Halting CPU because of STOP instruction (button held)");
                        self.incr_pc();
                        self.mode = Mode::Halt { sleep_timer: None };
                    }
                    (false, false, false) => {
                        // STOP is a 2-byte opcode, STOP mode is entered, DIV is reset.
                        info!("Stopping CPU because of STOP instruction (normal)");
                        self.incr_pc();
                        self.mode = Mode::Stop;
                        self.div_counter = 0;
                    }
                    (false, false, true) => {
                        // STOP is a 1-byte opcode, STOP mode is entered, DIV is reset.
                        info!("Stopping CPU because of STOP instruction (interrupt pending)");
                        self.mode = Mode::Stop;
                        self.div_counter = 0;
                    }
                    (false, true, false) => {
                        // STOP is a 2-byte opcode, HALT mode is entered, DIV is reset, CPU speed changes.
                        info!("Halting CPU because of STOP instruction (speed switch requested)");
                        self.incr_pc();
                        self.mode = Mode::Halt { sleep_timer: NonZeroU32::new(0x20000) };
                        self.div_counter = 0;
                        self.speed_switch_requested = false;
                        self.speed_switch_cycles_remaining = 8200;
                        self.high_speed = !self.high_speed;
                    }
                    (false, true, true) if self.interrupt_master_enable => panic!("CPU glitches non-deterministically!"),
                    (false, true, true) => {
                        // STOP is a 1-byte opcode, mode doesn't change, DIV is reset, CPU speed changes.
                        info!("Not halting CPU because of STOP instruction (speed switch requested, interrupt pending)");
                        self.div_counter = 0;
                        self.speed_switch_requested = false;
                        self.speed_switch_cycles_remaining = 8200;
                        self.high_speed = !self.high_speed;
                    }
                }
                next_instr = true;
            }
            (0, 0x10, 4..) => unreachable!(),
            // LD (DE), nn
            (0, 0x12, 3) => self.mem_write_start(self.regs.get_de(), self.regs.a),
            (0, 0x12, 4) => (),
            (0, 0x12, 5) => (),
            (0, 0x12, 6) => next_instr = true,
            (0, 0x12, 7..) => unreachable!(),
            // RLCA
            (0, 0x17, 3) => {
                let prev_a = self.regs.a;
                self.regs.a = prev_a << 1 | (self.regs.get_carry_flag() as u8);
                self.regs.set_carry_flag(prev_a >> 7 == 1);
                next_instr = true;
            }
            (0, 0x17, 4..) => unreachable!(),
            // JR d
            (0, 0x18, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x18, 4) => self.incr_pc(),
            (0, 0x18, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x18, 6..=9) => (),
            (0, 0x18, 10) => {
                let base = self.regs.pc.wrapping_sub(2) as i16;
                let d = self.get_immediate_d() as i16;
                let new_addr = base.wrapping_add(d);
                self.regs.pc = new_addr as u16;
            }
            (0, 0x18, 11) => next_instr = true,
            (0, 0x18, 12..) => unreachable!(),
            // LD A, (DE)
            (0, 0x1a, 3) => self.mem_read_start(self.regs.get_de()),
            (0, 0x1a, 4) => (),
            (0, 0x1a, 5) => self.regs.a = self.mem_read_result(),
            (0, 0x1a, 6) => next_instr = true,
            (0, 0x1a, 7..) => unreachable!(),
            (0, 0x1f, 3) => {
                let prev_a = self.regs.a;
                self.regs.a = prev_a >> 1 | ((self.regs.get_carry_flag() as u8) << 7);
                self.regs.set_carry_flag((prev_a & 1) == 1);
                next_instr = true;
            }
            (0, 0x1f, 4..) => unreachable!(),
            (0, 0x20 | 0x28 | 0x30 | 0x38, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x20 | 0x28 | 0x30 | 0x38, 4) => self.incr_pc(),
            (0, 0x20 | 0x28 | 0x30 | 0x38, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x20 | 0x28 | 0x30 | 0x38, 6) => {
                let cc = TABLE_CC[y as usize - 4];
                if !self.check_cc(cc) {
                    next_instr = true;
                }
            }
            (0, 0x20 | 0x28 | 0x30 | 0x38, 7..=9) => (),
            (0, 0x20 | 0x28 | 0x30 | 0x38, 10) => {
                let base = self.regs.pc as i16;
                let d = self.get_immediate_d() as i16;
                let new_addr = base.wrapping_add(d);
                self.regs.pc = new_addr as u16;
            }
            (0, 0x20 | 0x28 | 0x30 | 0x38, 11) => next_instr = true,
            (0, 0x20 | 0x28 | 0x30 | 0x38, 12..) => unreachable!(),
            // GameBoy change: LD (HL++), A
            //
            // Save A at (HL), then increment HL
            (0, 0x22, 3) => self.mem_write_start(self.regs.get_hl(), self.regs.a),
            (0, 0x22, 4) => (),
            (0, 0x22, 5) => {
                let value = self.regs.get_hl();
                let value = value.wrapping_add(1);
                self.regs.set_hl(value);
            }
            (0, 0x22, 6) => next_instr = true,
            (0, 0x22, 7..) => unreachable!(),
            // DAA
            (0, 0x27, 3) => {
                let n = self.regs.get_subtract_flag();
                let c = self.regs.get_carry_flag();
                let h = self.regs.get_half_carry_flag();
                let high = (self.regs.a >> 4) & 0xf;
                let low = self.regs.a & 0xf;
                let (to_add, new_carry) = match (n, c, high, h, low) {
                    (false, false, 0x0..=0x9, false, 0x0..=0x9) => (0x00, false),
                    (false, false, 0x0..=0x8, false, 0xa..=0xf) => (0x06, false),
                    (false, false, 0x0..=0x9, true, 0x0..=0x3) => (0x06, false),
                    (false, false, 0xa..=0xf, false, 0x0..=0x9) => (0x60, true),
                    (false, false, 0x9..=0xf, false, 0xa..=0xf) => (0x66, true),
                    (false, false, 0xa..=0xf, true, 0x0..=0x3) => (0x66, true),
                    (false, true, 0x0..=0x2, false, 0x0..=0x9) => (0x60, true),
                    (false, true, 0x0..=0x2, false, 0xa..=0xf) => (0x66, true),
                    (false, true, 0x0..=0x3, true, 0x0..=0x3) => (0x66, true),
                    (true, false, 0x0..=0x9, false, 0x0..=0x9) => (0x00, false),
                    (true, false, 0x0..=0x8, true, 0x6..=0xf) => (0xfa, false),
                    (true, true, 0x7..=0xf, false, 0x0..=0x9) => (0xa0, true),
                    (true, true, 0x6..=0xf, true, 0x6..=0xf) => (0x9a, true),
                    (_, _, 0x10.., _, _) => unreachable!(),
                    (_, _, _, _, 0x10..) => unreachable!(),
                    _ => (0x00, false),
                };
                self.regs.a = self.regs.a.wrapping_add(to_add);
                self.regs.set_carry_flag(new_carry);
                self.regs.set_zero_flag(self.regs.a == 0);
                self.regs.set_zero_flag(self.regs.a % 2 == 0);
                next_instr = true;
            }
            (0, 0x27, 4..) => unreachable!(),
            // GameBoy change: LD A, (HL++)
            //
            // Load A from (HL) and increment HL
            (0, 0x2a, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x2a, 4) => self.regs.set_hl(self.regs.get_hl().wrapping_add(1)),
            (0, 0x2a, 5) => self.regs.a = self.mem_read_result(),
            (0, 0x2a, 6) => next_instr = true,
            (0, 0x2a, 7..) => unreachable!(),
            // CPL
            (0, 0x2f, 3) => {
                self.regs.a = !self.regs.a;
                next_instr = true;
            }
            (0, 0x2f, 4..) => unreachable!(),
            // GameBoy change: LD (HL--), A
            //
            // Save A at (HL) and decrement HL
            (0, 0x32, 3) => self.mem_write_start(self.regs.get_hl(), self.regs.a),
            (0, 0x32, 4) => (),
            (0, 0x32, 5) => (),
            (0, 0x32, 6) => {
                self.regs.set_hl(self.regs.get_hl().wrapping_sub(1));
                next_instr = true;
            }
            (0, 0x32, 7..) => unreachable!(),
            // INC (HL)
            (0, 0x34, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x34, 4) => (),
            (0, 0x34, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x34, 6) => (),
            (0, 0x34, 7) => self.mem_write_start(self.regs.get_hl(), self.get_immediate_n().wrapping_add(1)),
            (0, 0x34, 8) => (),
            (0, 0x34, 9) => (),
            (0, 0x34, 10) => next_instr = true,
            (0, 0x34, 11..) => unreachable!(),
            // DEC (HL)
            (0, 0x35, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x35, 4) => (),
            (0, 0x35, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x35, 6) => (),
            (0, 0x35, 7) => self.mem_write_start(self.regs.get_hl(), self.get_immediate_n().wrapping_sub(1)),
            (0, 0x35, 8) => (),
            (0, 0x35, 9) => (),
            (0, 0x35, 10) => next_instr = true,
            (0, 0x35, 11..) => unreachable!(),
            // LD (HL), n
            (0, 0x36, 3) => self.mem_read_start(self.regs.pc),
            (0, 0x36, 4) => (),
            (0, 0x36, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x36, 6) => self.mem_write_start(self.regs.get_hl(), self.get_immediate_n()),
            (0, 0x36, 7) => (),
            (0, 0x36, 8) => (),
            (0, 0x36, 9) => next_instr = true,
            (0, 0x36, 10..) => unreachable!(),
            // SCF
            (0, 0x37, 3) => {
                self.regs.set_carry_flag(true);
                next_instr = true;
            }
            (0, 0x37, 4..) => unreachable!(),
            // GameBoy change: LD A, (HL--)
            //
            // Load A from (HL) and decrement HL
            (0, 0x3a, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x3a, 4) => self.regs.set_hl(self.regs.get_hl().wrapping_sub(1)),
            (0, 0x3a, 5) => self.regs.a = self.mem_read_result(),
            (0, 0x3a, 6) => next_instr = true,
            (0, 0x3a, 7..) => unreachable!(),
            // SCF
            (0, 0x3f, 3) => {
                let prev = self.regs.get_carry_flag();
                self.regs.set_carry_flag(!prev);
                next_instr = true;
            }
            (0, 0x3f, 4..) => unreachable!(),
            // LD r, (HL)
            (0, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e, 4) => (),
            (0, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e, 5) => {
                let r = TABLE_R[y as usize];
                self.set_r(r, self.mem_read_result());
            }
            (0, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e, 6) => next_instr = true,
            (0, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e, 7..) => unreachable!(),
            // HALT
            (0, 0x76, 3) => {
                self.mode = Mode::Halt { sleep_timer: None };
                next_instr = true;
            },
            // LD (HL), r
            (0, 0x70..=0x75 | 0x77, 3) => {
                let r = TABLE_R[z as usize];
                let value = self.get_r(r);
                self.mem_write_start(self.regs.get_hl(), value);
            }
            (0, 0x70..=0x75 | 0x77, 4) => (),
            (0, 0x70..=0x75 | 0x77, 5) => (),
            (0, 0x70..=0x75 | 0x77, 6) => next_instr = true,
            (0, 0x70..=0x75 | 0x77, 7..) => unreachable!(),
            // LD r, r
            (0, 0x40..=0x7f, 3) => {
                let r_1 = TABLE_R[y as usize];
                let r_2 = TABLE_R[z as usize];
                let value = self.get_r(r_2);
                self.set_r(r_1, value);
                next_instr = true;
            }
            (0, 0x40..=0x7f, 4..) => unreachable!(),
            // ALU operations on (HL)
            (0, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 3) => self.mem_read_start(self.regs.get_hl()),
            (0, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 4) => (),
            (0, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 6) => {
                let alu_op = TABLE_ALU[y as usize];
                self.do_alu(alu_op, self.get_immediate_n());
                next_instr = true;
            },
            (0, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 7..) => (),
            // ALU operations
            (0, 0x80..=0xbf, 3) => {
                let alu_op = TABLE_ALU[y as usize];
                let r = TABLE_R[z as usize];
                let r_value = self.get_r(r);
                self.do_alu(alu_op, r_value);
                next_instr = true;
            },
            (0, 0x80..=0xbf, 4..) => unreachable!(),
            // RET cc (11/5 cycles)
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 3) => (),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 4) => {
                let cc = TABLE_CC[y as usize];
                if !self.check_cc(cc) {
                    next_instr = true;
                }
            }
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 5) => self.mem_read_start(self.regs.sp),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 6) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 7) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 8) => self.mem_read_start(self.regs.sp),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 9) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 10) => {
                self.mem_read_finish_imm_2byte_second();
                let nn = self.get_immediate_nn();
                self.regs.pc = nn;
                next_instr = true;
            }
            (0, 0xc0 | 0xc8 | 0xd0 | 0xd8, 11..) => unreachable!(),
            // POP rp2 (10 cycles)
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 3) => self.mem_read_start(self.regs.sp),
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 4) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc1, 5) => self.regs.b = self.mem_read_result(),
            (0, 0xd1, 5) => self.regs.d = self.mem_read_result(),
            (0, 0xe1, 5) => self.regs.h = self.mem_read_result(),
            (0, 0xf1, 5) => self.regs.a = self.mem_read_result(),
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 6) => self.mem_read_start(self.regs.sp),
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 7) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc1, 8) => self.regs.c = self.mem_read_result(),
            (0, 0xd1, 8) => self.regs.e = self.mem_read_result(),
            (0, 0xe1, 8) => self.regs.l = self.mem_read_result(),
            (0, 0xf1, 8) => self.regs.set_flags(self.mem_read_result()),
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 9) => next_instr = true,
            (0, 0xc1 | 0xd1 | 0xe1 | 0xf1, 10..) => unreachable!(),
            // JP cc, nn (10 cycles)
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 4) => self.incr_pc(),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 7) => self.incr_pc(),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xc2 | 0xca | 0xd2 | 0xda, 9) => {
                let cc = TABLE_CC[y as usize];
                let dest = self.get_immediate_nn();
                if self.check_cc(cc) {
                    self.regs.pc = dest;
                }
                next_instr = true;
            }
            (0, 0xc2 | 0xca | 0xd2 | 0xda | 0xe2, 10..) => unreachable!(),
            // JP cc, nn (10 cycles)
            (0, 0xc3, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xc3, 4) => self.incr_pc(),
            (0, 0xc3, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xc3, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xc3, 7) => self.incr_pc(),
            (0, 0xc3, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xc3, 9) => {
                let dest = self.get_immediate_nn();
                self.regs.pc = dest;
                next_instr = true;
            }
            (0, 0xc3, 10..) => unreachable!(),
            // CALL cc, nn (17/10 cycles)
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 4) => self.incr_pc(),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 7) => self.incr_pc(),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 9) => {
                let cc = TABLE_CC[y as usize];
                if !self.check_cc(cc) {
                    next_instr = true;
                } else {
                    self.regs.sp = self.regs.sp.wrapping_sub(1);
                }
            }
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 10) => self.mem_write_start(self.regs.sp, (self.regs.pc >> 8) as u8),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 11) => (),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 12) => (),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 13) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 14) => self.mem_write_start(self.regs.sp, self.regs.pc as u8),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 15) => (),
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 16) => {
                self.regs.pc = self.get_immediate_nn();
                next_instr = true;
            }
            (0, 0xc4 | 0xcc | 0xd4 | 0xdc, 17..) => unreachable!(),
            // PUSH rp2
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 3) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 4) => self.mem_write_addr(self.regs.sp),
            (0, 0xc5, 5) => self.mem_write_data(self.regs.b),
            (0, 0xd5, 5) => self.mem_write_data(self.regs.d),
            (0, 0xe5, 5) => self.mem_write_data(self.regs.h),
            (0, 0xf5, 5) => self.mem_write_data(self.regs.a),
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 6) => (),
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 7) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 8) => self.mem_write_addr(self.regs.sp),
            (0, 0xc5, 9) => self.mem_write_data(self.regs.b),
            (0, 0xd5, 9) => self.mem_write_data(self.regs.d),
            (0, 0xe5, 9) => self.mem_write_data(self.regs.h),
            (0, 0xf5, 9) => self.mem_write_data(self.regs.a),
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 10) => {
                ();
                next_instr = true;
            }
            (0, 0xc5 | 0xd5 | 0xe5 | 0xf5, 11..) => unreachable!(),
            // alu n
            (0, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 4) => self.incr_pc(),
            (0, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 6) => {
                let alu_op = TABLE_ALU[y as usize];
                let n = self.get_immediate_n();
                self.do_alu(alu_op, n);
                next_instr = true;
            }
            (0, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 7..) => unreachable!(),
            // RST xx
            //
            // Push PC to the stack, then set PC to y*8.
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 3) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 4) => self.mem_write_start(self.regs.sp, (self.regs.pc >> 8) as u8),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 5) => (),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 6) => (),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 7) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 8) => self.mem_write_start(self.regs.sp, self.regs.pc as u8),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 9) => (),
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 10) => {
                ();
                self.regs.pc = y as u16 * 8;
                next_instr = true;
            }
            (0, 0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff, 11..) => unreachable!(),
            // RET
            (0, 0xc9, 3) => self.mem_read_start(self.regs.sp),
            (0, 0xc9, 4) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc9, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xc9, 6) => self.mem_read_start(self.regs.sp),
            (0, 0xc9, 7) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xc9, 8) => {
                self.mem_read_finish_imm_2byte_second();
                let nn = self.get_immediate_nn();
                self.regs.pc = nn;
            }
            (0, 0xc9, 9) => next_instr = true,
            (0, 0xc9, 10..) => unreachable!(),
            // CB prefix
            (0, 0xcb, 3) => self.opcode = 0xcb_00_03,
            (0, 0xcb, 4..) => unreachable!(),
            // CALL nn
            (0, 0xcd, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xcd, 4) => self.incr_pc(),
            (0, 0xcd, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xcd, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xcd, 7) => self.incr_pc(),
            (0, 0xcd, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xcd, 9) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xcd, 10) => self.mem_write_start(self.regs.sp, (self.regs.pc >> 8) as u8),
            (0, 0xcd, 11) => (),
            (0, 0xcd, 12) => (),
            (0, 0xcd, 13) => self.regs.sp = self.regs.sp.wrapping_sub(1),
            (0, 0xcd, 14) => self.mem_write_start(self.regs.sp, self.regs.pc as u8),
            (0, 0xcd, 15) => (),
            (0, 0xcd, 16) => {
                self.regs.pc = self.get_immediate_nn();
                next_instr = true;
            }
            (0, 0xcd, 17..) => unreachable!(),
            // GameBoy change: no-op (was OUTA (n))
            (0, 0xd3, 3..) => panic!("Tried to execute invalid opcode D3"),
            // GameBoy change: RETI
            //
            // Enable interrupts and return.
            (0, 0xd9, 3) => self.mem_read_start(self.regs.sp),
            (0, 0xd9, 4) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xd9, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xd9, 6) => self.mem_read_start(self.regs.sp),
            (0, 0xd9, 7) => self.regs.sp = self.regs.sp.wrapping_add(1),
            (0, 0xd9, 8) => {
                self.mem_read_finish_imm_2byte_second();
                let nn = self.get_immediate_nn();
                self.regs.pc = nn;
            }
            (0, 0xd9, 9) => next_instr = true,
            (0, 0xd9, 10..) => unreachable!(),
            // GameBoy change: no-op (was INA (n))
            (0, 0xdb, 3..) => panic!("Tried to execute invalid opcode DB"),
            // GameBoy change: no-op (was DD prefix)
            (0, 0xdd, 3..) => panic!("Tried to execute invalid opcode DD"),
            // GameBoy change: LD (n), A
            //
            // Save A at (FF00 + n)
            (0, 0xe0, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xe0, 4) => self.incr_pc(),
            (0, 0xe0, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0xe0, 6) => self.mem_write_start(0xff00 + self.get_immediate_n() as u16, self.regs.a),
            (0, 0xe0, 7) => (),
            (0, 0xe0, 8) => next_instr = true,
            (0, 0xe0, 9..) => unreachable!(),
            // GameBoy change: LD (C), A
            //
            // Save A at (FF00 + C)
            (0, 0xe2, 3) => self.mem_write_start(0xff00 + self.regs.c as u16, self.regs.a),
            (0, 0xe2, 4) => (),
            (0, 0xe2, 5) => next_instr = true,
            (0, 0xe2, 6..) => unreachable!(),
            // GameBoy change: no-op (was EX HL, (SP))
            (0, 0xe3, 3..) => panic!("Tried to execute invalid opcode E3"),
            // GameBoy change: no-op (was CALL PO, nn)
            (0, 0xe4, 3..) => panic!("Tried to execute invalid opcode E4"),
            // GameBoy change: ADD SP, d
            //
            // Add signed offset to SP
            (0, 0xe8, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xe8, 4) => self.incr_pc(),
            (0, 0xe8, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0xe8, 6) => self.regs.pc = self.regs.pc.wrapping_add(self.get_immediate_n() as i8 as i16 as u16),
            (0, 0xe8, 7) => next_instr = true,
            (0, 0xe8, 8..) => unreachable!(),
            // JP HL
            (0, 0xe9, 3) => {
                self.regs.pc = self.regs.get_hl();
                next_instr = true;
            }
            (0, 0xe9, 4..) => unreachable!(),
            // GameBoy change: LD (nn), A
            //
            // Save A at given address
            (0, 0xea, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xea, 4) => self.incr_pc(),
            (0, 0xea, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xea, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xea, 7) => self.incr_pc(),
            (0, 0xea, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xea, 9) => self.mem_write_start(self.get_immediate_nn(), self.regs.a),
            (0, 0xea, 10) => (),
            (0, 0xea, 11) => next_instr = true,
            (0, 0xea, 12..) => unreachable!(),
            // GameBoy change: no-op (was EX DE, HL)
            (0, 0xeb, 3..) => panic!("Tried to execute invalid opcode EB"),
            // GameBoy change: no-op (was CALL PE, nn)
            (0, 0xec, 3..) => panic!("Tried to execute invalid opcode EC"),
            // ED prefix
            (0, 0xed, 3) => self.opcode = 0xed_00_03,
            (0, 0xed, 4..) => unreachable!(),
            // GameBoy change: LD A, (FF00+n)
            //
            // Load A from (FF00 + n)
            (0, 0xf0, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xf0, 4) => self.incr_pc(),
            (0, 0xf0, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0xf0, 6) => self.mem_read_start(0xff00 + (self.get_immediate_n() as u16)),
            (0, 0xf0, 7) => (),
            (0, 0xf0, 8) => self.regs.a = self.mem_read_result(),
            (0, 0xf0, 9) => next_instr = true,
            (0, 0xf0, 10..) => unreachable!(),
            // GameBoy change: LD A, (FF00+C)
            (0, 0xf2, 3) => self.mem_read_start(0xff00 + self.regs.c as u16),
            (0, 0xf2, 4) => (),
            (0, 0xf2, 5) => self.regs.a = self.mem_read_result(),
            (0, 0xf2, 6) => next_instr = true,
            (0, 0xf2, 7..) => unreachable!(),
            // DI -- disable interrupts
            (0, 0xf3, 3) => {
                self.interrupt_master_enable = false;
                next_instr = true;
            }
            (0, 0xf3, 4..) => unreachable!(),
            // GameBoy change: no-op (was CALL P, nn)
            (0, 0xf4, 3..) => panic!("Tried to execute invalid opcode F4"),
            // GameBoy change: LDHL SP, d
            //
            // Load HL with SP + d
            (0, 0xf8, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xf8, 4) => self.incr_pc(),
            (0, 0xf8, 5) => self.mem_read_finish_imm_1byte(),
            (0, 0xf8, 6) => {
                let sp = self.regs.sp as i16;
                let value = sp.wrapping_add(self.get_immediate_d() as i16);
                self.regs.set_hl(value as u16);
                next_instr = true;
            }
            (0, 0xf8, 7..) => unreachable!(),
            // LD SP, HL
            (0, 0xf9, 3) => self.regs.sp = self.regs.get_hl(),
            (0, 0xf9, 4) => next_instr = true,
            (0, 0xf9, 5..) => unreachable!(),
            // GameBoy change: LD A, (nn)
            //
            // Load A from a given address
            (0, 0xfa, 3) => self.mem_read_start(self.regs.pc),
            (0, 0xfa, 4) => self.incr_pc(),
            (0, 0xfa, 5) => self.mem_read_finish_imm_2byte_first(),
            (0, 0xfa, 6) => self.mem_read_start(self.regs.pc),
            (0, 0xfa, 7) => self.incr_pc(),
            (0, 0xfa, 8) => self.mem_read_finish_imm_2byte_second(),
            (0, 0xfa, 9) => self.mem_read_start(self.get_immediate_nn()),
            (0, 0xfa, 10) => (),
            (0, 0xfa, 11) => {
                self.regs.a = self.mem_read_result();
                next_instr = true;
            }
            (0, 0xfa, 12..) => unreachable!(),
            // EI
            (0, 0xfb, 3) => {
                self.interrupt_master_enable = true;
                next_instr = true;
            }
            (0, 0xfb, 4..) => unreachable!(),
            // GameBoy change: no-op (was CALL M, nn)
            (0, 0xfc, 3..) => panic!("Tried to execute invalid opcode FC"),
            // GameBoy change: no-op (was FD prefix)
            (0, 0xfd, 3..) => panic!("Tried to execute invalid opcode FD"),

            // (0, _, _) => todo!("Unhandled opcode {:#06x} at tick {}", ((prefix as u16) << 8) | (opcode as u16), tick),
            (0xcb, _, 0..=3) => unreachable!(),
            // rot (HL) - 15 cycles
            (0xcb, 0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x36 | 0x3e, 7..) => todo!("{:?} (HL)", TABLE_ROT[y as usize]),
            // rot r - 8 cycles
            (0xcb, 0x00..=0x3f, 7) => {
                let r = TABLE_R[z as usize];
                let prev_value = self.get_r(r);
                match TABLE_ROT[y as usize] {
                    Rot::RLC => {
                        let top_bit = (prev_value >> 7) & 1;
                        self.regs.set_carry_flag(top_bit != 0);
                        let new_value = prev_value << 1 | top_bit;
                        self.set_r(r, new_value);
                    }
                    Rot::RRC => {
                        let low_bit = prev_value & 1;
                        self.regs.set_carry_flag(low_bit != 0);
                        let new_value = (low_bit << 7) | (prev_value >> 1);
                        self.set_r(r, new_value);
                    }
                    Rot::RL => {
                        let top_bit = (prev_value >> 7) & 1;
                        let prev_carry = if self.regs.get_carry_flag() { 1 } else { 0 };
                        let new_value = prev_value << 1 | prev_carry;
                        self.regs.set_carry_flag(top_bit != 0);
                        self.set_r(r, new_value);
                    }
                    Rot::RR => {
                        let low_bit = prev_value & 1;
                        let prev_carry = if self.regs.get_carry_flag() { 1 } else { 0 };
                        let new_value = (prev_carry << 7) | (prev_value >> 1);
                        self.regs.set_carry_flag(low_bit != 0);
                        self.set_r(r, new_value);
                    }
                    Rot::SLA => todo!(),
                    Rot::SRA => todo!(),
                    Rot::SLL => todo!(),
                    Rot::SRL => todo!(),
                }
                self.regs.set_subtract_flag(false);
                self.regs.set_half_carry_flag(false);
                next_instr = true;
            }
            (0xcb, 0x00..=0x3f, 8..) => unreachable!(),
            // BIT y, (HL)
            (0xcb, 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x76 | 0x7e, 3..) => todo!("BIT {}, (HL)", y),
            // BIT y, r
            (0xcb, 0x40..=0x7f, 7) => {
                let r = TABLE_R[z as usize];
                let prev_value = self.get_r(r);
                let shift = y as u32;
                let bit = (prev_value >> shift) & 1;
                self.regs.set_subtract_flag(false);
                self.regs.set_half_carry_flag(true);
                self.regs.set_zero_flag(bit == 0);
                next_instr = true;
            }
            (0xcb, 0x40..=0x7f, 8..) => unreachable!(),
            // RES y, (HL)
            (0xcb, 0x86 | 0x8e | 0x96 | 0x9e | 0xa6 | 0xae | 0xb6 | 0xbe, 3..) => todo!("RES {}, (HL)", y),
            // RES y, r
            (0xcb, 0x80..=0xbf, 3..) => todo!("RES {}, {:?}", y, TABLE_R[z as usize]),
            // SET y, (HL)
            (0xcb, 0xc6 | 0xce | 0xd6 | 0xde | 0xe6 | 0xee | 0xf6 | 0xfe, 3..) => todo!("SET {}, (HL)", y),
            // SET y, r
            (0xcb, 0xc0..=0xff, 3..) => todo!("SET {}, {:?}", y, TABLE_R[z as usize]),
            (0xed, _, _) => todo!("ED-prefixed opcodes"),
            (_, _, 0x10..) => unreachable!("Instruction ticks are less than {}", 0x10),
            (0x01..=0xca | 0xcc..=0xec | 0xee.., _, _) => panic!("Got a bad prefix byte {:#04x}", prefix),
        }
        if next_instr {
            self.opcode = 0;
        } else {
            self.opcode += 1;
        }

        self.do_dma();
        self.ppu.step_cycle(pins);

        self.handle_rw_out::<false>(pins);
        self.handle_rw_out::<true>(pins);

        trace!("Finished CPU tick, pins = {:?}", pins);
        trace!("Finished CPU tick, registers = {:?}", self.regs);
    }

    fn handle_interrupts(&mut self, pins: &mut SM83Pinout) {
        // If any interrupts are raised and we are halted, switch mode to running.
        // If any of the buttons are pressed while stopped, switch mode to running.
        if let Mode::Stop = self.mode {
            let any_buttons_pressed =
                pins.get_p00__opt() == Some(false)
                || pins.get_p01__opt() == Some(false)
                || pins.get_p02__opt() == Some(false)
                || pins.get_p03__opt() == Some(false)
                || pins.get_p10__opt() == Some(false)
                || pins.get_p11__opt() == Some(false)
                || pins.get_p12__opt() == Some(false)
                || pins.get_p13__opt() == Some(false);
            if any_buttons_pressed {
                self.mode = Mode::Run;
            }
        }
    }

    fn do_dma(&mut self) {
        let dma = match self.dma.as_ref() {
            Some(d) => *d,
            None => return,
        };
        trace!("Working on DMA transfer: {:?}", dma);
        let index = dma.cycle_counter / 4;
        match dma.cycle_counter % 4 {
            0 => {
                let addr: u16 = dma.base_address + index;
                self.mem_read_start_dma(addr);
            }
            1 => (),
            2 => {
                let data = self.mem_read_result_dma();
                self.ppu.oam_write_dma(index.try_into().unwrap(), data);
            }
            3 => (),
            _ => unreachable!(),
        }
        // We have to re-load this reference because we call methods on `self`,
        // and that conflicts with the borrow on `self.dma`.
        let mut dma = self.dma.as_mut().unwrap();
        dma.cycle_counter += 1;
        if dma.cycle_counter == 160 * 4 {
            self.dma = None;
        }
    }

    fn handle_rw_in<const FOR_DMA: bool>(&mut self, pins: &mut SM83Pinout) {
        use MemoryMappedAddr as MMA;

        let dma_active = self.dma.is_some();
        let state_ref: &mut RWState = match (FOR_DMA, self.dma.as_mut()) {
            (true, Some(dma)) => {
                trace!("SM86::handle_rw_out DMA rw_state = {:?}", dma.rw_state);
                &mut dma.rw_state
            }
            (true, None) => return,
            (false, _) => {
                trace!("SM86::handle_rw_out rw_state = {:?}", self.rw_state);
                &mut self.rw_state
            }
        };

        *state_ref = match *state_ref {
            RWState::None => RWState::None,
            RWState::ReadingAddr { addr } => RWState::ReadingWait { addr },
            RWState::ReadingWait { addr } if dma_active && !FOR_DMA => {
                let data = match addr {
                    MMA::HighRAM { mapped } => {
                        let data = self.high_ram[mapped as usize];
                        trace!("CPU read of high RAM {:#04x} @ {:#04x}", data, mapped);
                        data
                    },
                    _ => panic!("Cannot read from {:?} while DMA is active", addr),
                };
                RWState::ReadingDone { addr, data }
            }
            RWState::ReadingWait { addr } => {
                let data = match addr {
                    MMA::Cart { mapped } => {
                        let data = pins.cart_read_finish();
                        trace!("CPU read {:#04x} @ {:#06x} from cart", data, mapped);
                        data
                    }
                    MMA::VideoRAM { mapped } => {
                        let data = self.ppu.vram_read(mapped);
                        trace!("CPU read {:#04x} @ {:06x} from LCD Controller VRAM", data, mapped);
                        data
                    }
                    MMA::WorkRAM { mapped } => {
                        let data = pins.wram_read_finish();
                        trace!("CPU read {:#04x} @ {:#06x} from Work RAM", data, mapped);
                        data
                    }
                    MMA::OAM { .. } if FOR_DMA => panic!("DMA can't read from OAM"),
                    MMA::OAM { mapped } => {
                        let data = self.ppu.oam_read(mapped);
                        trace!("CPU read {:#04x} @ {:06x} from LCD Controller OAM", data, mapped);
                        data
                    }
                    MMA::ForbiddenRange { .. } if FOR_DMA => panic!("DMA can't read from forbidden memory range"),
                    MMA::ForbiddenRange { raw } => {
                        // According to <https://gbdev.io/pandocs/Memory_Map.html#fea0-feff-range>,
                        // this can do any number of things. We'll pick the latest
                        // hardware versions, which return the high nibble of the
                        // lower address byte twice.
                        let nibble = (raw >> 4) as u8 & 0xf as u8;
                        (nibble << 4) | nibble
                    }
                    MMA::IO { .. } if FOR_DMA => panic!("DMA can't read from IO registers"),
                    MMA::IO { mapped } =>
                        // I'm not sure if read-only fields are expected to crash on
                        // read, or if they return a fixed value instead.
                        match mapped {
                            0x00 => if self.joypad_select_action_buttons {
                                ((!pins.get_p10__opt().unwrap_or(true) as u8) << 0)
                                    | ((!pins.get_p11__opt().unwrap_or(true) as u8) << 1)
                                    | ((!pins.get_p12__opt().unwrap_or(true) as u8) << 2)
                                    | ((!pins.get_p13__opt().unwrap_or(true) as u8) << 3)
                                    | (1 << 5)
                            } else {
                                ((!pins.get_p00__opt().unwrap_or(true) as u8) << 0)
                                    | ((!pins.get_p01__opt().unwrap_or(true) as u8) << 1)
                                    | ((!pins.get_p02__opt().unwrap_or(true) as u8) << 2)
                                    | ((!pins.get_p03__opt().unwrap_or(true) as u8) << 3)
                                    | (1 << 4)
                            },
                            0x01 => todo!("Read from Serial transfer data register"),
                            0x02 => todo!("Read from Serial transfer control register"),
                            0x04 => (self.div_counter >> 8) as u8,
                            0x05 => todo!("Read from Timer counter register"),
                            0x06 => todo!("Read from Timer modulo register"),
                            0x07 => todo!("Read from Timer control register"),
                            0x0f => self.interrupt_f_reg,
                            0x10 => todo!("Read from NR10 - Channel 1 Sweep register"),
                            0x11 => todo!("Read from NR11 - Channel 1 Sound length/Wave pattern duty"),
                            0x12 => todo!("Read from NR12 - Channel 1 Volume Envelope"),
                            0x13 => todo!("Read from NR12 - Channel 1 Frequency Lo"),
                            0x14 => todo!("Read from NR12 - Channel 1 Frequency Hi"),
                            0x16 => todo!("Read from NR21 - Channel 2 Sound length/Wave pattern duty"),
                            0x17 => todo!("Read from NR22 - Channel 2 Volume Envelope"),
                            0x18 => todo!("Read from NR23 - Channel 2 Frequency Lo"),
                            0x19 => todo!("Read from NR24 - Channel 2 Frequency Hi"),
                            0x1a => todo!("Read from NR30 - Channel 3 Sound on/off"),
                            0x1b => todo!("Read from NR31 - Channel 3 Sound Length"),
                            0x1c => todo!("Read from NR32 - Channel 3 Select output level"),
                            0x1d => todo!("Read from NR33 - Channel 3 Frequency Lo"),
                            0x1f => todo!("Read from NR34 - Channel 3 Frequency Hi"),
                            0x20 => todo!("Read from NR41 - Channel 4 Sound Length"),
                            0x21 => todo!("Read from NR42 - Channel 4 Volume Envelope"),
                            0x22 => todo!("Read from NR43 - Channel 4 Polynomial Counter"),
                            0x23 => todo!("Read from NR44 - Channel 4 Counter/consecutive; Initial"),
                            0x24 => todo!("Read from NR50 - Channel control / On-Off/ Volume"),
                            0x25 => todo!("Read from NR51 - Selection of Sound output terminal"),
                            0x26 => todo!("Read from NR52 - Sound on/off"),
                            0x30..=0x3f => todo!("Read from wave pattern RAM"),
                            0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x47 | 0x48
                                | 0x49 | 0x4a | 0x4b | 0x68 | 0x69 | 0x6a | 0x6b =>
                                self.ppu.read_register(mapped),
                            0x4d => {
                                let mut result = 0x00;
                                if self.speed_switch_requested {
                                    result |= 1;
                                }
                                if self.high_speed {
                                    result |= 1 << 7;
                                }
                                result
                            }
                            0x4f => todo!("Read from VRAM Bank register"),
                            0x51 => todo!("Read from HDMA1 register"),
                            0x52 => todo!("Read from HDMA2 register"),
                            0x53 => todo!("Read from HDMA3 register"),
                            0x54 => todo!("Read from HDMA4 register"),
                            0x55 => todo!("Read from HDMA5 register"),
                            0x56 => todo!("Read from RP register"),
                            0x6c => todo!("Read from OPRI register"),
                            0x70 => todo!("Read from WRAM Bank register"),
                            0x72 => todo!("Read from undocumented register FF72"),
                            0x74 => todo!("Read from undocumented register FF74"),
                            0xff => self.interrupt_enable,
                            _ => todo!("Tried to read from unimplemented IO register {:#06x}!", 0xff00 + mapped as u16),
                        },
                    MMA::HighRAM { .. } if FOR_DMA => panic!("DMA can't read from High RAM"),
                    MMA::HighRAM { mapped } => {
                        let data = self.high_ram[mapped as usize];
                        trace!("CPU read of high RAM {:#04x} @ {:#04x}", data, mapped);
                        data
                    },
                    MMA::InterruptEnableRegister if FOR_DMA => panic!("DMA can't read from IE register"),
                    MMA::InterruptEnableRegister => {
                        trace!("CPU read of IE register with value {:#04x}", self.interrupt_enable);
                        self.interrupt_enable
                    },
                    _ => todo!("ReadingWait addr = {:?}", addr),
                };
                RWState::ReadingDone { addr, data }
            }
            RWState::ReadingDone { .. } => RWState::None,
            RWState::WritingAddrNoData { addr } => RWState::WritingAddrNoData { addr },
            RWState::WritingAddrWithData { addr, data } => RWState::WritingData { addr, data },
            RWState::WritingData { addr, data } => {
                match addr {
                    MMA::Cart { .. } => pins.cart_write_finish(),
                    MMA::VideoRAM { .. } => (),
                    MMA::WorkRAM { .. } => pins.wram_write_finish(),
                    MMA::OAM { .. } => (),
                    MMA::ForbiddenRange { .. } => (),
                    MMA::IO { .. } => (),
                    MMA::HighRAM { .. } => (),
                    MMA::InterruptEnableRegister => (),
                    _ => todo!("WritingData addr = {:?}", addr),
                }
                RWState::WritingDone { addr, data }
            }
            RWState::WritingDone { .. } => RWState::None,
        };
        trace!("SM86::handle_rw_in rw_state (after) = {:?}", state_ref);
    }

    fn handle_rw_out<const FOR_DMA: bool>(&mut self, pins: &mut SM83Pinout) {
        use MemoryMappedAddr as MMA;

        let state: RWState = match (FOR_DMA, self.dma.as_ref()) {
            (true, Some(dma)) => {
                trace!("SM86::handle_rw_out DMA rw_state = {:?}", dma.rw_state);
                dma.rw_state
            }
            (true, None) => return,
            (false, _) => {
                trace!("SM86::handle_rw_out rw_state = {:?}", self.rw_state);
                self.rw_state
            }
        };
        match state {
            RWState::None => (),
            RWState::ReadingAddr { addr } => match addr {
                MMA::Cart { mapped } => pins.cart_read_start(mapped),
                MMA::VideoRAM { mapped } => (),
                MMA::WorkRAM { mapped } => pins.wram_read_start(mapped),
                MMA::OAM { .. } => (),
                MMA::ForbiddenRange { .. } => (),
                MMA::IO { mapped } => (),
                MMA::HighRAM { .. } => (),
                MMA::InterruptEnableRegister => (),
                _ => todo!("ReadingAddr addr = {:?}", addr),
            },
            RWState::ReadingWait { .. } => pins.cart_rw_enable(true),
            RWState::ReadingDone { .. } => pins.cart_rw_enable(true),
            RWState::WritingAddrNoData { addr } => match addr {
                MMA::Cart { mapped } => pins.cart_write_addr(mapped),
                MMA::VideoRAM { mapped } => (),
                MMA::WorkRAM { mapped } => pins.wram_write_addr(mapped),
                MMA::OAM { .. } => (),
                MMA::HighRAM { .. } => (),
                MMA::InterruptEnableRegister => (),
                _ => todo!("WritingAddrNoData addr = {:?}", addr),
            }
            RWState::WritingAddrWithData { addr, data: _ } => match addr {
                MMA::Cart { mapped } => pins.cart_write_addr(mapped),
                MMA::VideoRAM { .. } => (),
                MMA::WorkRAM { mapped } => pins.wram_write_addr(mapped),
                MMA::OAM { mapped } => (),
                MMA::ForbiddenRange { .. } => (),
                MMA::IO { .. } => (),
                MMA::HighRAM { .. } => (),
                MMA::InterruptEnableRegister => (),
                _ => todo!("WritingAddrWithData addr = {:#06x?}", addr),
            }
            RWState::WritingData { addr, data } => {
                trace!("CPU (at {:#06x}) writing data at {:04x?}: {:#04x}", self.regs.pc, addr, data);
                match addr {
                    MMA::Cart { .. } => pins.cart_write_data(data),
                    MMA::VideoRAM { mapped } => self.ppu.vram_write(mapped, data),
                    MMA::WorkRAM { .. } => pins.wram_write_data(data),
                    MMA::OAM { mapped } => self.ppu.oam_write(mapped, data),
                    MMA::ForbiddenRange { .. } => (),
                    MMA::IO { mapped } => match mapped {
                        0x00 => if data & (1 << 5) != 0 {
                            self.joypad_select_action_buttons = true;
                        } else if data & (1 << 4) != 0 {
                            self.joypad_select_action_buttons = true;
                        },
                        0x01 => todo!("Write to Serial transfer data register"),
                        0x02 => todo!("Write to Serial transfer control register"),
                        0x04 => self.div_counter = (data as u16) << 8,
                        0x05 => todo!("Write to Timer counter register"),
                        0x06 => todo!("Write to Timer modulo register"),
                        0x07 => todo!("Write to Timer control register"),
                        0x0f => self.interrupt_f_reg = data & 0x1f,
                        0x10 => todo!("Write to NR10 - Channel 1 Sweep register"),
                        0x11 => todo!("Write to NR11 - Channel 1 Sound length/Wave pattern duty"),
                        0x12 => todo!("Write to NR12 - Channel 1 Volume Envelope"),
                        0x13 => todo!("Write to NR12 - Channel 1 Frequency Lo"),
                        0x14 => todo!("Write to NR12 - Channel 1 Frequency Hi"),
                        0x16 => todo!("Write to NR21 - Channel 2 Sound length/Wave pattern duty"),
                        0x17 => todo!("Write to NR22 - Channel 2 Volume Envelope"),
                        0x18 => todo!("Write to NR23 - Channel 2 Frequency Lo"),
                        0x19 => todo!("Write to NR24 - Channel 2 Frequency Hi"),
                        0x1a => todo!("Write to NR30 - Channel 3 Sound on/off"),
                        0x1b => todo!("Write to NR31 - Channel 3 Sound Length"),
                        0x1c => todo!("Write to NR32 - Channel 3 Select output level"),
                        0x1d => todo!("Write to NR33 - Channel 3 Frequency Lo"),
                        0x1f => todo!("Write to NR34 - Channel 3 Frequency Hi"),
                        0x20 => todo!("Write to NR41 - Channel 4 Sound Length"),
                        0x21 => todo!("Write to NR42 - Channel 4 Volume Envelope"),
                        0x22 => todo!("Write to NR43 - Channel 4 Polynomial Counter"),
                        0x23 => todo!("Write to NR44 - Channel 4 Counter/consecutive; Initial"),
                        0x24 => todo!("Write to NR50 - Channel control / On-Off/ Volume"),
                        0x25 => todo!("Write to NR51 - Selection of Sound output terminal"),
                        0x26 => warn!("Ignoring write to sound reg NR52 (Sound on/off)"),
                        0x30..=0x3f => todo!("Write to wave pattern RAM"),
                        0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x47 | 0x48
                            | 0x49 | 0x4a | 0x4b | 0x68 | 0x69 | 0x6a | 0x6b =>
                            self.ppu.write_register(mapped, data),
                        0x46 => self.dma = Some(DMATransfer::new(data)),
                        0x4d => {
                            // Speed switch request
                            let high_speed_requested = (data & 1) == 1;
                            if self.high_speed != high_speed_requested {
                                self.speed_switch_requested = true;
                            }
                        }
                        0x4f => todo!("Write to VRAM Bank register"),
                        0x51 => todo!("Write to HDMA1 register"),
                        0x52 => todo!("Write to HDMA2 register"),
                        0x53 => todo!("Write to HDMA3 register"),
                        0x54 => todo!("Write to HDMA4 register"),
                        0x55 => todo!("Write to HDMA5 register"),
                        0x56 => todo!("Write to RP register"),
                        0x6c => todo!("Write to OPRI register"),
                        0x70 => todo!("Write to WRAM Bank register"),
                        0x72 => todo!("Write to undocumented register FF72"),
                        0x74 => todo!("Write to undocumented register FF74"),
                        0xff => self.interrupt_enable = data & 0x1f,
                        _ => todo!("Tried to write from unimplemented IO register {:#06x}!", 0xff00 + mapped as u16),
                    },
                    MMA::HighRAM { mapped } => self.high_ram[mapped as usize] = data,
                    MMA::InterruptEnableRegister => self.interrupt_enable = data & 0x1f,
                    _ => todo!("WritingData addr = {:#04x?}", addr),
                }
            }
            RWState::WritingDone { .. } => (),
        }
    }

    #[inline]
    fn incr_pc(&mut self) {
        trace!("Incrementing PC");
        self.regs.pc = self.regs.pc.wrapping_add(1);
    }

    /// This currently panics on `R::IndHL`
    #[inline(always)]
    fn get_r(&self, r: R) -> u8 {
        match r {
            R::B => self.regs.b,
            R::C => self.regs.c,
            R::D => self.regs.d,
            R::E => self.regs.e,
            R::H => self.regs.h,
            R::L => self.regs.l,
            R::IndHL => todo!("get_r IndHL"),
            R::A => self.regs.a,
        }
    }

    /// This currently panics on `R::IndHL`
    #[inline(always)]
    fn set_r(&mut self, r: R, value: u8) {
        trace!("Setting 8-bit register {:?} to value {:#06x}", r, value);
        self.regs.set_zero_flag(value == 0x00);
        match r {
            R::B => self.regs.b = value,
            R::C => self.regs.c = value,
            R::D => self.regs.d = value,
            R::E => self.regs.e = value,
            R::H => self.regs.h = value,
            R::L => self.regs.l = value,
            R::IndHL => todo!("set_r IndHL value={:#04x}", value),
            R::A => self.regs.a = value,
        }
    }

    #[inline(always)]
    fn get_rp(&self, r: Rp1) -> u16 {
        match r {
            Rp1::BC => self.regs.get_bc(),
            Rp1::DE => self.regs.get_de(),
            Rp1::HL => self.regs.get_hl(),
            Rp1::SP => self.regs.sp,
        }
    }

    #[inline(always)]
    fn set_rp(&mut self, r: Rp1, value: u16) {
        trace!("Setting 16-bit register {:?} to value {:#06x}", r, value);
        self.regs.set_zero_flag(value == 0x0000);
        match r {
            Rp1::BC => self.regs.set_bc(value),
            Rp1::DE => self.regs.set_de(value),
            Rp1::HL => self.regs.set_hl(value),
            Rp1::SP => self.regs.sp = value,
        }
    }

    // #[inline(always)]
    // fn get_rp2(&self, r: Rp2) -> u16 {
    //     match r {
    //         Rp2::BC => self.regs.get_bc(),
    //         Rp2::DE => self.regs.get_de(),
    //         Rp2::HL => self.regs.get_hl(),
    //         Rp2::AF => self.regs.get_af(),
    //     }
    // }

    // #[inline(always)]
    // fn set_rp2(&mut self, r: Rp2, value: u16) {
    //     match r {
    //         Rp2::BC => self.regs.set_bc(value),
    //         Rp2::DE => self.regs.set_de(value),
    //         Rp2::HL => self.regs.set_hl(value),
    //         Rp2::AF => self.regs.set_af(value),
    //     }
    // }

    #[inline]
    fn get_immediate_d(&self) -> i8 {
        let (n, _) = self.immediates.unwrap();
        n as i8
    }

    #[inline]
    fn get_immediate_n(&self) -> u8 {
        let (n, _) = self.immediates.unwrap();
        n
    }

    #[inline]
    fn get_immediate_nn(&self) -> u16 {
        let (n1, inner) = self.immediates.unwrap();
        let n2 = inner.unwrap();
        ((n2 as u16) << 8) | (n1 as u16)
    }

    #[inline(always)]
    fn mem_read_start(&mut self, addr: u16) {
        let addr = MemoryMappedAddr::map(addr);
        self.rw_state = RWState::ReadingAddr { addr };
    }

    #[inline(always)]
    fn mem_read_start_dma(&mut self, addr: u16) {
        let dma: &mut DMATransfer = self.dma.as_mut().unwrap();
        let addr = MemoryMappedAddr::map(addr);
        dma.rw_state = RWState::ReadingAddr { addr };
        trace!("Starting DMA read at {:?}: {:?}", addr, dma);
    }

    #[inline(always)]
    fn mem_write_start(&mut self, addr: u16, data: u8) {
        let addr = MemoryMappedAddr::map(addr);
        self.rw_state = RWState::WritingAddrWithData { addr, data };
    }

    #[inline(always)]
    fn mem_write_addr(&mut self, addr: u16) {
        let addr = MemoryMappedAddr::map(addr);
        self.rw_state = RWState::WritingAddrNoData { addr };
    }

    #[inline(always)]
    fn mem_write_data(&mut self, data: u8) {
        match self.rw_state {
            RWState::WritingAddrNoData { addr } => self.rw_state = RWState::WritingData { addr, data },
            other => panic!("Seperate address/data writes must be sequenced in that order (expected WritingAddrNoData {{ .. }}, found {:?})", other),
        }
    }

    #[inline(always)]
    fn mem_read_result(&self) -> u8 {
        match self.rw_state {
            RWState::ReadingDone { addr: _, data } => data,
            other => panic!("Reads must be done two cycles after the read is started! Expected ReadingDone {{ addr, data }}, found {:?}", other),
        }
    }

    #[inline(always)]
    fn mem_read_result_dma(&self) -> u8 {
        let dma = self.dma.as_ref().expect("mem_read_result_dma only works during DMA");
        match dma.rw_state {
            RWState::ReadingDone { addr: _, data } => data,
            other => panic!("Reads must be done two cycles after the read is started! Expected ReadingDone {{ addr, data }}, found {:?}", other),
        }
    }

    #[inline]
    fn mem_read_finish_imm_1byte(&mut self) {
        let new = self.mem_read_result();
        trace!("Setting only immediate byte to {:#04x}", new);
        self.immediates = Some((new, None));
    }

    #[inline]
    fn mem_read_finish_imm_2byte_first(&mut self) {
        let new = self.mem_read_result();
        trace!("Setting first immediate byte to {:#04x}", new);
        self.immediates = Some((new, None));
    }

    #[inline]
    fn mem_read_finish_imm_2byte_second(&mut self) {
        let new = self.mem_read_result();
        trace!("Setting second immediate byte to {:#04x}", new);
        let (first, _) = self.immediates.unwrap();
        self.immediates = Some((first, Some(new)));
    }

    fn check_cc(&self, cc: CC) -> bool {
        let result = match cc {
            CC::NZ => self.regs.get_zero_flag() == false,
            CC::Z => self.regs.get_zero_flag() == true,
            CC::NC => self.regs.get_carry_flag() == false,
            CC::C => self.regs.get_carry_flag() == true,
            CC::PO => panic!("Parity flag was removed in GB CPU"),
            CC::PE => panic!("Parity flag was removed in GB CPU"),
            CC::P => panic!("Sign flag was removed in GB CPU"),
            CC::M => panic!("Sign flag was removed in GB CPU"),
        };
        trace!("CPU is checking condition code {:?} = {:?}", cc, result);
        result
    }

    fn do_alu(&mut self, alu_op: ALU, value: u8) {
        match alu_op {
            ALU::ADD_A => self.regs.a = self.regs.a.wrapping_add(value),
            ALU::ADC_A => self.regs.a = self.regs.a.wrapping_add(value).wrapping_add(self.regs.get_carry_flag() as u8),
            ALU::SUB => {
                let result = self.regs.a.wrapping_sub(value);
                self.regs.set_carry_flag(self.regs.a < value);
                self.regs.set_zero_flag(result == 0);
                self.regs.set_subtract_flag(true);
                self.regs.set_half_carry_flag((self.regs.a >> 4) < (value >> 4));
                self.regs.a = result;
            }
            ALU::SBC_A => self.regs.a = self.regs.a.wrapping_sub(value).wrapping_sub(self.regs.get_carry_flag() as u8),
            ALU::AND => self.regs.a &= value,
            ALU::XOR => self.regs.a ^= value,
            ALU::OR => {
                self.regs.a |= value;
                self.regs.set_zero_flag(self.regs.a == 0);
                self.regs.set_carry_flag(false);
                self.regs.set_carry_flag(false);
                self.regs.set_half_carry_flag(false);
            }
            ALU::CP => {
                let result = self.regs.a.wrapping_sub(value);
                self.regs.set_carry_flag(self.regs.a < value);
                self.regs.set_zero_flag(result == 0);
                self.regs.set_subtract_flag(true);
                self.regs.set_half_carry_flag((self.regs.a >> 4) < (value >> 4));
            }
        }
    }
}

#[inline(never)]
fn display_opcode(prefix: u8, opcode: u8) -> impl fmt::Display {
    if prefix == 0x00 {
        match opcode {
            0x00 => "NOP",
            0x01 => "LD BC, nn",
            0x02 => "LD (BC), A",
            0x03 => "INC BC",
            0x04 => "INC B",
            0x05 => "DEC B",
            0x06 => "LD B, n",
            0x07 => "RLCA",
            0x08 => "LD (nn), SP",
            0x09 => "ADD HL, BC",
            0x0a => "LD A, (BC)",
            0x0b => "DEC BC",
            0x0c => "INC C",
            0x0d => "DEC C",
            0x0e => "LD C, n",
            0x0f => "RRCA",

            0x10 => "STOP",
            0x11 => "LD DE, nn",
            0x12 => "LD (DE), A",
            0x13 => "INC DE",
            0x14 => "INC D",
            0x15 => "DEC D",
            0x16 => "LD D, n",
            0x17 => "RLA",
            0x18 => "JR n",
            0x19 => "ADD HL, DE",
            0x1a => "LD A, (DE)",
            0x1b => "DEC DE",
            0x1c => "INC E",
            0x1d => "DEC E",
            0x1e => "LD E, n",
            0x1f => "RRA",

            0x20 => "JR NZ, n",
            0x21 => "LD HL, nn",
            0x22 => "LD (HL++), A",
            0x23 => "INC HL",
            0x24 => "INC H",
            0x25 => "DEC H",
            0x26 => "LD H, n",
            0x27 => "DAA",
            0x28 => "JR Z, n",
            0x29 => "ADD HL, HL",
            0x2a => "LD A, (HL++)",
            0x2b => "DEC HL",
            0x2c => "INC L",
            0x2d => "DEC L",
            0x2e => "LD L, N",
            0x2f => "CPL",

            0x30 => "JR NC, n",
            0x31 => "LD SP, nn",
            0x32 => "LD (HL--), A",
            0x33 => "INC SP",
            0x34 => "INC (HL)",
            0x35 => "DEC (HL)",
            0x36 => "LD (HL), n",
            0x37 => "SCF",
            0x38 => "JR C, n",
            0x39 => "ADD HL, SP",
            0x3a => "LD A, (HL--)",
            0x3b => "DEC SP",
            0x3c => "INC A",
            0x3d => "DEC A",
            0x3e => "LD A, n",
            0x3f => "CCF",

            0x40 => "LD B, B",
            0x41 => "LD B, C",
            0x42 => "LD B, D",
            0x43 => "LD B, E",
            0x44 => "LD B, H",
            0x45 => "LD B, L",
            0x46 => "LD B, (HL)",
            0x47 => "LD B, A",
            0x48 => "LD C, B",
            0x49 => "LD C, C",
            0x4a => "LD C, D",
            0x4b => "LD C, E",
            0x4c => "LD C, H",
            0x4d => "LD C, L",
            0x4e => "LD C, (HL)",
            0x4f => "LD C, A",

            0x50 => "LD D, B",
            0x51 => "LD D, C",
            0x52 => "LD D, D",
            0x53 => "LD D, E",
            0x54 => "LD D, H",
            0x55 => "LD D, L",
            0x56 => "LD D, (HL)",
            0x57 => "LD D, A",
            0x58 => "LD E, B",
            0x59 => "LD E, C",
            0x5a => "LD E, D",
            0x5b => "LD E, E",
            0x5c => "LD E, H",
            0x5d => "LD E, L",
            0x5e => "LD E, (HL)",
            0x5f => "LD E, A",

            0x60 => "LD H, B",
            0x61 => "LD H, C",
            0x62 => "LD H, D",
            0x63 => "LD H, E",
            0x64 => "LD H, H",
            0x65 => "LD H, L",
            0x66 => "LD H, (HL)",
            0x67 => "LD H, A",
            0x68 => "LD L, B",
            0x69 => "LD L, C",
            0x6a => "LD L, D",
            0x6b => "LD L, E",
            0x6c => "LD L, H",
            0x6d => "LD L, L",
            0x6e => "LD L, (HL)",
            0x6f => "LD L, A",

            0x70 => "LD (HL), B",
            0x71 => "LD (HL), C",
            0x72 => "LD (HL), D",
            0x73 => "LD (HL), E",
            0x74 => "LD (HL), H",
            0x75 => "LD (HL), L",
            0x76 => "HALT",
            0x77 => "LD (HL), A",
            0x78 => "LD A, B",
            0x79 => "LD A, C",
            0x7a => "LD A, D",
            0x7b => "LD A, E",
            0x7c => "LD A, H",
            0x7d => "LD A, L",
            0x7e => "LD A, (HL)",
            0x7f => "LD A, A",

            0x80 => "ADD A, B",
            0x81 => "ADD A, C",
            0x82 => "ADD A, D",
            0x83 => "ADD A, E",
            0x84 => "ADD A, H",
            0x85 => "ADD A, L",
            0x86 => "ADD A, (HL)",
            0x87 => "ADD A, A",
            0x88 => "ADC A, B",
            0x89 => "ADC A, C",
            0x8a => "ADC A, D",
            0x8b => "ADC A, E",
            0x8c => "ADC A, H",
            0x8d => "ADC A, L",
            0x8e => "ADC A, (HL)",
            0x8f => "ADC A, A",

            0x90 => "SUB B",
            0x91 => "SUB C",
            0x92 => "SUB D",
            0x93 => "SUB E",
            0x94 => "SUB H",
            0x95 => "SUB L",
            0x96 => "SUB (HL)",
            0x97 => "SUB A",
            0x98 => "SBC B",
            0x99 => "SBC C",
            0x9a => "SBC D",
            0x9b => "SBC E",
            0x9c => "SBC H",
            0x9d => "SBC L",
            0x9e => "SBC (HL)",
            0x9f => "SBC A",

            0xa0 => "AND B",
            0xa1 => "AND C",
            0xa2 => "AND D",
            0xa3 => "AND E",
            0xa4 => "AND H",
            0xa5 => "AND L",
            0xa6 => "AND (HL)",
            0xa7 => "AND A",
            0xa8 => "XOR B",
            0xa9 => "XOR C",
            0xaa => "XOR D",
            0xab => "XOR E",
            0xac => "XOR H",
            0xad => "XOR L",
            0xae => "XOR (HL)",
            0xaf => "XOR A",

            0xb0 => "OR B",
            0xb1 => "OR C",
            0xb2 => "OR D",
            0xb3 => "OR E",
            0xb4 => "OR H",
            0xb5 => "OR L",
            0xb6 => "OR (HL)",
            0xb7 => "OR A",
            0xb8 => "CP B",
            0xb9 => "CP C",
            0xba => "CP D",
            0xbb => "CP E",
            0xbc => "CP H",
            0xbd => "CP L",
            0xbe => "CP (HL)",
            0xbf => "CP A",

            0xc0 => "RET NZ",
            0xc1 => "POP BC",
            0xc2 => "JP NZ, nn",
            0xc3 => "JP nn",
            0xc4 => "CALL NZ, nn",
            0xc5 => "PUSH BC",
            0xc6 => "ADD A, n",
            0xc7 => "RST 00h",
            0xc8 => "RET Z",
            0xc9 => "RET",
            0xca => "JP Z, nn",
            0xcb => "CB prefix",
            0xcc => "CALL Z, nn",
            0xcd => "CALL nn",
            0xce => "ADC A, n",
            0xcf => "RST 08h",

            0xd0 => "RET NC",
            0xd1 => "POP DE",
            0xd2 => "JP NC, nn",
            0xd3 => "Invalid (D3)",
            0xd4 => "CALL NC, nn",
            0xd5 => "PUSH DE",
            0xd6 => "SUB n",
            0xd7 => "RST 10h",
            0xd8 => "RET C",
            0xd9 => "RETI",
            0xda => "JP C, nn",
            0xdb => "Invalid (DB)",
            0xdc => "CALL C, nn",
            0xdd => "Invalid (DD)",
            0xde => "SBC A, n",
            0xdf => "RST 18h",

            0xe0 => "LD (FF00+n), A",
            0xe1 => "POP HL",
            0xe2 => "LD (FF00+C), A",
            0xe3 => "Invalid (E3)",
            0xe4 => "Invalid (E4)",
            0xe5 => "PUSH HL",
            0xe6 => "AND n",
            0xe7 => "RST 20h",
            0xe8 => "ADD SP, d",
            0xe9 => "JP HL",
            0xea => "LD (nn), A",
            0xeb => "Invalid (EB)",
            0xec => "Invalid (EC)",
            0xed => "ED prefix",
            0xee => "XOR n",
            0xef => "RST 28h",

            0xf0 => "LD A, (FF00 + n)",
            0xf1 => "POP AF",
            0xf2 => "LD A, (FF00 + C)",
            0xf3 => "DI",
            0xf4 => "Invalid (F4)",
            0xf5 => "PUSH AF",
            0xf6 => "OR n",
            0xf7 => "RST 30h",
            0xf8 => "LD HL, SP + d",
            0xf9 => "LD SP, HL",
            0xfa => "LD A, (nn)",
            0xfb => "EI",
            0xfc => "Invalid (FC)",
            0xfd => "Invalid (FD)",
            0xfe => "CP n",
            0xff => "RST 38h",
        }
    } else if prefix == 0xcb {
        match opcode {
            0x00 => "RLC B",
            0x01 => "RLC C",
            0x02 => "RLC D",
            0x03 => "RLC E",
            0x04 => "RLC H",
            0x05 => "RLC L",
            0x06 => "RLC (HL)",
            0x07 => "RLC A",
            0x08 => "RRC B",
            0x09 => "RRC C",
            0x0a => "RRC D",
            0x0b => "RRC E",
            0x0c => "RRC H",
            0x0d => "RRC L",
            0x0e => "RRC (HL)",
            0x0f => "RRC A",
            0x10 => "RL B",
            0x11 => "RL C",
            0x12 => "RL D",
            0x13 => "RL E",
            0x14 => "RL H",
            0x15 => "RL L",
            0x16 => "RL (HL)",
            0x17 => "RL A",
            0x18 => "RR B",
            0x19 => "RR C",
            0x1a => "RR D",
            0x1b => "RR E",
            0x1c => "RR H",
            0x1d => "RR L",
            0x1e => "RR (HL)",
            0x1f => "RR A",
            0x20 => "SLA B",
            0x21 => "SLA C",
            0x22 => "SLA D",
            0x23 => "SLA E",
            0x24 => "SLA H",
            0x25 => "SLA L",
            0x26 => "SLA (HL)",
            0x27 => "SLA A",
            0x28 => "SRA B",
            0x29 => "SRA C",
            0x2a => "SRA D",
            0x2b => "SRA E",
            0x2c => "SRA H",
            0x2d => "SRA L",
            0x2e => "SRA (HL)",
            0x2f => "SRA A",
            0x30 => "SLL B",
            0x31 => "SLL C",
            0x32 => "SLL D",
            0x33 => "SLL E",
            0x34 => "SLL H",
            0x35 => "SLL L",
            0x36 => "SLL (HL)",
            0x37 => "SLL A",
            0x38 => "SRL B",
            0x39 => "SRL C",
            0x3a => "SRL D",
            0x3b => "SRL E",
            0x3c => "SRL H",
            0x3d => "SRL L",
            0x3e => "SRL (HL)",
            0x3f => "SRL A",
            0x40 => "BIT 0, B",
            0x41 => "BIT 0, C",
            0x42 => "BIT 0, D",
            0x43 => "BIT 0, E",
            0x44 => "BIT 0, H",
            0x45 => "BIT 0, L",
            0x46 => "BIT 0, (HL)",
            0x47 => "BIT 0, A",
            0x48 => "BIT 1, B",
            0x49 => "BIT 1, C",
            0x4a => "BIT 1, D",
            0x4b => "BIT 1, E",
            0x4c => "BIT 1, H",
            0x4d => "BIT 1, L",
            0x4e => "BIT 1, (HL)",
            0x4f => "BIT 1, A",
            0x50 => "BIT 2, B",
            0x51 => "BIT 2, C",
            0x52 => "BIT 2, D",
            0x53 => "BIT 2, E",
            0x54 => "BIT 2, H",
            0x55 => "BIT 2, L",
            0x56 => "BIT 2, (HL)",
            0x57 => "BIT 2, A",
            0x58 => "BIT 3, B",
            0x59 => "BIT 3, C",
            0x5a => "BIT 3, D",
            0x5b => "BIT 3, E",
            0x5c => "BIT 3, H",
            0x5d => "BIT 3, L",
            0x5e => "BIT 3, (HL)",
            0x5f => "BIT 3, A",
            0x60 => "BIT 4, B",
            0x61 => "BIT 4, C",
            0x62 => "BIT 4, D",
            0x63 => "BIT 4, E",
            0x64 => "BIT 4, H",
            0x65 => "BIT 4, L",
            0x66 => "BIT 4, (HL)",
            0x67 => "BIT 4, A",
            0x68 => "BIT 5, B",
            0x69 => "BIT 5, C",
            0x6a => "BIT 5, D",
            0x6b => "BIT 5, E",
            0x6c => "BIT 5, H",
            0x6d => "BIT 5, L",
            0x6e => "BIT 5, (HL)",
            0x6f => "BIT 5, A",
            0x70 => "BIT 6, B",
            0x71 => "BIT 6, C",
            0x72 => "BIT 6, D",
            0x73 => "BIT 6, E",
            0x74 => "BIT 6, H",
            0x75 => "BIT 6, L",
            0x76 => "BIT 6, (HL)",
            0x77 => "BIT 6, A",
            0x78 => "BIT 7, B",
            0x79 => "BIT 7, C",
            0x7a => "BIT 7, D",
            0x7b => "BIT 7, E",
            0x7c => "BIT 7, H",
            0x7d => "BIT 7, L",
            0x7e => "BIT 7, (HL)",
            0x7f => "BIT 7, A",
            0x80 => "RES 0, B",
            0x81 => "RES 0, C",
            0x82 => "RES 0, D",
            0x83 => "RES 0, E",
            0x84 => "RES 0, H",
            0x85 => "RES 0, L",
            0x86 => "RES 0, (HL)",
            0x87 => "RES 0, A",
            0x88 => "RES 1, B",
            0x89 => "RES 1, C",
            0x8a => "RES 1, D",
            0x8b => "RES 1, E",
            0x8c => "RES 1, H",
            0x8d => "RES 1, L",
            0x8e => "RES 1, (HL)",
            0x8f => "RES 1, A",
            0x90 => "RES 2, B",
            0x91 => "RES 2, C",
            0x92 => "RES 2, D",
            0x93 => "RES 2, E",
            0x94 => "RES 2, H",
            0x95 => "RES 2, L",
            0x96 => "RES 2, (HL)",
            0x97 => "RES 2, A",
            0x98 => "RES 3, B",
            0x99 => "RES 3, C",
            0x9a => "RES 3, D",
            0x9b => "RES 3, E",
            0x9c => "RES 3, H",
            0x9d => "RES 3, L",
            0x9e => "RES 3, (HL)",
            0x9f => "RES 3, A",
            0xa0 => "RES 4, B",
            0xa1 => "RES 4, C",
            0xa2 => "RES 4, D",
            0xa3 => "RES 4, E",
            0xa4 => "RES 4, H",
            0xa5 => "RES 4, L",
            0xa6 => "RES 4, (HL)",
            0xa7 => "RES 4, A",
            0xa8 => "RES 5, B",
            0xa9 => "RES 5, C",
            0xaa => "RES 5, D",
            0xab => "RES 5, E",
            0xac => "RES 5, H",
            0xad => "RES 5, L",
            0xae => "RES 5, (HL)",
            0xaf => "RES 5, A",
            0xb0 => "RES 6, B",
            0xb1 => "RES 6, C",
            0xb2 => "RES 6, D",
            0xb3 => "RES 6, E",
            0xb4 => "RES 6, H",
            0xb5 => "RES 6, L",
            0xb6 => "RES 6, (HL)",
            0xb7 => "RES 6, A",
            0xb8 => "RES 7, B",
            0xb9 => "RES 7, C",
            0xba => "RES 7, D",
            0xbb => "RES 7, E",
            0xbc => "RES 7, H",
            0xbd => "RES 7, L",
            0xbe => "RES 7, (HL)",
            0xbf => "RES 7, A",
            0xc0 => "SET 0, B",
            0xc1 => "SET 0, C",
            0xc2 => "SET 0, D",
            0xc3 => "SET 0, E",
            0xc4 => "SET 0, H",
            0xc5 => "SET 0, L",
            0xc6 => "SET 0, (HL)",
            0xc7 => "SET 0, A",
            0xc8 => "SET 1, B",
            0xc9 => "SET 1, C",
            0xca => "SET 1, D",
            0xcb => "SET 1, E",
            0xcc => "SET 1, H",
            0xcd => "SET 1, L",
            0xce => "SET 1, (HL)",
            0xcf => "SET 1, A",
            0xd0 => "SET 2, B",
            0xd1 => "SET 2, C",
            0xd2 => "SET 2, D",
            0xd3 => "SET 2, E",
            0xd4 => "SET 2, H",
            0xd5 => "SET 2, L",
            0xd6 => "SET 2, (HL)",
            0xd7 => "SET 2, A",
            0xd8 => "SET 3, B",
            0xd9 => "SET 3, C",
            0xda => "SET 3, D",
            0xdb => "SET 3, E",
            0xdc => "SET 3, H",
            0xdd => "SET 3, L",
            0xde => "SET 3, (HL)",
            0xdf => "SET 3, A",
            0xe0 => "SET 4, B",
            0xe1 => "SET 4, C",
            0xe2 => "SET 4, D",
            0xe3 => "SET 4, E",
            0xe4 => "SET 4, H",
            0xe5 => "SET 4, L",
            0xe6 => "SET 4, (HL)",
            0xe7 => "SET 4, A",
            0xe8 => "SET 5, B",
            0xe9 => "SET 5, C",
            0xea => "SET 5, D",
            0xeb => "SET 5, E",
            0xec => "SET 5, H",
            0xed => "SET 5, L",
            0xee => "SET 5, (HL)",
            0xef => "SET 5, A",
            0xf0 => "SET 6, B",
            0xf1 => "SET 6, C",
            0xf2 => "SET 6, D",
            0xf3 => "SET 6, E",
            0xf4 => "SET 6, H",
            0xf5 => "SET 6, L",
            0xf6 => "SET 6, (HL)",
            0xf7 => "SET 6, A",
            0xf8 => "SET 7, B",
            0xf9 => "SET 7, C",
            0xfa => "SET 7, D",
            0xfb => "SET 7, E",
            0xfc => "SET 7, H",
            0xfd => "SET 7, L",
            0xfe => "SET 7, (HL)",
            0xff => "SET 7, A",
        }
    } else {
        panic!("Tried to display an invliad opcode")
    }
}
