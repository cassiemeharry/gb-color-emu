use arrayvec::ArrayVec;
use std::fmt;

use crate::{
    cart::Cart,
    cpu::{CPU, InterruptFlags},
    memory::{Memory, MemoryMapChange as MMC},
    screen::{Screen, Rgb555},
    sound::APU,
    system::{SystemRef, RwSource},
};

bitflags::bitflags! {
    #[derive(Default)]
    pub struct LcdControlFlags: u8 {
        const ENABLE = 1 << 7;
        const WINDOW_TILE_MAP_HIGH = 1 << 6;
        const WINDOW_ENABLE = 1 << 5;
        const TILE_DATA_LOW = 1 << 4;
        const BG_TILE_MAP_HIGH = 1 << 3;
        const OBJ_SIZE_TALL = 1 << 2;
        const OBJ_ENABLE = 1 << 1;
        const BG_WINDOW_ENABLE = 1 << 0;
    }

    #[derive(Default)]
    pub struct LcdStatusFlags: u8 {
        const INTERRUPT_LYC_EQ_LY = 1 << 6;
        const INTERRUPT_MODE_2_OAM = 1 << 5;
        const INTERRUPT_MODE_1_VBLANK = 1 << 4;
        const INTERRUPT_MODE_0_HBLANK = 1 << 3;
        const LYC_EQ_LY = 1 << 2;
        const MODE_MASK = 0b11;
    }

    #[derive(Default)]
    pub struct OAMFlags: u8 {
        const BG_WIND_OVER_OBJ = 1 << 7;
        const Y_FLIP = 1 << 6;
        const X_FLIP = 1 << 5;
        const PALETTE_HIGH = 1 << 4;
        const TILE_VRAM_BANK_HIGH = 1 << 3;
        const PALETTE_NUMBER_MASK = 0b111;
    }
}

impl LcdStatusFlags {
    pub const WRITABLE_MASK: Self =
        Self::INTERRUPT_LYC_EQ_LY
        .union(Self::INTERRUPT_MODE_2_OAM)
        .union(Self::INTERRUPT_MODE_1_VBLANK)
        .union(Self::INTERRUPT_MODE_0_HBLANK);

    pub fn mode_flag(self) -> LcdModeFlag {
        match self.bits() & Self::MODE_MASK.bits() {
            0 => LcdModeFlag::HBlank,
            1 => LcdModeFlag::VBlank,
            2 => LcdModeFlag::OAM,
            3 => LcdModeFlag::LCDTransfer,
            _ => unreachable!(),
        }
    }

    pub fn set_mode_flag(&mut self, mode: LcdModeFlag) {
        let prev_bits = self.difference(Self::MODE_MASK).bits();
        let new_bits = prev_bits | (mode as u8);
        *self = Self::from_bits_truncate(new_bits);
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LcdModeFlag {
    HBlank = 0,
    VBlank = 1,
    OAM = 2,
    LCDTransfer = 3,
}

impl fmt::Display for LcdModeFlag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n = *self as u8;
        write!(f, "mode {}", n)
    }
}

impl OAMFlags {
    #[inline]
    fn palette_number(self) -> u8 {
        self.bits() & Self::PALETTE_NUMBER_MASK.bits()
    }
}

#[derive(Clone, Debug)]
pub struct LcdRegisters {
    pub control: LcdControlFlags,
    pub status: LcdStatusFlags,
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub window_y: u8,
    pub window_x: u8,
    pub lcd_current_y: u8,
    pub lcd_compare_y: u8,
    pub bg_palette_data: u8,
    pub obj_palette_data_0: u8,
    pub obj_palette_data_1: u8,
    pub bg_color_palette: LcdColorPalette<0xff68>,
    pub obj_color_palette: LcdColorPalette<0xff6a>,
    pub object_attr_memory: OAM,
}

impl Default for LcdRegisters {
    fn default() -> Self {
        Self {
            control: LcdControlFlags::empty(),
            status: LcdStatusFlags::empty(),
            scroll_y: 0x00,
            scroll_x: 0x00,
            lcd_current_y: 0x00,
            lcd_compare_y: 0x00,
            bg_palette_data: 0xfc,
            obj_palette_data_0: 0xfc,
            obj_palette_data_1: 0xfc,
            window_y: 0x00,
            window_x: 0x00,
            bg_color_palette: LcdColorPalette::default(),
            obj_color_palette: LcdColorPalette::default(),
            object_attr_memory: OAM::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LcdColorPalette<const SPEC_ADDR: u16> {
    spec: u8,
    data: Vec<u8>,
}

impl<const N: u16> Default for LcdColorPalette<N> {
    fn default() -> Self {
        Self {
            spec: u8::default(),
            data: vec![0x00; 0x40],
        }
    }
}

impl<const SPEC_ADDR: u16> LcdColorPalette<SPEC_ADDR> {
    const INDEX_MASK: u8 = 0x1f;
    const INCR_MASK: u8 = 1 << 7;
    const MASK: u8 = Self::INDEX_MASK | Self::INCR_MASK;

    pub fn write_spec(&mut self, new_value: u8) -> Option<MMC> {
        let new_value = new_value & Self::MASK;
        self.spec = new_value;
        Some(MMC::InvalidateRange(SPEC_ADDR..=(SPEC_ADDR + 1)))
    }

    pub fn write_data(&mut self, new_value: u8) -> Option<MMC> {
        let index = (self.spec & Self::INDEX_MASK) as usize;
        let auto_increment = (self.spec & Self::INCR_MASK) != 0;

        self.data[index] = new_value;

        if auto_increment {
            self.spec = (self.spec & Self::INCR_MASK)
                | (((index + 1) as u8) & Self::INDEX_MASK);
            Some(MMC::InvalidateRange(SPEC_ADDR..=(SPEC_ADDR + 1)))
        } else {
            Some(MMC::InvalidateOne(SPEC_ADDR + 1))
        }
    }
}

#[derive(Clone, Debug)]
pub struct OAM {
    entries: [OAMEntry; 40],
}

impl Default for OAM {
    fn default() -> Self {
        const DEFAULT_ENTRY: OAMEntry = OAMEntry {
            y_position: 0,
            x_position: 0,
            tile_index: 0,
            flags: OAMFlags::empty(),
        };
        Self {
            entries: [DEFAULT_ENTRY; 40],
        }
    }
}

impl OAM {
    pub fn read(&self, address: u16) -> Option<u8> {
        let byte_index = match address {
            0xfe00..=0xfe9f => (address - 0xfe00) as usize,
            _ => return None,
        };
        let index = byte_index / 4;
        let offset = byte_index % 4;
        let entry = &self.entries[index];
        match offset {
            0 => Some(entry.y_position),
            1 => Some(entry.x_position),
            2 => Some(entry.tile_index),
            3 => Some(entry.flags.bits()),
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, address: u16, data: u8) -> Option<MMC> {
        let byte_index = match address {
            0xfe00..=0xfe9f => (address - 0xfe00) as usize,
            _ => return None,
        };
        let index = byte_index / 4;
        let offset = byte_index % 4;
        let entry = &mut self.entries[index];
        match offset {
            0 => MMC::invalidate_one(address, &mut entry.y_position, data),
            1 => MMC::invalidate_one(address, &mut entry.x_position, data),
            2 => MMC::invalidate_one(address, &mut entry.tile_index, data),
            3 => MMC::invalidate_one(address, &mut entry.flags, OAMFlags::from_bits_truncate(data)),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct OAMEntry {
    y_position: u8,
    x_position: u8,
    tile_index: u8,
    flags: OAMFlags,
}

#[derive(Clone, Debug)]
pub struct LcdController {
    pub(crate) registers: LcdRegisters,
    last_frame_start_cycle: u64,
    last_cycle: u64,
    was_disabled: bool,
    line_state: LineState,
}

#[derive(Clone, Debug, Default)]
struct LineState {
    active_sprite_indexes: ArrayVec<usize, 10>,
    next_screen_x: u8,
    draw_state: DrawState,
}

#[derive(Copy, Clone, Debug)]
enum DrawState {
    GetTileInfo,
    GetTileDataLow { map_index: u8, tile_attrs: u8 },
    GetTileDataHigh { map_index: u8, tile_attrs: u8, low: u8 },
    // Sleep { tile_attrs: u8, low: u8, high: u8 },
    Push { tile_attrs: u8, low: u8, high: u8 },
}

impl Default for DrawState {
    fn default() -> Self {
        Self::GetTileInfo
    }
}

const CYCLES_PER_LINE: u64 = 456;
const CYCLES_PER_FRAME: u64 = 70224;

struct SysRef<'a> {
    cart: &'a mut Cart,
    cpu: &'a mut CPU,
    memory: &'a mut Memory,
    screen: &'a mut Screen,
    sound: &'a mut APU,
}

impl<'a> SysRef<'a> {
    #[inline]
    fn as_full<'b: 'a>(&'b mut self, lcd: &'b mut LcdController) -> SystemRef<'b> {
        SystemRef {
            cart: &mut *self.cart,
            cpu: &mut *self.cpu,
            lcd,
            memory: &mut *self.memory,
            screen: &mut *self.screen,
            sound: &mut *self.sound,
        }
    }

    #[inline]
    fn read<'b: 'a>(&'b mut self, lcd: &'b mut LcdController, address: u16) -> Option<u8> {
        let mut full = self.as_full(lcd);
        full.read(address, RwSource::PPU)
    }
}

impl LcdController {
    pub(crate) fn new() -> Self {
        Self {
            registers: LcdRegisters::default(),
            last_frame_start_cycle: 0,
            last_cycle: 0,
            was_disabled: true,
            line_state: LineState::default(),
        }
    }

    pub(crate) fn oam_inaccessible(&self) -> bool {
        match self.registers.status.mode_flag() {
            LcdModeFlag::HBlank => false,
            LcdModeFlag::VBlank => false,
            LcdModeFlag::OAM => true,
            LcdModeFlag::LCDTransfer => true,
        }
    }

    fn set_mode_flag(&mut self, sys: &mut SysRef<'_>, flag: LcdModeFlag) {
        if self.registers.status.mode_flag() == flag {
            return;
        }
        self.registers.status.set_mode_flag(flag);
        sys.cpu.registers.interrupt_flags.remove(InterruptFlags::LCD_STAT | InterruptFlags::VBLANK);
        match flag {
            LcdModeFlag::HBlank if self.registers.status.contains(LcdStatusFlags::INTERRUPT_MODE_0_HBLANK) => {
                debug!("LCD controller is triggering LCD_STAT interrupt because of HBlank");
                sys.cpu.registers.interrupt_flags.insert(InterruptFlags::LCD_STAT);
            }
            LcdModeFlag::HBlank => (),
            LcdModeFlag::OAM if self.registers.status.contains(LcdStatusFlags::INTERRUPT_MODE_2_OAM) => {
                debug!("LCD controller is triggering LCD_STAT interrupt because of OAM");
                sys.cpu.registers.interrupt_flags.insert(InterruptFlags::LCD_STAT);
            }
            LcdModeFlag::OAM => (),
            LcdModeFlag::LCDTransfer => (),
            LcdModeFlag::VBlank => {
                if self.registers.status.contains(LcdStatusFlags::INTERRUPT_MODE_1_VBLANK) {
                    debug!("LCD controller is triggering LCD_STAT interrupt because of VBlank");
                    sys.cpu.registers.interrupt_flags.insert(InterruptFlags::LCD_STAT);
                }
                debug!("LCD controller is triggering VBLANK interrupt");
                sys.cpu.registers.interrupt_flags.insert(InterruptFlags::VBLANK);
            }
        }
    }

    pub(crate) fn step(
        &mut self,
        end_cycle: u64,
        cart: &mut Cart,
        cpu: &mut CPU,
        memory: &mut Memory,
        screen: &mut Screen,
        sound: &mut APU,
    ) {
        if !self.registers.control.contains(LcdControlFlags::ENABLE) {
            self.was_disabled = true;
            return;
        }

        if self.was_disabled {
            self.last_frame_start_cycle = end_cycle;
            self.last_cycle = end_cycle;
            self.was_disabled = false;
            return;
        }

        let mut sys = SysRef { cart, cpu, memory, screen, sound };

        while let Ok(()) = self.small_step(end_cycle, &mut sys) {
            // keep going
        }

        let delta_frame_start: u64 = self.last_cycle.checked_sub(self.last_frame_start_cycle).unwrap();
        let line_number = delta_frame_start / CYCLES_PER_LINE;
        let line_number: u8 = match line_number.try_into() {
            Ok(ln) => ln,
            Err(e) => panic!(
                "Failed to fit line number {} in u8, frame started on {}, last_cycle was {}, delta is {}",
                line_number,
                self.last_frame_start_cycle,
                self.last_cycle,
                delta_frame_start,
            ),
        };
        self.registers.lcd_current_y = line_number;
    }

    fn small_step(&mut self, end_cycle: u64, sys: &mut SysRef<'_>) -> Result<(), ()> {
        if end_cycle <= self.last_cycle {
            return Err(());
        }
        // This should also always be higher than when our last frame started.
        let delta_frame_start: u64 = self.last_cycle.checked_sub(self.last_frame_start_cycle).unwrap();
        trace!(
            "LCD controller small step, going from {} towards {} (delta_frame_start = {})",
            self.last_cycle, end_cycle, delta_frame_start,
        );
        let mut new_cycle = self.last_cycle;

        // Macro instead of function because we want to assign to `new_cycle`
        // without actually borrowing it.
        macro_rules! require_cycles {
            ($n:expr) => {
                let n = $n;
                if self.last_cycle + n > end_cycle {
                    return Err(());
                } else {
                    new_cycle += n;
                }
            };
        }

        // let mut require_cycles = |n: u64| -> Result<(), ()> {
        //     if self.last_cycle + n > end_cycle {
        //         Err(())
        //     } else {
        //         new_cycle += n;
        //         Ok(())
        //     }
        // };

        let line_number = delta_frame_start / CYCLES_PER_LINE;
        let line_number: u8 = match line_number.try_into() {
            Ok(ln) => ln,
            Err(e) => panic!(
                "Failed to fit line number {} in u8, frame started on {}, last_cycle was {}, delta is {}",
                line_number,
                self.last_frame_start_cycle,
                self.last_cycle,
                delta_frame_start,
            ),
        };
        let line_progress = delta_frame_start % CYCLES_PER_LINE;
        match line_number {
            0..=143 => {
                if line_progress == 0 {
                    self.line_state.active_sprite_indexes.clear();
                } else if line_progress == 80 {
                    self.line_state.next_screen_x = 0;
                    self.line_state.draw_state = DrawState::default();
                }
                if line_progress < 80 {
                    self.set_mode_flag(&mut *sys, LcdModeFlag::OAM);
                    require_cycles!(2);
                    let index = (line_progress / 2) as usize;
                    trace!("OAM scanning index {}", index);
                    let entry = &self.registers.object_attr_memory.entries[index];
                    let tall_objs = self.registers.control.contains(LcdControlFlags::OBJ_SIZE_TALL);
                    let mut include = true;
                    if tall_objs && entry.y_position == 0 {
                        include = false;
                    } else if !tall_objs && entry.y_position <= 8 {
                        include = false;
                    } else if entry.y_position >= 160 {
                        include = false;
                    }
                    if include {
                        let _ = self.line_state.active_sprite_indexes.try_push(index);
                    }
                } else if self.line_state.next_screen_x < 160 {
                    self.set_mode_flag(sys, LcdModeFlag::LCDTransfer);
                    self.line_state.draw_state = match self.line_state.draw_state {
                        DrawState::GetTileInfo => {
                            require_cycles!(2);
                            trace!("DrawState::GetTileInfo");
                            let tilemap_base: u16 = 0x9800;
                            // TODO: window over BG would change `tilemap_base`
                            let x = ((self.registers.scroll_x / 8) + self.line_state.next_screen_x) & 0x1f;
                            let y = line_number.wrapping_add(self.registers.scroll_y);
                            let offset = (y as u16 / 8) + (x as u16);
                            let map_address = (tilemap_base + offset) as usize - 0x8000;
                            let map_index = sys.memory.video_ram_bank_0[map_address];
                            let tile_attrs = sys.memory.video_ram_bank_1[map_address];
                            DrawState::GetTileDataLow { map_index, tile_attrs }
                        }
                        DrawState::GetTileDataLow { map_index, tile_attrs } => {
                            require_cycles!(2);
                            trace!("DrawState::GetTileDataLow");
                            // TODO: check if tile is flipped vertically, and whether PPU access to VRAM is blocked.
                            let address: u16 = if self.registers.control.contains(LcdControlFlags::TILE_DATA_LOW) {
                                0x8000 + (map_index as u16) * 16
                            } else if map_index > 127 {
                                0x8800 + ((map_index as u16) - 128) * 16
                            } else {
                                0x9000 + (map_index as u16) * 16
                            };
                            let low = sys.memory.read(address).unwrap();
                            DrawState::GetTileDataHigh { map_index, tile_attrs, low }
                        }
                        DrawState::GetTileDataHigh { map_index, tile_attrs, low } => {
                            require_cycles!(2);
                            trace!("DrawState::GetTileDataHigh");
                            // TODO: check if tile is flipped vertically, and whether PPU access to VRAM is blocked.
                            let address: u16 = if self.registers.control.contains(LcdControlFlags::TILE_DATA_LOW) {
                                0x8000 + (map_index as u16) * 16
                            } else if map_index > 127 {
                                0x8800 + ((map_index as u16) - 128) * 16
                            } else {
                                0x9000 + (map_index as u16) * 16
                            };
                            let high = sys.memory.read(address + 1).unwrap();
                            DrawState::Push { tile_attrs, low, high }
                        }
                        // DrawState::Sleep { tile_attrs, low, high } => {
                        //     require_cycles(2)?;
                        //     // No real activity here.
                        //     DrawState::Push { tile_attrs, low, high }
                        // }
                        DrawState::Push { tile_attrs, low, high } => {
                            require_cycles!(2);
                            trace!("DrawState::Push");
                            let combined = ((low as u16) << 8) | (high as u16);
                            let mut i = 16;
                            while i > 0 {
                                i -= 2;
                                let color_index = (((combined >> i) & 0b11) as usize) * 2;
                                let lsb = self.registers.bg_color_palette.data[color_index];
                                let msb = self.registers.bg_color_palette.data[color_index + 1];
                                let color = Rgb555::from((lsb, msb));
                                if self.line_state.next_screen_x < 160 {
                                    sys.screen.put_pixel(self.line_state.next_screen_x, line_number, color);
                                    self.line_state.next_screen_x += 1;
                                }
                            }
                            if self.line_state.next_screen_x >= 160 {
                                self.set_mode_flag(sys, LcdModeFlag::HBlank);
                            }
                            DrawState::GetTileInfo
                        }
                    };
                } else {
                    let target = self.last_cycle + (CYCLES_PER_LINE - line_progress);
                    trace!(
                        "LCD controller in hblank (frame delta {}, line {}, line_progress {}, end of hblank target is {})",
                        delta_frame_start,
                        line_number,
                        line_progress,
                        target,
                    );
                    debug_assert!(target > self.last_cycle);
                    if target <= end_cycle {
                        new_cycle = target;
                        if line_number == 143 {
                            self.set_mode_flag(sys, LcdModeFlag::VBlank);
                        }
                    } else {
                        new_cycle = end_cycle;
                    }
                }
            }
            144.. => {
                let target = self.last_frame_start_cycle + CYCLES_PER_FRAME;
                trace!(
                    "LCD controller in vblank (frame delta {}, line {}, end of vblank target is {})",
                    delta_frame_start,
                    line_number,
                    target,
                );
                debug_assert!(target > self.last_cycle);
                if target <= end_cycle {
                    new_cycle = target;
                    self.last_frame_start_cycle = target;
                    self.set_mode_flag(sys, LcdModeFlag::OAM);
                    debug!("VBlank has ended in LCD controller");
                } else {
                    new_cycle = end_cycle;
                }
            }
        }

        debug_assert!(new_cycle > self.last_cycle, "new_cycle did not increase, new_cycle = {:?}, last_cycle = {:?}", new_cycle, self.last_cycle);
        debug_assert!(new_cycle <= end_cycle);
        self.last_cycle = new_cycle;
        Ok(())
    }
}
