use arrayvec::ArrayVec;
use bitfield::bitfield;
use log::*;
use std::{collections::VecDeque, fmt};

use crate::screen::Color;
use super::SM83Pinout;

bitfield! {
    #[derive(Copy, Clone, Default)]
    struct LcdControl(u8);
    impl Debug;
    lcd_enable, set_lcd_enable: 7;
    window_tile_map_area, set_window_tile_map_area: 6;
    window_enable, set_window_eanble: 5;
    bg_window_tile_data_area, set_bg_window_tile_data_area: 4;
    bg_tile_map_area, set_bg_tile_map_area: 3;
    obj_size, set_obj_size: 2;
    obj_enable, set_obj_enable: 1;
    bg_window_enable, set_bg_window_enable: 0;
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum ModeBits {
    HBlank = 0,
    VBlank = 1,
    SearchingOAM = 2,
    TransferToLcdController = 3,
}

impl Default for ModeBits {
    fn default() -> Self {
        Self::HBlank
    }
}

impl From<u8> for ModeBits {
    fn from(x: u8) -> Self {
        match x {
            0 => Self::HBlank,
            1 => Self::VBlank,
            2 => Self::SearchingOAM,
            3 => Self::TransferToLcdController,
            _ => panic!("Only lower two bits may be set in conversion from u8 to ModeBits"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum SearchingOAMStep {
    Reading,
    Processing { bytes: [u8; 4] },
}

#[derive(Clone, Debug)]
struct State {
    frame_dot_counter: u32,
    pixel_fifo: VecDeque<(u8, u8, u8)>,
    mode: Mode,
}

#[derive(Clone, Debug)]
enum Mode {
    HBlank { dots_remaining: u8 },
    VBlank { dots_remaining: u16 },
    SearchingOAM {
        sprite_index: u8,
        search_step: SearchingOAMStep,
        sprites_to_render: ArrayVec<u8, 10>,
    },
    TransferToLcdController {
        current_x: u8,
        sprites_to_render: ArrayVec<u8, 10>,
        step: Mode3Step,
    },
}

impl Mode {
    #[inline]
    fn search_start() -> Self {
        Self::SearchingOAM {
            sprite_index: 0,
            search_step: SearchingOAMStep::Reading,
            sprites_to_render: ArrayVec::new(),
        }
    }
}

impl Default for Mode {
    #[inline]
    fn default() -> Self {
        Self::search_start()
    }
}

#[derive(Clone, Debug)]
enum Mode3Step {
    GetTile { next: bool },
    GetTileDataLow { next: bool },
    GetTileDataHigh { next: bool },
    Sleep { next: bool },
    Push { },
}

bitfield! {
    #[derive(Copy, Clone, Default, PartialEq, Eq)]
    pub struct LcdStatus(u8);
    impl Debug;
    lyc_eq_ly_stat_interrupt, set_lyc_eq_ly_stat_interrupt: 6;
    mode_2_oam_stat_interrupt, set_mode_2_oam_stat_interrupt: 5;
    mode_1_vblank_stat_interrupt, set_mode_1_vblank_stat_interrupt: 4;
    mode_0_hblank_stat_interrupt, set_mode_0_hblank_stat_interrupt: 3;
    lyc_eq_ly_flag, _: 2;
    u8, into ModeBits, mode_flag, set_mode_flag: 1, 0;
}

#[derive(Clone, Debug, Default)]
struct PPURegisters {
    // 0xff40 - LCD Control (R/W)
    control: LcdControl,
    // 0xff41 - LCD Status (R/W)
    status: LcdStatus,
    // 0xff42 - Scroll Y (R/W)
    scroll_y: u8,
    // 0xff43 - Scroll X (R/W)
    scroll_x: u8,
    // 0xff44 - LCD Y Coordinate (R)
    y_coordinate: u8,
    // 0xff45 - LY Compare (R/W)
    y_compare: u8,
    // 0xff4a - Window Y position (R/W)
    window_y_position: u8,
    // 0xff4b - Window X position (R/W)
    window_x_position: u8,

    // Non-color palette registers:
    // 0xff47 - BG Palette Data (R/W) - Non-CGB mode only
    bw_bg_palette_data: u8,
    // 0xff48 - OBJ Palette 0 Data (R/W)
    bw_obj_palette_0_data: u8,
    // 0xff49 - OBJ Palette 1 Data (R/W)
    bw_obj_palette_1_data: u8,

    // Color palette registers:
    color_bg_palette_index: u8,
    color_obj_palette_index: u8,
}

impl PPURegisters {
    fn get_mode(&self) -> ModeBits {
        self.status.mode_flag()
    }

    fn set_mode(&mut self, mode: ModeBits) {
        self.status.set_mode_flag(mode as u8);
    }
}

#[derive(Clone)]
struct PPUMemory {
    oam: Vec<u8>,
    vram_bank_0: Vec<u8>,
    vram_bank_1: Vec<u8>,
    vram_bank_1_active: bool,
    internal_vram_read_enabled: bool,
    color_bg_palette_data: Vec<u8>,
    color_obj_palette_data: Vec<u8>,
}

impl Default for PPUMemory {
    fn default() -> Self {
        Self {
            oam: vec![0x00; 0x100],
            vram_bank_0: vec![0x00; 0x2000],
            vram_bank_1: vec![0x00; 0x2000],
            vram_bank_1_active: false,
            internal_vram_read_enabled: true,
            color_bg_palette_data: vec![0x00; 64],
            color_obj_palette_data: vec![0x00; 64],
        }
    }
}

impl fmt::Debug for PPUMemory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PPUMemory")
            .field("oam", &format_args!("<{} bytes>", self.oam.len()))
            .field("vram_bank_0", &format_args!("<{} bytes>", self.vram_bank_0.len()))
            .field("vram_bank_1", &format_args!("<{} bytes>", self.vram_bank_1.len()))
            .field("vram_bank_1_active", &self.vram_bank_1_active)
            .field("internal_vram_read_enabled", &self.internal_vram_read_enabled)
            .finish()
    }
}

impl PPUMemory {
    fn ppu_vram_read(&self, index: u16) -> u8 {
        match (self.internal_vram_read_enabled, self.vram_bank_1_active) {
            (false, _) => 0xff,
            (true, false) => self.vram_bank_0[index as usize],
            (true, true) => self.vram_bank_1[index as usize],
        }
    }
}

#[derive(Clone)]
pub struct PPU {
    regs: PPURegisters,
    mem: PPUMemory,
    state: Option<State>,
}

impl fmt::Debug for PPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PPU")
            .field("regs", &self.regs)
            .field("state", &self.state)
            .finish()
    }
}

impl Default for PPU {
    fn default() -> Self {
        Self {
            regs: PPURegisters::default(),
            mem: PPUMemory::default(),
            state: Some(State {
                frame_dot_counter: 0,
                pixel_fifo: VecDeque::with_capacity(160),
                mode: Mode::search_start(),
            }),
        }
    }
}

impl PPU {
    pub fn step_cycle(&mut self, pins: &mut SM83Pinout) {
        let state = match self.state.as_mut() {
            Some(s) => s,
            None => return,
        };

        let dots_this_scanline = state.frame_dot_counter % 456;
        trace!(
            "Starting PPU step with {} dots this scanline ({} dots this frame, on scanline {})",
            dots_this_scanline, state.frame_dot_counter, self.regs.y_coordinate,
        );
        trace!("PPU mode: {:?}", state.mode);

        match state.mode {
            Mode::HBlank { dots_remaining } if dots_remaining == 0 => {
                assert_eq!(
                    dots_this_scanline, 455,
                    "HBlank end with the wrong dot length: {} (expected 455), total: {}, y: {}",
                    dots_this_scanline, state.frame_dot_counter, self.regs.y_coordinate,
                );
                if self.regs.y_coordinate >= 143 {
                    state.mode = Mode::VBlank { dots_remaining: 4560 - 1 };
                } else {
                    state.mode = Mode::search_start();
                }
                self.regs.y_coordinate += 1;
            }
            Mode::HBlank { ref mut dots_remaining } => *dots_remaining -= 1,
            Mode::VBlank { dots_remaining: 0 } => {
                state.mode = Mode::search_start();
                self.regs.y_coordinate = 0;
                assert_eq!(state.frame_dot_counter + 1, 70224);
                state.frame_dot_counter = 0;
                return;
            }
            Mode::VBlank { ref mut dots_remaining } => {
                if *dots_remaining % 456 == 0 {
                    self.regs.y_coordinate += 1;
                    assert!(self.regs.y_coordinate < 154);
                }
                *dots_remaining -= 1;
            }
            Mode::SearchingOAM {
                ref mut sprite_index,
                ref mut search_step,
                ref mut sprites_to_render,
            } => {
                match *search_step {
                    SearchingOAMStep::Reading => {
                        if *sprite_index == 0 {
                            assert_eq!(
                                dots_this_scanline, 0,
                                "PPU Mode 2 should start on dot 1 for the frame, not dot {}",
                                dots_this_scanline + 1,
                            );
                        }
                        let i = *sprite_index as usize * 4;
                        let mut bytes = [0x00; 4];
                        bytes.copy_from_slice(&self.mem.oam[i..i+4]);
                        *search_step = SearchingOAMStep::Processing { bytes };
                    }
                    SearchingOAMStep::Processing { bytes } => {
                        let sprite_y_plus_16 = bytes[0];
                        let scanline = self.regs.y_coordinate;
                        if self.regs.control.obj_size() {
                            // 16px tall sprites
                            if sprite_y_plus_16 > scanline && sprite_y_plus_16 < scanline + 15 {
                                let _ = sprites_to_render.try_push(*sprite_index);
                            }
                        } else {
                            // 8px tall sprites
                            if sprite_y_plus_16 > (scanline + 8) && sprite_y_plus_16 < scanline + 15 {
                                let _ = sprites_to_render.try_push(*sprite_index);
                            }
                        }
                        *search_step = SearchingOAMStep::Reading;
                        *sprite_index += 1;
                    }
                }
                if *sprite_index >= 40 {
                    assert_eq!(dots_this_scanline, 79, "SearchingOAM mode should take exactly 80 dots, not {} dots", dots_this_scanline + 1);
                    state.mode = Mode::TransferToLcdController {
                        current_x: 0,
                        sprites_to_render: std::mem::take(sprites_to_render),
                        step: Mode3Step::GetTile { next: false },
                    };
                }
            }
            Mode::TransferToLcdController { .. } if dots_this_scanline < 140 => (),
            Mode::TransferToLcdController {
                ref mut current_x,
                ref mut sprites_to_render,
                ref mut step,
            } => {
                // TODO: this is *way* more complicated than this code makes it
                // look. For example, we're entirely ignoring the "window" stuff
                // for now. See <https://gbdev.io/pandocs/pixel_fifo.html> for
                // more details.
                let mut bg_tilemap_base: u16 = 0x1800;
                if self.regs.control.bg_tile_map_area() {
                    bg_tilemap_base = 0x1c00;
                }
                let bg_tile_x = (self.regs.scroll_x / 8).wrapping_add(*current_x) & 0x1f;
                let bg_tile_y = (self.regs.scroll_y / 8).wrapping_add(self.regs.y_coordinate) & 0x1f;
                let bg_tilemap_index = ((bg_tile_y as u16) * 32) + (bg_tile_x as u16);
                let bg_tile_index = self.mem.ppu_vram_read(bg_tilemap_base + bg_tilemap_index);
                let bg_tile_base_address = if self.regs.control.bg_window_tile_data_area() {
                    0x0000 + (bg_tile_index as u16 * 8)
                } else if bg_tile_index < 128 {
                    0x1000 + (bg_tile_index as u16 * 8)
                } else {
                    0x0800 + (bg_tile_index as u16 * 8)
                };
                let x_offset = self.regs.scroll_x.wrapping_add(*current_x) % 8;
                let y_offset = self.regs.scroll_y.wrapping_add(self.regs.y_coordinate) % 8 * 2;
                let bg_tile_byte_1 = self.mem.ppu_vram_read(bg_tile_base_address + y_offset as u16);
                let bg_tile_byte_2 = self.mem.ppu_vram_read(bg_tile_base_address + y_offset as u16 + 1);
                let bg_tile_shift = 7 - x_offset;
                let bg_tile_palette_index = 0;
                let bg_color_low = self.mem.color_bg_palette_data[bg_tile_palette_index as usize * 2];
                let bg_color_high = self.mem.color_bg_palette_data[bg_tile_palette_index as usize * 2 + 1];
                let bg_color = ((bg_color_high as u16) << 8) | (bg_color_low as u16);
                let bg_r = (bg_color >> 0) as u8 & 0x1f;
                let bg_g = (bg_color >> 5) as u8 & 0x1f;
                let bg_b = (bg_color >> 10) as u8 & 0x1f;

                // TODO: handle window and sprites
                let (r, g, b) = (bg_r, bg_g, bg_b);

                state.pixel_fifo.push_back((r, g, b));
                *current_x += 1;

                assert!(
                    dots_this_scanline <= 369,
                    "Overran dot budget in mode 3! Expected less than 370 dots, found {} instead!",
                    dots_this_scanline,
                );

                if *current_x >= 160 {
                    let dots_remaining = 454 - dots_this_scanline;
                    state.mode = Mode::HBlank { dots_remaining: dots_remaining.try_into().unwrap() };
                }
            }
        }

        if let Some(true) = pins.get_lcd_accepted__opt() {
            info!("PPU saw LCD Controller accepted a pixel");
            state.pixel_fifo.pop_front();
        }
        if let Some((r, g, b)) = state.pixel_fifo.front() {
            pins.set_lcd_r(*r);
            pins.set_lcd_g(*g);
            pins.set_lcd_b(*b);
            pins.set_lcd_push(true);
        } else {
            pins.set_lcd_r__opt(None);
            pins.set_lcd_g__opt(None);
            pins.set_lcd_b__opt(None);
            pins.set_lcd_push(false);
        }

        state.frame_dot_counter += 1;
    }

    fn get_mode(&self) -> ModeBits {
        match &self.state {
            None => ModeBits::VBlank,
            Some(s) => match &s.mode {
                Mode::HBlank { .. } => ModeBits::HBlank,
                Mode::VBlank { .. } => ModeBits::VBlank,
                Mode::SearchingOAM { .. } => ModeBits::SearchingOAM,
                Mode::TransferToLcdController { .. } => ModeBits::TransferToLcdController,
            }
        }
    }

    pub fn entering_stop(&mut self) {
        // TODO
        // self.internal_vram_read_enabled =
    }

    pub fn exiting_stop(&mut self) {
        // TODO
        // self.internal_vram_read_enabled =
    }

    pub fn oam_read(&self, index: u8) -> u8 {
        match self.get_mode() {
            ModeBits::SearchingOAM | ModeBits::TransferToLcdController => 0xff,
            ModeBits::HBlank | ModeBits::VBlank => self.mem.oam[index as usize],
        }
    }

    pub fn oam_write(&mut self, index: u8, data: u8) {
        let mode = self.get_mode();
        match mode {
            ModeBits::SearchingOAM | ModeBits::TransferToLcdController =>
                warn!(
                    "Ignoring OAM write of {:#04x} to index {:#04x} in Mode {}",
                    data, index, mode as u8,
                ),
            ModeBits::HBlank | ModeBits::VBlank => self.mem.oam[index as usize] = data,
        }
    }

    pub fn oam_write_dma(&mut self, index: u8, data: u8) {
        // DMA can change OAM even when it's normally locked.
        self.mem.oam[index as usize] = data;
    }

    pub fn vram_read(&self, index: u16) -> u8 {
        match self.get_mode() {
            ModeBits::TransferToLcdController => 0xff,
            _ if self.mem.vram_bank_1_active => self.mem.vram_bank_1[index as usize],
            _ => self.mem.vram_bank_0[index as usize],
        }
    }

    pub fn vram_write(&mut self, index: u16, data: u8) {
        match self.get_mode() {
            ModeBits::TransferToLcdController =>
                debug!("Ignoring VRAM write of {:#04x} to index {:#06x} in Mode 3", data, index),
            _ if self.mem.vram_bank_1_active => self.mem.vram_bank_1[index as usize] = data,
            _ => self.mem.vram_bank_0[index as usize] = data,
        }
    }

    pub fn read_register(&self, reg: u8) -> u8 {
        debug!("CPU is reading PPU register {:#04x}", reg);
        match reg {
            0x40 => self.regs.control.0,
            0x41 => {
                let mut s = self.regs.status;
                s.set_mode_flag(self.get_mode() as u8);
                s.0
            }
            0x42 => self.regs.scroll_y,
            0x43 => self.regs.scroll_x,
            0x44 => self.regs.y_coordinate,
            0x45 => self.regs.y_compare,
            0x47 => self.regs.bw_bg_palette_data,
            0x48 => self.regs.bw_obj_palette_0_data,
            0x49 => self.regs.bw_obj_palette_1_data,
            0x4a => self.regs.window_y_position,
            0x4b => self.regs.window_x_position,
            0x68 => self.regs.color_bg_palette_index,
            // TODO: palette data registers are blocked in mode 3
            0x69 => self.mem.color_bg_palette_data[(self.regs.color_bg_palette_index & 0x3F) as usize],
            0x6a => self.regs.color_obj_palette_index,
            0x6b => self.mem.color_obj_palette_data[(self.regs.color_obj_palette_index & 0x3F) as usize],
            _ => panic!("Invalid PPU register ID {:#04x}!", reg),
        }
    }

    pub fn write_register(&mut self, reg: u8, data: u8) {
        debug!("Setting PPU register {:#04x} to {:#04x}", reg, data);
        match reg {
            0x40 => {
                self.regs.control.0 = data;
                if self.regs.control.lcd_enable() && self.state.is_none() {
                    self.state = Some(State {
                        frame_dot_counter: 0,
                        pixel_fifo: VecDeque::with_capacity(160),
                        mode: Mode::VBlank { dots_remaining: 0 },
                    });
                }
            }
            0x41 => self.regs.status.0 = (data & 0xf8) | (self.regs.status.0 & 0b0111),
            0x42 => self.regs.scroll_y = data,
            0x43 => self.regs.scroll_x = data,
            0x44 => warn!("Ignoring write to PPU register LY"),
            0x45 => self.regs.y_compare = data,
            0x47 => self.regs.bw_bg_palette_data = data,
            0x48 => self.regs.bw_obj_palette_0_data = data & 0xfc,
            0x49 => self.regs.bw_obj_palette_1_data = data & 0xfc,
            0x4a => self.regs.window_y_position = data,
            0x4b => self.regs.window_x_position = data,
            0x68 => self.regs.color_bg_palette_index = data & 0b0101_1111,
            // TODO: palette data registers are blocked in mode 3
            0x69 => {
                let slot = &mut self.mem.color_bg_palette_data[(self.regs.color_bg_palette_index & 0x3F) as usize];
                *slot = data;
                if self.regs.color_bg_palette_index & (1 << 7) != 0 {
                    self.regs.color_bg_palette_index = (self.regs.color_bg_palette_index + 1) & 0b0101_1111;
                }
            }
            0x6a => self.regs.color_obj_palette_index = data & 0b0101_1111,
            0x6b => {
                let slot = &mut self.mem.color_obj_palette_data[(self.regs.color_obj_palette_index & 0x3F) as usize];
                *slot = data;
                if self.regs.color_obj_palette_index & (1 << 7) != 0 {
                    self.regs.color_obj_palette_index = (self.regs.color_obj_palette_index + 1) & 0b0101_1111;
                }
            }
            _ => panic!("Invalid PPU register ID {:#04x}!", reg),
        }
    }
}
