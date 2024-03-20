use crate::{
    cart::Cart,
    cpu::{CPU, InterruptFlags, SpeedSwitchFlags, WriteRecord},
    lcd::{LcdController, LcdControlFlags, LcdModeFlag, LcdStatusFlags},
    memory::{Memory, MemoryMapChange as MMC},
    screen::Screen,
    sound::{APU, SoundOnFlags},
};

#[derive(Clone, Debug)]
pub struct System {
    pub(crate) cart: Cart,
    pub(crate) cpu: CPU,
    pub(crate) lcd: LcdController,
    pub(crate) memory: Memory,
    pub(crate) screen: Screen,
    pub(crate) sound: APU,
    pub(crate) cycles: u64,
}

impl System {
    pub fn new(cart: Cart) -> Self {
        let mut memory = Memory::new();
        if cart.skip_boot_rom {
            memory.boot_rom = None;
        }
        Self {
            cart,
            cpu: CPU::new(),
            lcd: LcdController::new(),
            memory,
            screen: Screen::new(),
            sound: APU::default(),
            cycles: 0,
        }
    }

    #[inline]
    pub fn step(&mut self) -> u64 {
        let cycles_4mhz = self.cpu.step(
            &mut self.cart,
            &mut self.lcd,
            &mut self.memory,
            &mut self.screen,
            &mut self.sound,
        );
        self.lcd.step(
            cycles_4mhz,
            &mut self.cart,
            &mut self.cpu,
            &mut self.memory,
            &mut self.screen,
            &mut self.sound,
        );
        self.sound.step(
            cycles_4mhz,
            &mut self.cart,
            &mut self.cpu,
            &mut self.lcd,
            &mut self.memory,
            &mut self.screen,
        );
        self.cycles = cycles_4mhz;
        cycles_4mhz
    }

    #[inline]
    pub fn get_cpu(&self) -> &CPU {
        &self.cpu
    }

    #[inline]
    pub fn get_cpu_mut(&mut self) -> &mut CPU {
        &mut self.cpu
    }

    #[inline]
    pub fn get_screen(&self) -> &Screen {
        &self.screen
    }

    #[cfg(test)]
    pub(crate) fn as_ref(&mut self) -> SystemRef<'_> {
        SystemRef {
            cpu: &mut self.cpu,
            cart: &mut self.cart,
            lcd: &mut self.lcd,
            memory: &mut self.memory,
            screen: &mut self.screen,
            sound: &mut self.sound,
        }
    }

    pub fn run_to_vblank(&mut self) -> u64 {
        // Run until end of vblank, if we're already in vblank.
        let mut last_pc: u32 = 0x10000;
        let mut pc_loop = false;
        let mut boot_rom_labels = std::collections::HashMap::new();
        if self.memory.boot_rom.is_some() {
            boot_rom_labels.insert(0x0000, "sub_0000");
            boot_rom_labels.insert(0x007c, "System_Setup");
            boot_rom_labels.insert(0x0097, "Load_Logo_into_RAM_VRAM");
            boot_rom_labels.insert(0x0275, "Setup_sound");
            boot_rom_labels.insert(0x028b, "_loop_clear_wave_data");
            boot_rom_labels.insert(0x0290, "_loop_clear_wave_data+5");
        }

        macro_rules! log_pc {
            ($msg:literal $(, $arg:tt)*) => {
                debug!($msg $(, $arg)*);
            };
        }

        while self.lcd.registers.status.mode_flag() == LcdModeFlag::VBlank {
            self.step();
            // if self.memory.boot_rom.is_some() {
            //     let current_pc = self.cpu.registers.pc;
            //     let pc_label: String = match boot_rom_labels.get(&current_pc) {
            //         Some(l) => l.to_string(),
            //         None => format!("{:#06x}", current_pc),
            //     };
            //     let current_pc = current_pc as u32;
            //     if current_pc != last_pc {
            //         log_pc!("PC is now {}", pc_label);
            //         last_pc = current_pc;
            //         pc_loop = false;
            //     } else if !pc_loop {
            //         log_pc!("PC looping at {}", pc_label);
            //         pc_loop = true;
            //     }
            // }
        }

        // Now that the flag is disabled, run until the LCD controller sets it.
        // Note that if the LCD controller is turned off, then this may run for
        // a long time.
        while self.lcd.registers.status.mode_flag() != LcdModeFlag::VBlank {
            self.step();
            if self.memory.boot_rom.is_some() {
                let current_pc = self.cpu.registers.pc;
                let pc_label: String = match boot_rom_labels.get(&current_pc) {
                    Some(l) => l.to_string(),
                    None => format!("{:#06x}", current_pc),
                };
                let current_pc = current_pc as u32;
                if current_pc != last_pc {
                    log_pc!("PC is now {}", pc_label);
                    last_pc = current_pc;
                    pc_loop = false;
                } else if !pc_loop {
                    log_pc!("PC looping at {}", pc_label);
                    pc_loop = true;
                }
            }
        }
        self.cycles
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum RwSource {
    CPU,
    DMA,
    PPU,
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct SystemRef<'a> {
    pub(crate) cart: &'a mut Cart,
    pub(crate) cpu: &'a mut CPU,
    pub(crate) lcd: &'a mut LcdController,
    pub(crate) memory: &'a mut Memory,
    pub(crate) screen: &'a mut Screen,
    pub(crate) sound: &'a mut APU,
}

impl SystemRef<'_> {
    pub(crate) fn read(&mut self, address: u16, source: RwSource) -> Option<u8> {
        use RwSource::*;

        match (self.memory.boot_rom.as_ref(), self.cpu.dma_active(), source, address) {
            // Boot ROM is overlaid on top of regular memory.
            (Some(rom), _, _, 0x0000..=0x00ff | 0x0150..=Memory::BOOT_ROM_HIGH_ADDR) => return Some(rom[address as usize]),
            (Some(rom), _, _, 0x0100..=0x014f) => (),
            (_, false, CPU, _) => (),
            (_, true, CPU, 0xff80..=0xfffe) => (),
            (_, true, CPU, _) => {
                panic!("CPU tried to read regular memory at {:#06x} during DMA", address);
                return None;
            }
            (_, false, DMA, _) => {
                panic!("Found DMA trying to read {:#06x} when DMA isn't active!", address);
            }
            (_, true, DMA, 0x0000..=0xdf9f) => (),
            (_, true, DMA, 0xdfa0..) => {
                panic!("DMA attempted to read {:#06x} above 0xdf9f", address);
                return None;
            }
            (_, _, PPU, _) => {
                todo!("Read of {:#06x} from PPU", address);
            }
        }

        match address {
            0x0000..=0x7fff => self.cart.read(address),
            0x8000..=0x9fff => self.memory.read(address),
            0xa000..=0xbfff => self.cart.read(address),
            0xc000..=0xdfff => self.memory.read(address),
            0xe000..=0xfdff => self.memory.read(address - 0x2000),
            0xfe00..=0xfe9f if self.lcd.oam_inaccessible() => {
                debug!("Attempted to read from OAM while VRAM is blocked");
                Some(0xff)
            }
            0xfe00..=0xfe9f => self.lcd.registers.object_attr_memory.read(address),
            0xfea0..=0xfeff => {
                error!("TODO: Read from prohibited area! (address = {:#06x})", address);
                None
            }
            0xff00 => {
                error!("TODO: read from JOYP IO register");
                None

            }
            0xff0f => Some(self.cpu.registers.interrupt_flags.bits()),
            0xff42 => Some(self.lcd.registers.scroll_y),
            0xff43 => Some(self.lcd.registers.scroll_x),
            0xff44 => Some(self.lcd.registers.lcd_current_y),
            0xff45 => Some(self.lcd.registers.lcd_compare_y),
            0xff4a => Some(self.lcd.registers.window_y),
            0xff4b => Some(self.lcd.registers.window_x),
            0xff01..=0xff7f => {
                error!("TODO: Read from IO register! (address = {:#06x})", address);
                None
            }
            0xff80..=0xfffe => self.memory.read(address),
            0xffff => Some(self.cpu.registers.interrupt_enable.bits()),
        }
    }

    pub(crate) fn write(&mut self, address: u16, data: u8, source: RwSource) {
        self.cpu.writes.push(WriteRecord {
            target_address: address,
            data: data,
        });

        // if self.memory.boot_rom.is_some() && address >= 0xff00 {
        //     info!("{:?} is writing {:#04x} to {:#06x}", source, data, address);
        // }

        #[inline]
        fn io_reg<T: std::fmt::LowerHex>(label: &str, slot: &mut T, new: T) -> Option<MMC> {
            debug!("Writing {:#04x} to IO register {}", new, label);
            *slot = new;
            None
        }

        #[inline]
        fn io_reg_partial_write<T>(label: &str, slot: &mut T, new: T, mask: T) -> Option<MMC>
        where
            T: std::ops::BitOr<Output = T> + std::fmt::LowerHex,
        {
            let new = new | mask;
            debug!("Writing {:#04x} to IO register {}", new, label);
            *slot = new;
            None
        }

        macro_rules! ignored_io_reg {
            ($msg:literal, $data:expr) => {{
                warn!("TODO: Write to {} IO register (addr = {:#06x}, data = {:?})", $msg, address, $data);
                None
            }};
            ($msg:literal) => {{
                warn!("TODO: Write to {} IO register (addr = {:#06x}, data = {:#04x})", $msg, address, data);
                None
            }};
        }

        use RwSource::*;

        // handle special cases
        let draw_mode = self.lcd.registers.status.mode_flag();
        match (self.memory.boot_rom.as_ref(), self.cpu.dma_active(), draw_mode, source, address) {
            (Some(rom), _, _, _, 0x0000..=Memory::BOOT_ROM_HIGH_ADDR) => todo!("Handle write of {:#04x} to {:#06x} during boot ROM", data, address),
            (_, _, LcdModeFlag::LCDTransfer, CPU | DMA, 0x8000..=0x9fff) => {
                panic!("{:?} tried to write {:#04x} to VRAM ({:#06x}) during Mode 3", source, data, address);
                return;
            }
            (_, _, LcdModeFlag::OAM | LcdModeFlag::LCDTransfer, CPU, 0xfe00..=0xfe9f) => {
                panic!("CPU tried to write {:#04x} to OAM ({:#06x}) during {}", data, address, draw_mode);
                return;
            }
            (_, _, _, CPU, 0xfe00..=0xfe9f) => (),
            (_, false, _, CPU, _) => (),
            (_, true, _, CPU, _) => {
                panic!("CPU tried to write {:#04x} to {:#06x} during DMA", data, address);
                return;
            }
            (_, false, _, DMA, _) => panic!("DMA tried to write {:#04x} to {:#06x} while it isn't active?", data, address),
            (_, true, _, DMA, 0xfe00..=0xfe9f) => (),
            (_, true, _, DMA, _) => panic!("DMA tried to write {:#04x} to {:#06x} outside range 0xfe00..=0xfe9f", data, address),
            (_, _, _, PPU, _) => (),
        }

        let change = match address {
            0x0000..=0x3fff => self.cart.write(address, data),
            0x4000..=0x7fff => self.cart.write(address, data),
            0x8000..=0x9fff => self.memory.write(address, data),
            0xa000..=0xbfff => self.cart.write(address, data),
            0xc000..=0xdfff => self.memory.write(address, data),
            0xe000..=0xfdff => self.memory.write(address - 0x2000, data),
            0xfe00..=0xfe9f if self.lcd.oam_inaccessible() => {
                debug!("Attempted to write to OAM while VRAM is blocked");
                None
            }
            0xfe00..=0xfe9f => self.lcd.registers.object_attr_memory.write(address, data),
            0xfea0..=0xfeff => {
                error!("TODO: Write to prohibited area! (address = {:#06x}, data = {:#04x})", address, data);
                None
            }
            0xff00 => {
                warn!("Tried to write to read-only JOYP IO register");
                None
            }
            0xff01 => ignored_io_reg!("SB"),
            0xff02 => ignored_io_reg!("SC", data & 0x83),
            0xff0f => io_reg("IF (interrupt flags)", &mut self.cpu.registers.interrupt_flags, InterruptFlags::from_bits_truncate(data)),
            0xff10 => io_reg("NR10", &mut self.sound.registers.nr10, data),
            0xff11 => io_reg("NR11", &mut self.sound.registers.nr11, data), // Channel 1 Sound Length/Wave Pattern
            0xff12 => io_reg("NR12", &mut self.sound.registers.nr12, data), // Channel 1 Volume Envelope
            0xff13 => io_reg("NR13", &mut self.sound.registers.nr13, data), // Channel 1 Frequency Low
            0xff14 => io_reg("NR14", &mut self.sound.registers.nr14, data), // Channel 1 Frequency High
            0xff15 => None, // unused
            0xff16 => io_reg("NR21", &mut self.sound.registers.nr21, data), // Channel 2 Sound Length/Wave Pattern
            0xff17 => io_reg("NR22", &mut self.sound.registers.nr22, data), // Channel 2 Volume Envelope
            0xff18 => io_reg("NR23", &mut self.sound.registers.nr23, data), // Channel 2 Frequency Low
            0xff19 => io_reg("NR24", &mut self.sound.registers.nr24, data), // Channel 2 Frequency High
            0xff1a => io_reg("NR30", &mut self.sound.registers.nr30, data), // Channel 3 Sound on/off
            0xff1b => io_reg("NR31", &mut self.sound.registers.nr31, data), // Channel 3 Sound Length
            0xff1c => io_reg("NR32", &mut self.sound.registers.nr32, data), // Channel 3 Select output level
            0xff1d => io_reg("NR33", &mut self.sound.registers.nr33, data), // Channel 3 Frequency Low
            0xff1e => io_reg("NR34", &mut self.sound.registers.nr34, data), // Channel 3 Frequency High
            0xff1f => None, // unused
            0xff20 => io_reg("NR41", &mut self.sound.registers.nr41, data), // Channel 4 Sound Length
            0xff21 => io_reg("NR42", &mut self.sound.registers.nr42, data), // Channel 4 Volume Envelope
            0xff22 => io_reg("NR43", &mut self.sound.registers.nr43, data), // Channel 4 Polynomial Counter
            0xff23 => io_reg("NR44", &mut self.sound.registers.nr44, data), // Channel 4 Counter/consecutive
            0xff24 => io_reg("NR50", &mut self.sound.registers.nr50, data), // Channel control / on-off / Volume
            0xff25 => io_reg("NR51", &mut self.sound.registers.nr51, data), // Selection of Sound output terminal
            0xff26 => io_reg("NR52", &mut self.sound.registers.nr52, SoundOnFlags::from_bits_truncate(data)), // Sound on/off
            0xff30..=0xff3f => {
                let index = (address as usize) & 0xf;
                io_reg("wave pattern ram", &mut self.sound.registers.wave_pattern_ram[index], data)
            }
            0xff40 => {
                let data = LcdControlFlags::from_bits_truncate(data);
                info!("Setting LCD control register to {:?}", data);
                io_reg("LCDC", &mut self.lcd.registers.control, data)
            }
            0xff41 => io_reg_partial_write(
                "LCDS",
                &mut self.lcd.registers.status,
                LcdStatusFlags::WRITABLE_MASK,
                LcdStatusFlags::from_bits_truncate(data),
            ),
            0xff42 => io_reg("SCRY", &mut self.lcd.registers.scroll_y, data),
            0xff43 => io_reg("SCRX", &mut self.lcd.registers.scroll_x, data),
            0xff44 => {
                warn!("Attempted to write to LY IO register");
                None
            }
            0xff45 => io_reg("LYC", &mut self.lcd.registers.lcd_compare_y, data),
            0xff46 => {
                self.cpu.start_dma(data);
                Some(MMC::InvalidateRange(0x0000..=0xff7f))
            }
            0xff47 => io_reg("BGP", &mut self.lcd.registers.bg_palette_data, data),
            0xff48 => io_reg("OBP0", &mut self.lcd.registers.obj_palette_data_0, data),
            0xff49 => io_reg("OBP1", &mut self.lcd.registers.obj_palette_data_1, data),
            0xff4a => io_reg("WY", &mut self.lcd.registers.window_y, data),
            0xff4b => io_reg("WX", &mut self.lcd.registers.window_x, data),
            0xff4d => {
                let bit = data & SpeedSwitchFlags::PREPARE_SWITCH.bits();
                let new = SpeedSwitchFlags::from_bits_truncate(bit);
                MMC::invalidate_one(address, &mut self.cpu.registers.key1, new)
            }
            0xff4f => ignored_io_reg!("VRAM Bank", data & 1),
            0xff50 => {
                // Special: disable the boot rom.
                if let Some(_) = self.memory.boot_rom.take() {
                    Some(MMC::InvalidateAll)
                } else {
                    None
                }
            }
            0xff51 => ignored_io_reg!("HDMA1"),
            0xff52 => ignored_io_reg!("HDMA2"),
            0xff53 => ignored_io_reg!("HDMA3"),
            0xff54 => ignored_io_reg!("HDMA4"),
            0xff55 => ignored_io_reg!("HDMA5"),
            0xff56 => ignored_io_reg!("RP (Infrared Comm. Port)"),
            0xff68 => self.lcd.registers.bg_color_palette.write_spec(data),
            0xff69 => self.lcd.registers.bg_color_palette.write_data(data),
            0xff6a => self.lcd.registers.obj_color_palette.write_spec(data),
            0xff6b => self.lcd.registers.obj_color_palette.write_data(data),
            0xff6c => ignored_io_reg!("OPRI (Object Priority Mode)"),
            0xff70 => match data {
                0 => MMC::invalidate_range(0xd000..=0xdfff, &mut self.memory.work_ram_bank, 1),
                1..=7 => MMC::invalidate_range(0xd000..=0xdfff, &mut self.memory.work_ram_bank, data as usize),
                _ => None,
            },
            0xff72 => ignored_io_reg!("UNK $FF72"),
            0xff73 => ignored_io_reg!("UNK $FF73"),
            0xff74 => ignored_io_reg!("UNK $FF74"),
            0xff75 => ignored_io_reg!("UNK $FF75", data & 0b0111_0000),
            0xff76 => {
                warn!("Tried to write to read-only PCM12 register");
                None
            }
            0xff77 => {
                warn!("Tried to write to read-only PCM34 register");
                None
            }
            0xff00..=0xff7f => {
                error!("Write to UNKNOWN IO register (address = {:#06x}, data = {:#04x})", address, data);
                None
            },
            0xff80..=0xfffe => self.memory.write(address, data),
            0xffff => MMC::invalidate_one(
                address,
                &mut self.cpu.registers.interrupt_enable,
                InterruptFlags::from_bits_truncate(data),
            ),
        };
        if let Some(change) = change {
            self.cpu.memory_map_changes.push(change);
        }
    }
}
