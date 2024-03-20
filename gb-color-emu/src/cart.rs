use log::*;
use std::{borrow::Cow, fmt, num::NonZeroU8};

pinout! {
    pub struct CartPinout {
        in address: u16,
        inout data: u8,
        in wr: bool,
        in rd: bool,
        in cs: bool,
        in res: bool,
        in vin: bool,
    }
}

impl CartPinout {
    pub fn new() -> Self {
        Self::default()
    }

    // pub fn update_from_cpu_pins(&mut self, cpu_pins: &SM83Pinout) {
    //     trace!("Updating Cart pins from CPU pins {:?}", cpu_pins);
    //     self.set_address(cpu_pins.get_address());
    //     self.set_data(cpu_pins.get_data());
    //     self.set_wr(cpu_pins.get_wr());
    //     self.set_rd(cpu_pins.get_rd());
    //     self.set_cs(cpu_pins.get_cs());
    //     self.set_res(cpu_pins.get_reset());
    //     self.set_vin(cpu_pins.get_vin());
    // }
}

#[derive(Clone)]
pub enum MBCRAM {
    NoRam,
    Disabled(Vec<u8>),
    Enabled(Vec<u8>),
}

impl fmt::Debug for MBCRAM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoRam => write!(f, "<none>"),
            Self::Disabled(ram) => write!(f, "{} bytes (disabled)", ram.len()),
            Self::Enabled(ram) => write!(f, "{} bytes (enabled)", ram.len()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MBC {
    MBC1 {
        bank: NonZeroU8,
        ram: MBCRAM,
    },
    MBC5 {
        rom_bank: u16,
        ram_bank: u8,
        ram: MBCRAM,
    }
}

// #[derive(Copy, Clone, Debug)]
// pub enum State {
//     Ready,
//     Reading { addr: u16 },
//     Writing { addr: u16, data: u8 },
// }

#[derive(Clone)]
pub struct Cart<'rom> {
    pub rom: Cow<'rom, [u8]>,
    pub mbc: Option<MBC>,
    pub entry_point: u16,
    // pub state: State,
}

impl fmt::Debug for Cart<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Cart")
            .field("mbc", &self.mbc)
            // .field("state", &self.state)
            .field("rom", &format_args!("({} bytes)", self.rom.len()))
            .finish()
    }
}

// CPU runs at 240ns per cycle
//
// Cart clock is inverted every two CPU cycles

impl<'rom> Cart<'rom> {
    pub fn new(rom: impl Into<Cow<'rom, [u8]>>) -> Self {
        let rom = rom.into();
        let mbc = match rom.get(0x0147).copied().unwrap_or(0) {
            0x00 => None,
            0x01 => Some(MBC::MBC1 { bank: NonZeroU8::new(1).unwrap(), ram: MBCRAM::NoRam }),
            0x02 => Some(MBC::MBC1 { bank: NonZeroU8::new(1).unwrap(), ram: MBCRAM::Disabled(vec![0x00; 0x2000]) }),
            0x1b => Some(MBC::MBC5 {
                rom_bank: 1,
                ram_bank: 0,
                ram: MBCRAM::Disabled(vec![0x00; 128 * 1024]),
            }),
            other => panic!("Unknown cartridge type {:#04x}!", other),
        };
        Self { rom, mbc, entry_point: 0x0100 }
    }

    pub fn new_no_mbc(rom: impl Into<Cow<'rom, [u8]>>) -> Self {
        Self {
            rom: rom.into(),
            mbc: None,
            entry_point: 0,
        }
    }

    pub fn step_cycle(&mut self, pins: &mut CartPinout) {
        pins.release_data();
        if let None | Some(true) = pins.get_cs__opt() {
            trace!("CS pin high in Cart::step_cycle, doing nothing");
            // self.state = State::Ready;
            return;
        }
        if !pins.get_rd() {
            let addr = pins.get_address();
            let value = self.read_byte(addr);
            trace!("Cart outputting value {:#04x} read from {:#06x}", value, addr);
            pins.set_data(value);
        } else if !pins.get_wr() {
            let addr = pins.get_address();
            let data = pins.get_data();
            self.write_byte(data, addr);
        }
        trace!("Cart tick finished, pins = {:?}", pins);
        // trace!("Cart tick finished, state = {:?}", self.state);
    }

    fn read_rom_byte(&self, address: u16) -> u8 {
        match self.rom.get(address as usize) {
            Some(b) => *b,
            None => panic!(
                "Accessed byte outside of ROM range. Rom is {:#06x} long, and requested address was {:#06x}.",
                self.rom.len(),
                address,
            ),
        }
    }

    fn read_ram_byte(ram: &[u8], address: u16) -> u8 {
        match ram.get(address as usize) {
            Some(b) => *b,
            None => panic!(
                "Accessed byte outside of RAM range. RAM is {:#06x} long, and requested index was {:#06x}.",
                ram.len(),
                address,
            ),
        }
    }

    #[allow(unused)]
    fn write_ram_byte(ram: &mut [u8], address: u16, data: u8) {
        match ram.get_mut(address as usize) {
            Some(b) => *b = data,
            None => panic!(
                "Tried to write byte {:#04x} outside of RAM range. RAM is {:#06x} long, and requested index was {:#06x}.",
                data,
                ram.len(),
                address,
            ),
        }
    }

    fn read_byte(&self, address: u16) -> u8 {
        trace!("Reading byte at {:#06x} in cart {:?}", address, self);
        match self.mbc.as_ref() {
            None => self.read_rom_byte(address),
            Some(MBC::MBC1 { bank, ram }) => match address {
                0x0000..=0x3fff => self.read_rom_byte(address),
                0x4000..=0x7fff => {
                    let address = 0x4000 + ((address - 0x4000) * bank.get() as u16);
                    self.read_rom_byte(address)
                }
                0xa000..=0xbfff => match ram {
                    MBCRAM::Enabled(ram) => Self::read_ram_byte(&ram, address - 0xa000),
                    MBCRAM::Disabled(_) | MBCRAM::NoRam => 0x00,
                }
                _ => panic!("Tried to read address {:#06x} out of range for MBC1 cart!", address),
            },
            Some(MBC::MBC5 { rom_bank, ram_bank, ram }) => match address {
                0x0000..=0x3fff => self.read_rom_byte(address),
                0x4000..=0x7fff => {
                    let address = address - 0x4000 + (0x4000 * rom_bank);
                    self.read_rom_byte(address)
                },
                0xa000..=0xbfff => match ram {
                    MBCRAM::Enabled(ram) => {
                        let address = address - 0xa000 + (0x2000 * (*ram_bank) as u16);
                        Self::read_ram_byte(&ram, address)
                    }
                    MBCRAM::Disabled(_) | MBCRAM::NoRam => 0x00,
                }
                _ => panic!("Tried to read address {:#06x} out of range for MBC5 cart!", address),
            }
        }
    }

    fn write_byte(&mut self, byte: u8, address: u16) {
        trace!("Writing byte {:#04x} at {:#06x} in cart {:?}", byte, address, self);
        match self.mbc.as_mut() {
            None => match self.rom.to_mut().get_mut(address as usize) {
                Some(slot) => *slot = byte,
                None => panic!(
                    "Tried to write byte outside of ROM range. Rom is {:#06x} long, and requested address was {:#06x}.",
                    self.rom.len(),
                    address,
                ),
            }
            Some(MBC::MBC1 { bank, ram }) => match address {
                0x0000..=0x1fff => match (byte & 0xf == 0xa, &mut *ram) {
                    (_, MBCRAM::NoRam) => (),
                    (true, MBCRAM::Enabled(_)) => (),
                    (true, MBCRAM::Disabled(disabled_ram)) => {
                        let contents = std::mem::replace(disabled_ram, vec![]);
                        *ram = MBCRAM::Enabled(contents);
                    }
                    (false, MBCRAM::Enabled(enabled_ram)) => {
                        let contents = std::mem::replace(enabled_ram, vec![]);
                        *ram = MBCRAM::Disabled(contents);
                    }
                    (false, MBCRAM::Disabled(_)) => (),
                },
                0x2000..=0x3fff => {
                    *bank = NonZeroU8::new((byte & 0x1F).max(0x01)).unwrap();
                },
                _ => todo!("Cart::write_byte with MBC1, address = {:#06x}", address),
            }
            Some(MBC::MBC5 { rom_bank, ram_bank, ram }) => match address {
                0x0000..=0x1fff => match (byte & 0xf == 0xa, &mut *ram) {
                    (_, MBCRAM::NoRam) => (),
                    (true, MBCRAM::Enabled(_)) => (),
                    (true, MBCRAM::Disabled(disabled_ram)) => {
                        let contents = std::mem::replace(disabled_ram, vec![]);
                        *ram = MBCRAM::Enabled(contents);
                    }
                    (false, MBCRAM::Enabled(enabled_ram)) => {
                        let contents = std::mem::replace(enabled_ram, vec![]);
                        *ram = MBCRAM::Disabled(contents);
                    }
                    (false, MBCRAM::Disabled(_)) => (),
                },
                0x2000..=0x2fff => *rom_bank = (*rom_bank & 0x100) | (byte as u16),
                0x3000..=0x3fff => *rom_bank = ((byte as u16 & 1) << 8) | (*rom_bank & 0xff),
                0x4000..=0x5fff => *ram_bank = byte & 0x1f,
                // RAM write
                0xa000..=0xbfff => match ram {
                    MBCRAM::NoRam => (),
                    MBCRAM::Disabled(_) => (),
                    MBCRAM::Enabled(ref mut ram) => {
                        let ram_addr = (address - 0xa000) + (0x2000 * (*ram_bank) as u16);
                        Self::write_ram_byte(ram, ram_addr, byte);
                    }
                },
                _ => todo!("Cart::write_byte with MBC5, address = {:#06x}", address),
            }
        }
    }
}


