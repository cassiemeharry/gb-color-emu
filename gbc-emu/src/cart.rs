use std::ops::{Bound::*, RangeBounds};

use crate::memory::MemoryMapChange;

#[derive(Clone, Debug)]
pub struct Cart {
    rom: Vec<u8>,
    ram: Vec<u8>,
    mbc: MBC,
    pub(crate) skip_boot_rom: bool
}

#[derive(Clone, Debug)]
enum MBC {
    MBC1 {
        ram_enable: bool,
        ram_bank_number: u8,
        rom_bank_number: u8,
        banking_mode_select: bool,
    }
}

impl Cart {
    pub fn load_rom(mut f: impl std::io::Read) -> std::io::Result<Self> {
        let mut rom = Vec::with_capacity(1024);
        f.read_to_end(&mut rom)?;
        let mbc = match rom[0x147] {
            0x02 => MBC::MBC1 {
                ram_enable: false,
                ram_bank_number: 0,
                rom_bank_number: 1,
                banking_mode_select: false,
            },
            x => panic!("Unknown cartridge type {:#04x}", x),
        };
        println!("Loaded cartridge with type {:?}", mbc);
        let ram = vec![0x00; 0x2000];
        Ok(Self {
            rom,
            ram,
            mbc,
            skip_boot_rom: false,
        })
    }

    pub fn new_raw(rom: impl Into<Vec<u8>>) -> Self {
        Self {
            rom: rom.into(),
            ram: vec![],
            mbc: MBC::MBC1 {
                ram_enable: false,
                ram_bank_number: 0,
                rom_bank_number: 1,
                banking_mode_select: false,
            },
            skip_boot_rom: true,
        }
    }

    pub fn read_slice(&self, range: impl RangeBounds<u16>) -> &[u8] {
        let (start, end) = match (range.start_bound(), range.end_bound()) {
            (Unbounded, Unbounded) => (0, self.rom.len()),
            (Unbounded, Included(y)) => (0, *y as usize + 1),
            (Unbounded, Excluded(y)) => (0, *y as usize),
            (Included(x), Unbounded) => (*x as usize, self.rom.len()),
            (Included(x), Included(y)) => (*x as usize, *y as usize + 1),
            (Included(x), Excluded(y)) => (*x as usize, *y as usize + 1),
            (Excluded(x), Unbounded) => (*x as usize + 1, self.rom.len()),
            (Excluded(x), Included(y)) => (*x as usize + 1, *y as usize + 1),
            (Excluded(x), Excluded(y)) => (*x as usize + 1, *y as usize + 1),
        };
        if start > self.rom.len() {
            &[]
        } else {
            &self.rom[start..end]
        }
    }

    pub fn read(&mut self, address: u16) -> Option<u8> {
        // error!("Faking cart read from address {:#06x}", address);
        let data_opt = match &self.mbc {
            MBC::MBC1 { ram_enable, ram_bank_number, rom_bank_number, .. } => match address {
                0x0000..=0x3fff => self.rom.get(address as usize),
                0x4000..=0x7fff => {
                    let base_address = 0x4000 * (*rom_bank_number) as usize;
                    let offset = address as usize - 0x4000;
                    self.rom.get(base_address + offset)
                }
                0xa000..=0xbfff if !ram_enable => None,
                0xa000..=0xbfff => {
                    let base_address = 0x2000 * (*ram_bank_number) as usize;
                    let offset = address as usize - 0xa000;
                    self.ram.get(base_address + offset)
                }
                _ => panic!("Invalid read from cart (address is {:#06x})", address),
            },
        };
        data_opt.copied()
    }

    pub fn write(&mut self, address: u16, data: u8) -> Option<MemoryMapChange> {
        trace!("In cart, writing {:#04x} to {:#06x}", data, address);
        match address {
            0x0000..=0x7fff => match &mut self.mbc {
                MBC::MBC1 { ref mut ram_enable, ram_bank_number: _, ref mut rom_bank_number, ref mut banking_mode_select } => match address {
                    0x0000..=0x1fff => MemoryMapChange::invalidate_range(0xa000..=0xbfff, ram_enable, (data & 0xf) == 0xa),
                    0x2000..=0x3fff => MemoryMapChange::invalidate_range(
                        0x4000..=0x7fff,
                        rom_bank_number,
                        (*rom_bank_number & 0b1110_0000) | (data & 0x1f),
                    ),
                    0x4000..=0x5fff => todo!("MBC1 upper ROM bank number"),
                    0x6000..=0x7fff => MemoryMapChange::invalidate_range(0x0000..=0x7fff, banking_mode_select, data == 1),
                    _ => unreachable!(),
                },
            },
            0xa000..=0xbfff => {
                let ram_address = address as usize - 0xa000;
                MemoryMapChange::invalidate_one(address, &mut self.ram[ram_address], data)
            }
            _ => panic!("Incorrectly sent write with address {:#06x} to cart!", address),
        }
    }
}
