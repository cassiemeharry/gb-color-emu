use std::{fmt, ops::RangeInclusive};

#[derive(Clone)]
pub enum MemoryMapChange {
    InvalidateAll,
    InvalidateRange(RangeInclusive<u16>),
    InvalidateOne(u16),
}

impl fmt::Debug for MemoryMapChange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidateAll => f.debug_tuple("InvalidateAll").finish(),
            Self::InvalidateRange(range) => {
                let (lb, ub) = (range.start(), range.end());
                f.debug_tuple("InvalidateRange")
                    .field(&format_args!("{:#06x}..={:#06x}", lb, ub))
                    .finish()
            }
            Self::InvalidateOne(addr) => f.debug_tuple("InvalidateOne")
                .field(&format_args!("{:#06x}", addr))
                .finish(),
        }
    }
}

impl MemoryMapChange {
    #[inline]
    pub fn invalidate_all<T: Copy + Eq + std::fmt::Debug>(slot: &mut T, new_val: T) -> Option<Self> {
        let prev_val = *slot;
        if prev_val != new_val {
            trace!("Invalidating ALL memory because data changed from {:?} to {:?}", prev_val, new_val);
            *slot = new_val;
            Some(Self::InvalidateAll)
        } else {
            trace!("No memory change in invalidate_all");
            None
        }
    }

    #[inline]
    pub fn invalidate_range<T: Copy + Eq + std::fmt::Debug>(range: RangeInclusive<u16>, slot: &mut T, new_val: T) -> Option<Self> {
        let prev_val = *slot;
        if prev_val != new_val {
            trace!("Invalidating memory range {:04x?} because data changed from {:?} to {:?}", range, prev_val, new_val);
            *slot = new_val;
            Some(Self::InvalidateRange(range))
        } else {
            trace!("No memory change in invalidate_range");
            None
        }
    }

    #[inline]
    pub fn invalidate_one<T: Copy + Eq + std::fmt::Debug>(address: u16, slot: &mut T, new_val: T) -> Option<Self> {
        let prev_val = *slot;
        if prev_val != new_val {
            trace!("Invalidating memory address {:#06x?} because data changed from {:?} to {:?}", address, prev_val, new_val);
            *slot = new_val;
            Some(Self::InvalidateOne(address))
        } else {
            trace!("No memory change in invalidate_one");
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct Memory {
    pub(crate) video_ram_bank_0: Vec<u8>,
    pub(crate) video_ram_bank_1: Vec<u8>,
    pub video_ram_bank: usize,
    work_ram: Vec<u8>,
    pub work_ram_bank: usize,
    high_ram: Vec<u8>,
    pub(crate) boot_rom: Option<&'static [u8]>,
}

impl Memory {
    pub(crate) const BOOT_ROM_HIGH_ADDR: u16 = include_bytes!("../cgb_boot.bin").len() as u16 - 1;

    pub fn new() -> Self {
        Self {
            video_ram_bank_0: vec![0x00; 0x2000],
            video_ram_bank_1: vec![0x00; 0x2000],
            video_ram_bank: 0,
            work_ram: vec![0x00; 0x8000],
            work_ram_bank: 1,
            high_ram: vec![0x00; 0xff - 0x80],
            boot_rom: Some(include_bytes!("../cgb_boot.bin")),
        }
    }

    pub fn read(&mut self, address: u16) -> Option<u8> {
        match address {
            0x8000..=0x9fff if self.video_ram_bank == 0 => self.video_ram_bank_0.get(address as usize - 0x8000).copied(),
            0x8000..=0x9fff if self.video_ram_bank == 1 => self.video_ram_bank_1.get(address as usize - 0x8000).copied(),
            0x8000..=0x9fff => {
                error!("unhandled Memory::video_ram_bank value {:?}", self.video_ram_bank);
                None
            }
            0xc000..=0xcfff => {
                let ram_address = address as usize - 0xc000;
                trace!("Reading from Work RAM at offset {:#06x}", ram_address);
                self.work_ram.get(ram_address).copied()
            }
            0xd000..=0xdfff if self.work_ram_bank <= 7 => {
                let base_address = address as usize - 0xd000;
                let ram_address = base_address + (0x1000 * self.work_ram_bank.min(1));
                self.work_ram.get(ram_address).copied()
            }
            0xff80..=0xfffe => {
                let offset = address as usize - 0xff80;
                trace!("Reading from High RAM at offset {:#04x}", offset);
                self.high_ram.get(offset).copied()
            }
            _ => {
                error!("Tried to read bad address from system memory: {:#06x}", address);
                None
            }
        }
    }

    pub fn write(&mut self, address: u16, data: u8) -> Option<MemoryMapChange> {
        match address {
            0x8000..=0x9fff if self.video_ram_bank == 0 => {
                let ram_address = address as usize - 0x8000;
                trace!("Writing to Video RAM (bank 1) at offset {:#04x}", ram_address);
                MemoryMapChange::invalidate_one(address, &mut self.video_ram_bank_0[ram_address], data)
            }
            0x8000..=0x9fff if self.video_ram_bank == 1 => {
                let ram_address = address as usize - 0x8000;
                trace!("Writing to Video RAM (bank 1) at offset {:#04x}", ram_address);
                MemoryMapChange::invalidate_one(address, &mut self.video_ram_bank_1[ram_address], data)
            }
            0x8000..=0x9fff => {
                error!("unhandled Memory::video_ram_bank value {:?}", self.video_ram_bank);
                None
            }
            0xc000..=0xcfff => {
                let ram_address = address as usize - 0xc000;
                trace!("Writing to Work RAM at offset {:#06x}", ram_address);
                MemoryMapChange::invalidate_one(address, &mut self.work_ram[ram_address], data)
            }
            0xd000..=0xdfff => {
                let base_address = address as usize - 0xd000;
                let ram_address = base_address + (0x1000 * self.work_ram_bank.min(1));
                trace!("Writing to Work RAM at offset {:#06x}", ram_address);
                MemoryMapChange::invalidate_one(address, &mut self.work_ram[ram_address], data)
            }
            0xff80..=0xfffe => {
                let ram_address = address as usize - 0xff80;
                trace!("Writing to High RAM at offset {:#04x}", ram_address);
                MemoryMapChange::invalidate_one(address, &mut self.high_ram[ram_address], data)
            }
            _ => panic!("Incorrectly write with address {:#06x} to memory!", address),
        }
    }
}
