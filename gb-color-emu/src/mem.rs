use bitvec::prelude::*;
use log::*;
use std::fmt;
use thiserror::Error;

#[derive(Copy, Clone, Debug, Error)]
#[error("Attempted to read uninitialized memory at {address:#06x}")]
pub struct UninitializedReadError {
    pub address: u16,
}

#[derive(Copy, Clone, Debug, Error)]
pub enum MemoryError {
    #[error("Tried to interact with memory address {address:#06x} that is out of range")]
    InvalidAddress { address: u16 },
    #[error("{0}")]
    UninitializedRead(#[from] UninitializedReadError),
}

pinout! {
    pub struct LH5P832Pinout {
        in address: u16,
        inout io: u8,
        in ce: bool,
        in oe_rfsh: bool,
        in rw: bool,
    }
}

impl LH5P832Pinout {
    pub fn new() -> Self {
        let mut pins = Self::default();
        pins.set_ce__external(true);
        pins.set_oe_rfsh__external(true);
        pins
    }
}

    // pub fn update_from_cpu_pins(&mut self, cpu_pins: &SM83Pinout) {
    //     trace!("Updating LH5P832 pins from CPU pins {:?}", cpu_pins);
    //     self.set_address(cpu_pins.get_maddress());
    //     self.set_io(cpu_pins.get_mdata());
    //     self.set_ce(cpu_pins.get_cs1());
    //     self.set_oe_rfsh(cpu_pins.get_mrd());
    //     self.set_rw(cpu_pins.get_mwr());
    // }

// #[derive(Copy, Clone, Debug)]
// enum State {
//     Ready,
//     Reading { addr: u16 },
//     Writing { addr: u16 },
// }

pub struct LH5P832 {
    data: Vec<u8>,
    initialized: BitVec,
    write_ready: bool,
}

impl fmt::Debug for LH5P832 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("LH5P832")
            // .field("state", &self.state)
            .field("data", &format_args!("<elided>"))
            .finish()
    }
}

impl LH5P832 {
    pub fn new() -> Self {
        Self {
            data: vec![0; 0x4000],
            // state: State::Ready,
            initialized: bitvec![0; 0x4000],
            write_ready: false,
        }
    }

    #[inline]
    pub fn load(&mut self, buffer: &[u8]) {
        self.load_at(buffer, 0)
    }

    pub fn load_at(&mut self, buffer: &[u8], address: u16) {
        assert!(buffer.len() < u16::MAX as usize);
        for (i, byte) in buffer.iter().copied().enumerate() {
            self.write_byte(byte, address + i as u16);
        }
    }

    #[inline]
    fn write_byte(&mut self, byte: u8, address: u16) {
        let address = address as usize;
        self.data[address] = byte;
        *self.initialized.get_mut(address).unwrap() = true;
    }

    #[inline]
    fn read_byte(&self, address: u16) -> Result<u8, MemoryError> {
        match self.initialized.get(address as usize) {
            Some(bit) if *bit => Ok(self.data[address as usize]),
            Some(_) => Err(UninitializedReadError { address }.into()),
            None => Err(MemoryError::InvalidAddress { address }),
        }
    }

    #[inline]
    fn validate_addr(address: u16) -> Result<(), MemoryError> {
        if address < (1 << 15) {
            Ok(())
        } else {
            Err(MemoryError::InvalidAddress { address })
        }
    }

    pub fn step_cycle(&mut self, pins: &mut LH5P832Pinout) {
        trace!("Stepping LH5P832 with pins {:?}", pins);
        pins.release_io();
        if let None | Some(true) = pins.get_ce__opt() {
            trace!("CE pin high in LH5P832::step_cycle, doing nothing");
            self.write_ready = false;
            return;
        }
        if !pins.get_rw() {
            if self.write_ready {
                let addr = pins.get_address();
                Self::validate_addr(addr).unwrap();
                let value = pins.get_io();
                match self.read_byte(addr) {
                    Ok(prev) => debug!("In LH5P832, writing {:#04x} over {:#04x} at address {:#06x}", value, prev, addr),
                    Err(_) => debug!("In LH5P832, writing {:#04x} over uninitialized data at address {:#06x}", value, addr),
                }
                self.write_byte(value, addr);
            } else {
                self.write_ready = true;
            }
        } else if !pins.get_oe_rfsh() {
            self.write_ready = false;
            let addr = pins.get_address();
            Self::validate_addr(addr).unwrap();
            let value = match self.read_byte(addr) {
                Ok(x) => x,
                Err(MemoryError::UninitializedRead(_)) => 0x00,
                Err(e) => panic!("Failed to read byte from memory! {}", e),
            };
            debug!("In LH5P832, reading {:#04x} at address {:#06x}", value, addr);
            pins.set_io(value);
        } else {
            self.write_ready = false;
        }
        trace!("Finished stepping LH5P832, pins now {:?}", pins);
    }
}
