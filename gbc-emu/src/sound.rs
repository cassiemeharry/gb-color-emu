use crate::{
    cart::Cart,
    cpu::{CPU, InterruptFlags, SpeedSwitchFlags, WriteRecord},
    lcd::LcdController,
    memory::{Memory, MemoryMapChange as MMC},
    screen::Screen,
};

bitflags::bitflags! {
    #[derive(Default)]
    pub(crate) struct SoundOnFlags: u8 {
        const MASTER = 1 << 7;
        const CHANNEL_4 = 1 << 3;
        const CHANNEL_3 = 1 << 2;
        const CHANNEL_2 = 1 << 1;
        const CHANNEL_1 = 1 << 0;
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SoundRegisters {
    pub(crate) nr10: u8,
    pub(crate) nr11: u8,
    pub(crate) nr12: u8,
    pub(crate) nr13: u8,
    pub(crate) nr14: u8,
    pub(crate) nr21: u8,
    pub(crate) nr22: u8,
    pub(crate) nr23: u8,
    pub(crate) nr24: u8,
    pub(crate) nr30: u8,
    pub(crate) nr31: u8,
    pub(crate) nr32: u8,
    pub(crate) nr33: u8,
    pub(crate) nr34: u8,
    pub(crate) nr41: u8,
    pub(crate) nr42: u8,
    pub(crate) nr43: u8,
    pub(crate) nr44: u8,
    pub(crate) nr50: u8,
    pub(crate) nr51: u8,
    pub(crate) nr52: SoundOnFlags,
    pub(crate) wave_pattern_ram: [u8; 16],
}

impl Default for SoundRegisters {
    fn default() -> Self {
        Self {
            nr10: 0x80,
            nr11: 0xbf,
            nr12: 0xf3,
            nr13: 0xff,
            nr14: 0xbf,
            nr21: 0x3f,
            nr22: 0x00,
            nr23: 0xff,
            nr24: 0xbf,
            nr30: 0x7f,
            nr31: 0xff,
            nr32: 0x9f,
            nr33: 0xff,
            nr34: 0xbf,
            nr41: 0xff,
            nr42: 0x00,
            nr43: 0x00,
            nr44: 0xbf,
            nr50: 0x77,
            nr51: 0xf3,
            nr52: SoundOnFlags::from_bits_truncate(0xf1),
            wave_pattern_ram: [0x00; 16],
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct APU {
    pub(crate) registers: SoundRegisters,
    cycles: u64,
}

impl APU {
    pub(crate) fn step(&mut self, cycles: u64, cart: &mut Cart, cpu: &mut CPU, lcd: &mut LcdController, memory: &mut Memory, screen: &mut Screen) {
        if !self.registers.nr52.contains(SoundOnFlags::MASTER) {
            return;
        }

        // todo!()
    }
}
