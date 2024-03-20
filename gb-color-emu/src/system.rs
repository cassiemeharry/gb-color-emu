use log::*;

use crate::{
    cart::{Cart, CartPinout},
    cpu::{SM83, SM83Pinout},
    lcd::{LcdController, LcdControllerPinout},
    mem::{LH5P832, LH5P832Pinout},
};

#[derive(Debug)]
pub struct GameBoyColor<'rom> {
    pub cpu: SM83,
    pub cpu_pins: SM83Pinout,
    pub work_ram: LH5P832,
    pub work_ram_pins: LH5P832Pinout,
    pub cart: Cart<'rom>,
    pub cart_pins: CartPinout,
    pub lcd_controller: LcdController,
    pub lcd_controller_pins: LcdControllerPinout,
    pub in_vblank: bool,
}

impl<'rom> GameBoyColor<'rom> {
    pub fn new(cart: Cart<'rom>) -> Self {
        let mut cpu = SM83::new();
        cpu.regs.pc = cart.entry_point;
        Self {
            cpu,
            cpu_pins: SM83Pinout::new(),
            work_ram: LH5P832::new(),
            work_ram_pins: LH5P832Pinout::new(),
            cart,
            cart_pins: CartPinout::default(),
            lcd_controller: LcdController::default(),
            lcd_controller_pins: LcdControllerPinout::default(),
            in_vblank: true,
        }
    }

    pub fn step_cycle(&mut self) {
        trace!("Starting a system cycle");
        self.cpu.step_cycle(&mut self.cpu_pins);
        self.cart.step_cycle(&mut self.cart_pins);
        self.work_ram.step_cycle(&mut self.work_ram_pins);
        self.lcd_controller.step_cycle(&mut self.lcd_controller_pins);

        self.reconcile_pins();

        if !self.in_vblank && self.lcd_controller_pins.get_vblank__external() {
            self.in_vblank = true;
            println!("Screen:\n{}", self.lcd_controller.screen.display_terminal());
        } else if self.in_vblank && !self.lcd_controller_pins.get_vblank__external() {
            self.in_vblank = false;
        }

        trace!("Finished with a system cycle");
    }

    fn reconcile_pins(&mut self) {
        trace!("Reconciling pins...");
        self.reconcile_pins_cpu_cart();
        self.reconcile_pins_cpu_work_ram();
        self.reconcile_pins_cpu_lcd();
        trace!("Finished reconciling pins.");
    }

    fn reconcile_pins_cpu_cart(&mut self) {
        trace!("Reconciling CPU pins with Cart...");
        connect_pins! {
            self.cart_pins; CartPinout;
            self.cpu_pins; SM83Pinout;
            [
                address, data, wr, rd, cs, [res => reset], vin,
            ]
        };
        trace!("Reconciled CPU pins with Cart");
        trace!("CPU pins: {:?}", self.cpu_pins);
        trace!("Cart pins: {:?}", self.cart_pins);
    }

    fn reconcile_pins_cpu_work_ram(&mut self) {
        trace!("Reconciling CPU pins with Work RAM...");
        connect_pins! {
            self.work_ram_pins; LH5P832Pinout;
            self.cpu_pins; SM83Pinout;
            [
                [address => maddress], [io => mdata], [rw => mwr], [oe_rfsh => mrd], [ce => cs1],
            ]
        };
        trace!("Reconciled CPU pins with Work RAM");
        trace!("CPU pins: {:?}", self.cpu_pins);
        trace!("Work RAM pins: {:?}", self.work_ram_pins);
    }

    fn reconcile_pins_cpu_lcd(&mut self) {
        trace!("Reconciling CPU pins with LCD Controller...");
        connect_pins! {
            self.lcd_controller_pins; LcdControllerPinout;
            self.cpu_pins; SM83Pinout;
            [
                [r => lcd_r],
                [g => lcd_g],
                [b => lcd_b],
                [push => lcd_push],
                [accepted => lcd_accepted],
                hblank,
                vblank,
            ]
        };
    }

    // fn reconcile_pins_cpu_lcd_controller(&mut self) {
    //     trace!("Reconciling CPU pins with LCD Controller...");
    //     todo!();
    //     trace!("Reconciled CPU pins with LCD Controller");
    //     trace!("CPU pins: {:?}", self.cpu_pins);
    //     trace!("LCD Controller pins: {:?}", self.lcd_controller_pins);
    // }
}
