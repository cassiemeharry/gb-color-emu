use log::*;

use crate::screen::Color;

pinout! {
    pub struct LcdControllerPinout {
        // These three are really only five bits wide.
        in r: u8,
        in g: u8,
        in b: u8,
        // The CPU should set this to true when it's driving a valid value on r,
        // g, and b, and false otherwise.
        in push: bool,
        // This will go high for one cycle when the LCD controller has accepted
        // the pixel being pushed on the r, g, and b lines. This is a signal for
        // the PPU to start sending the next pixel.
        out accepted: bool,
        // Interrupts - these stay high during the respective period, and go low
        // when that period ends.
        out hblank: bool,
        out vblank: bool,
    }
}

#[derive(Clone, Debug, Default)]
pub struct LcdController {
    current_y: u8,
    current_x: u8,
    frame_dot_counter: u32,
    pub screen: crate::screen::Screen,
}

impl LcdController {
    pub fn step_cycle(&mut self, pins: &mut LcdControllerPinout) {
        pins.set_accepted(false);
        if self.current_y > 143 {
            pins.set_vblank(true);
            pins.set_hblank(false);
        } else if self.current_x >= 160 {
            pins.set_vblank(false);
            pins.set_hblank(true);
        } else {
            pins.set_vblank(false);
            pins.set_hblank(false);
        }

        let dots_this_scanline = self.frame_dot_counter % 456;
        if self.current_x >= 160 && dots_this_scanline == 455 {
            self.current_x = 0;
            self.current_y += 1;
        } else if self.current_x >= 160 || self.current_y >= 144 {
            // do nothing
        } else if let Some(true) = pins.get_push__opt() {
            info!("LCD Controller accepting pixel from PPU");
            let r = pins.get_r();
            let g = pins.get_g();
            let b = pins.get_b();
            let color = Color::new(r, g, b);
            self.screen.set_xy(self.current_x, self.current_y, color);
            self.current_x += 1;
            pins.set_accepted(true);
        }

        self.frame_dot_counter += 1;
        if self.frame_dot_counter >= 70224 {
            self.frame_dot_counter = 70224;
        }
    }
}
