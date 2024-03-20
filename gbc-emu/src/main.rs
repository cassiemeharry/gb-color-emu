#[macro_use]
extern crate log;

use std::convert::TryInto;

use gbc_emu::{Cart, System, screen::{Screen, Rgb888}};

#[cfg(debug_assertions)]
const CYCLES: u64 = 1_000_000;
#[cfg(not(debug_assertions))]
const CYCLES: u64 = 10_000_000;

const FRAME_TIME: std::time::Duration = std::time::Duration::from_micros(1_000_000 / 60);

fn main() {
    pretty_env_logger::init();

    let f = std::fs::File::open("roms/SpaceInvasion01.gb").unwrap();
    let cart = Cart::load_rom(f).unwrap();
    let mut sys = System::new(cart);
    let mut cycles = 0;
    let mut sim_time = std::time::Duration::from_secs(0);
    let mut wall_start = std::time::Instant::now();
    let mut frames = 0;
    let mut last_pc = 0xffff;
    let mut pc_loop = false;
    while cycles < CYCLES {
        let frame_start = std::time::Instant::now();
        let cycles_prev = cycles;
        cycles = sys.run_to_vblank();
        sim_time += frame_start.elapsed();
        trace!("At end of step, cycles went from {} to {}", cycles_prev, cycles);

        if frames % 32 == 0 {
            render::<4, 16>(sys.get_screen());
            println!("frame {}, after {} cycles", frames, cycles);
        }
        frames += 1;
        let frame_end = std::time::Instant::now();
        if frame_start + FRAME_TIME > frame_end {
            std::thread::sleep(FRAME_TIME - (frame_end - frame_start));
        }
    }
    let wall_time = wall_start.elapsed();
    info!(
        "Did {} cycles ({} frames) in {:?} ({:?} per 1000 cycles, target is 238μs/1000 cycles)",
        cycles,
        frames,
        sim_time,
        sim_time / (cycles / 1000).try_into().unwrap(),
    );
    info!("Spent {:?} in wall clock time ({:?} per frame)", wall_time, wall_time / frames);
    info!("Last PC was {:#06x}", sys.get_cpu().get_registers().pc);
}

fn render<const DIV: usize, const DIV_SQUARED: usize>(screen: &Screen) {
    use std::fmt::Write;

    assert!(DIV >= 1);
    assert_eq!(DIV * DIV, DIV_SQUARED);

    const UPPER_HALF_BLOCK: char = '\u{2580}';

    let mut buffer = String::new();

    let pixels = screen.pixels();
    let mut last_color_upper = Rgb888::BLACK;
    let mut last_color_lower = Rgb888::BLACK;
    buffer.push_str("--------------------------------------------------------------------------------\n");
    // buffer.push_str("\x1b[2J");
    for y in (0..144).step_by(DIV * 2) {
        for x in (0..160).step_by(DIV) {
            let mut pixels_upper: [Rgb888; DIV_SQUARED] = [Rgb888::BLACK; DIV_SQUARED];
            let mut pixels_lower: [Rgb888; DIV_SQUARED] = [Rgb888::BLACK; DIV_SQUARED];
            for i in 0..DIV {
                for j in 0..DIV {
                    let index = i * DIV + j;
                    pixels_upper[index] = pixels[(y + i +  0 ) * 160 + (x + j)].into();
                    pixels_lower[index] = pixels[(y + i + DIV) * 160 + (x + j)].into();
                }
            }
            // for DIV = 1:
            // upper[0] = (y + 0 + 0) * 160 + (x + 0)
            // lower[0] = (y + 0 + 1) * 160 + (x + 0)
            //
            // for DIV = 2:
            // upper[0] = (y + 0 + 0) * 160 + (x + 0)
            // upper[1] = (y + 0 + 0) * 160 + (x + 1)
            // upper[2] = (y + 1 + 0) * 160 + (x + 0)
            // upper[3] = (y + 1 + 0) * 160 + (x + 1)
            // lower[0] = (y + 0 + 2) * 160 + (x + 0)
            // lower[1] = (y + 0 + 2) * 160 + (x + 1)
            // lower[2] = (y + 1 + 2) * 160 + (x + 0)
            // lower[3] = (y + 1 + 2) * 160 + (x + 1)

            let upper_pixel = average_pixels(&pixels_upper);
            let lower_pixel = average_pixels(&pixels_lower);
            if upper_pixel != last_color_upper {
                write!(&mut buffer, "\x1b[38;2;{};{};{}m", upper_pixel.r, upper_pixel.g, upper_pixel.b).unwrap();
                last_color_upper = upper_pixel;
            }
            if lower_pixel != last_color_lower {
                write!(&mut buffer, "\x1b[48;2;{};{};{}m", lower_pixel.r, lower_pixel.g, lower_pixel.b).unwrap();
                last_color_lower = lower_pixel;
            }
            if lower_pixel == upper_pixel {
                buffer.push(' ');
            } else {
                buffer.push(UPPER_HALF_BLOCK);
            }
        }
        buffer.push_str("\x1b[0m\n");
        last_color_upper = Rgb888::BLACK;
        last_color_lower = Rgb888::BLACK;
    }
    println!("{}", buffer);
}

fn average_pixels<const N: usize>(pixels: &[Rgb888; N]) -> Rgb888 {
    let len = N as u32;
    let mut r = 0;
    let mut g = 0;
    let mut b = 0;
    for p in pixels {
        r += p.r as u32;
        g += p.g as u32;
        b += p.b as u32;
    }
    Rgb888 {
        r: (r / len) as u8,
        g: (g / len) as u8,
        b: (b / len) as u8,
    }
}
