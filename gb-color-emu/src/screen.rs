use cgmath::{Matrix4, Vector3, Vector4};
use rand::prelude::*;
use std::fmt;

const SUBPIXEL_MASK: u8 = 0b0001_1111;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;
const PIXELS: usize = WIDTH * HEIGHT;

macro_rules! index {
    (y: $y:tt , x : $x:tt) => {
        index!(x: $x, y: $y)
    };
    (x : $x:tt , y: $y:tt) => {
        ($y as usize) * WIDTH + ($x as usize)
    };
}

/// RGB555 colors stored as RGB888
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl From<Vector3<f32>> for Color {
    fn from(m: Vector3<f32>) -> Color {
        Color {
            r: (m.x * 255.0).round().clamp(0.0, 255.0) as u8,
            g: (m.y * 255.0).round().clamp(0.0, 255.0) as u8,
            b: (m.z * 255.0).round().clamp(0.0, 255.0) as u8,
        }
    }
}

impl Color {
    pub const BLACK: Self = Color { r: 0, g: 0, b: 0 };
    pub const WHITE: Self = Color { r: 0xff, g: 0xff, b: 0xff };
    pub const GRAY: Self = Color { r: 0x7f, g: 0x7f, b: 0x7f };

    #[inline]
    fn convert_5bit_to_8bit(raw: [u8; 3]) -> Color {
        // This is translated from libretro's `gbc-color.slang` shader.
        const INPUT_MAX: f32 = 0x1f as f32;
        let screen: Vector4<f32> = [
            raw[0].min(0x1f) as f32 / INPUT_MAX,
            raw[1].min(0x1f) as f32 / INPUT_MAX,
            raw[2].min(0x1f) as f32 / INPUT_MAX,
            1.0,
        ].into();
        const LUM: f32 = 0.94;
        const SAT: f32 = 1.0;
        const GAMMA: f32 = 2.2;
        let color: Matrix4<f32> = [
            // r, g, b, black
            [0.82, 0.125, 0.195, 0.0], // red channel
            [0.24, 0.665, 0.075, 0.0], // green channel
            [-0.06, 0.21, 0.73, 0.0], // blue channel
            [0.0, 0.0, 0.0, 0.0],
        ].into();
        let adjust: Matrix4<f32> = [
            [(1.0 - SAT) * 0.3086 + SAT, (1.0 - SAT) * 0.3086, (1.0 - SAT) * 0.3086, 1.0],
            [(1.0 - SAT) * 0.6094, (1.0 - SAT) * 0.6094 + SAT, (1.0 - SAT) * 0.6094, 1.0],
            [(1.0 - SAT) * 0.0820, (1.0 - SAT) * 0.0820, (1.0 - SAT) * 0.0820 + SAT, 1.0],
            [0.0, 0.0, 0.0, 1.0],
        ].into();
        let color = color * adjust;
        let screen: Vector4<f32> = color * (screen * LUM).map(|x| x.clamp(0.0, 1.0));
        screen.map(|x| x.powf(1.0 / GAMMA)).truncate().into()
    }

    #[inline]
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        assert!(r <= SUBPIXEL_MASK);
        assert!(g <= SUBPIXEL_MASK);
        assert!(b <= SUBPIXEL_MASK);
        Self::convert_5bit_to_8bit([r, g, b])
    }

    fn display_terminal_24bit(&self, foreground: bool) -> impl fmt::Display {
        TerminalColor { color: *self, foreground }
    }
}

struct TerminalColor {
    color: Color,
    foreground: bool,
}

impl fmt::Display for TerminalColor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = self.color;
        if self.foreground {
            write!(f, "\u{1b}[38;2;{};{};{}m", c.r, c.g, c.b)
        } else {
            write!(f, "\u{1b}[48;2;{};{};{}m", c.r, c.g, c.b)
        }
    }
}

#[derive(Clone)]
pub struct Screen {
    pixels: Vec<Color>,
}

impl fmt::Debug for Screen {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       f.debug_struct("Screen").finish_non_exhaustive()
   }
}

impl Default for Screen {
    fn default() -> Self {
        Self {
            pixels: vec![Color::GRAY; PIXELS],
        }
    }
}
impl Screen {
    pub fn gradient() -> Self {
        let mut pixels = vec![Color::BLACK; PIXELS];
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                pixels[index!(x: x, y: y)] = Color {
                    r: ((y as f32 / HEIGHT as f32) * 256.0).round() as u32 as u8,
                    g: ((x as f32 / WIDTH as f32) * 256.0).round() as u32 as u8,
                    b: 0,
                };
            }
        }
        Self { pixels }
    }

    pub fn random<R: Rng>(mut rng: R) -> Self {
        let mut pixels = vec![Color::BLACK; PIXELS];
        for i in 0..PIXELS {
            let r = rng.gen::<u8>() & SUBPIXEL_MASK;
            let g = rng.gen::<u8>() & SUBPIXEL_MASK;
            let b = rng.gen::<u8>() & SUBPIXEL_MASK;
            // assert!(!(r == g && g == b), "Got the same color for pixel {}! {:?}, {:?}, {:?}", i, r, g, b);
            pixels[i] = Color::new(r, g, b);
        }
        Self { pixels }
    }

    pub fn display_terminal(&self) -> impl fmt::Display + '_ {
        TerminalScreen(self)
        // const RESET: &str = "\x1b[0m";
        // format!(
        //     "red: {}{}, green: {}{}, blue: {}{}, yellow: {}{}, cyan: {}{}, magenta: {}{}",
        //     TerminalGlyph { fg: Color::new(0x1f, 0, 0), bg: Color::BLACK, glyph: 'R' }, RESET,
        //     TerminalGlyph { fg: Color::new(0, 0x1f, 0), bg: Color::BLACK, glyph: 'G' }, RESET,
        //     TerminalGlyph { fg: Color::new(0, 0, 0x1f), bg: Color::BLACK, glyph: 'B' }, RESET,
        //     TerminalGlyph { fg: Color::new(0x1f, 0x1f, 0), bg: Color::BLACK, glyph: 'Y' }, RESET,
        //     TerminalGlyph { fg: Color::new(0, 0x1f, 0x1f), bg: Color::BLACK, glyph: 'C' }, RESET,
        //     TerminalGlyph { fg: Color::new(0x1f, 0, 0x1f), bg: Color::BLACK, glyph: 'M' }, RESET,
        // )
    }

    pub fn set_xy(&mut self, x: u8, y: u8, color: Color) {
        self.pixels[index!(x: x, y: y)] = color;
    }
}

struct TerminalScreen<'a>(&'a Screen);

impl fmt::Display for TerminalScreen<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let avg = |i: usize| -> Color {
            let Color { r: r1, g: g1, b: b1} = self.0.pixels[i];
            let Color { r: r2, g: g2, b: b2} = self.0.pixels[i + WIDTH];
            Color {
                r: (((r1 as u16) + (r2 as u16)) / 2) as u8,
                g: (((g1 as u16) + (g2 as u16)) / 2) as u8,
                b: (((b1 as u16) + (b2 as u16)) / 2) as u8,
            }
        };

        for y in (0..HEIGHT).step_by(4) {
            for x in (0..WIDTH).step_by(2) {
                let i_base = (y * WIDTH) + x;
                let g = TerminalGlyph::new(
                    avg(i_base), avg(i_base + 1),
                    avg(i_base + (WIDTH * 2)), avg(i_base + (WIDTH * 2) + 1),
                );
                write!(f, "{}", g)?;
            }
            write!(f, "\x1b[0m\n")?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
enum Glyph {
    Background,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
    LeftBar,
    TopLeftBottomRight,
    LowerBlock,
}

impl Glyph {
    const ALL: [Glyph; 8] = [
        Self::Background,
        Self::TopLeft,
        Self::TopRight,
        Self::BottomLeft,
        Self::BottomRight,
        Self::LeftBar,
        Self::TopLeftBottomRight,
        Self::LowerBlock,
    ];

    #[inline]
    const fn to_char(self) -> char {
        match self {
            Self::Background => ' ',
            Self::TopLeft => '\u{2598}',
            Self::TopRight => '\u{259d}',
            Self::BottomLeft => '\u{2596}',
            Self::BottomRight => '\u{2597}',
            Self::LeftBar => '\u{258c}',
            Self::TopLeftBottomRight => '\u{259a}',
            Self::LowerBlock => '\u{2584}',
        }
    }
}

struct TerminalGlyph {
    fg: Color,
    bg: Color,
    glyph: char,
}

impl TerminalGlyph {
    fn new(tl: Color, tr: Color, bl: Color, br: Color) -> Self {
        fn avg_distance<const N: usize>(pixels: [Color; N]) -> (Color, f32) {
            let mut totals = [0.0; 3];
            for c in pixels {
                totals[0] += c.r as f32;
                totals[1] += c.g as f32;
                totals[2] += c.b as f32;
            }
            totals[0] /= N as f32;
            totals[1] /= N as f32;
            totals[2] /= N as f32;

            let mut total_distance = 0.0;
            for c in pixels {
                let dr = (totals[0] - c.r as f32).powi(2);
                let dg = (totals[1] - c.g as f32).powi(2);
                let db = (totals[2] - c.b as f32).powi(2);
                total_distance += (dr + dg + db).sqrt();
            }
            (
                Color {
                    r: totals[0].round() as u32 as u8,
                    g: totals[1].round() as u32 as u8,
                    b: totals[2].round() as u32 as u8,
                },
                total_distance,
            )
        }

        let mut best = TerminalGlyph {
            fg: Color::WHITE,
            bg: Color::BLACK,
            glyph: 'X',
        };
        let mut best_distance = 1e12;
        for glyph_choice in Glyph::ALL {
            let ((bg, d), fg) = match glyph_choice {
                Glyph::Background => {
                    let (bg, d) = avg_distance([tl, tr, bl, br]);
                    ((bg, d), bg)
                }
                Glyph::TopLeft => (avg_distance([tr, bl, br]), tl),
                Glyph::TopRight => (avg_distance([tr, bl, br]), tr),
                Glyph::BottomLeft => (avg_distance([tl, tr, br]), bl),
                Glyph::BottomRight => (avg_distance([tl, tr, bl]), br),
                Glyph::LeftBar => {
                    let (bg, d1) = avg_distance([tr, br]);
                    let (fg, d2) = avg_distance([tl, bl]);
                    ((bg, d1 + d2), fg)
                }
                Glyph::TopLeftBottomRight => {
                    let (bg, d1) = avg_distance([tr, bl]);
                    let (fg, d2) = avg_distance([tl, br]);
                    ((bg, d1 + d2), fg)
                }
                Glyph::LowerBlock => {
                    let (bg, d1) = avg_distance([bl, br]);
                    let (fg, d2) = avg_distance([tl, tr]);
                    ((bg, d1 + d2), fg)
                }
            };
            if d < best_distance {
                best = TerminalGlyph { fg, bg, glyph: glyph_choice.to_char() };
                if d < 1.0 {
                    break;
                }
                best_distance = d;
            }
        }
        best
    }
}

impl fmt::Display for TerminalGlyph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f, "{}{}{}",
            self.bg.display_terminal_24bit(false),
            self.fg.display_terminal_24bit(true),
            self.glyph,
        )
    }
}
