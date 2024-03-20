use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rgb888 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Rgb888 {
    pub const BLACK: Self = Self { r: 0, g: 0, b: 0 };

    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

impl From<Rgb555> for Rgb888 {
    #[inline]
    fn from(small: Rgb555) -> Rgb888 {
        let (r, g, b) = small.into_555_tuple();
        // Repeat the top bits into the lower bits to get an even spread of color.
        Self {
            r: (r << 3) | (r >> 5),
            g: (g << 3) | (g >> 5),
            b: (b << 3) | (b >> 5),
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Rgb555 {
    word: u16,
}

impl fmt::Debug for Rgb555 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (r, g, b) = self.into_555_tuple();
        f.debug_struct("Rgb555")
            .field("r", &r)
            .field("g", &g)
            .field("b", &b)
            .finish()
    }
}

impl From<Rgb888> for Rgb555 {
    #[inline]
    fn from(big: Rgb888) -> Rgb555 {
        Rgb555::new(big.r >> 3, big.g >> 3, big.b >> 3)
    }
}

impl From<u16> for Rgb555 {
    #[inline]
    fn from(word: u16) -> Rgb555 {
        let word = word & 0x7fff;
        Rgb555 { word }
    }
}

impl From<(u8, u8)> for Rgb555 {
    #[inline]
    fn from((lsb, msb): (u8, u8)) -> Rgb555 {
        let msb = msb & 0x7f;
        let word = ((msb as u16) << 8) | (lsb as u16);
        Rgb555 { word }
    }
}

impl Rgb555 {
    const BLACK: Self = Self { word: 0x0000 };

    #[inline]
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        debug_assert!(r <= 0x1f);
        debug_assert!(g <= 0x1f);
        debug_assert!(b <= 0x1f);
        Self {
            word: ((b as u16) << 10) | ((g as u16) << 5) | ((r as u16) << 0),
        }
    }

    #[inline]
    pub fn into_555_tuple(self) -> (u8, u8, u8) {
        let r = (self.word >> 0) as u8 & 0x1f;
        let g = (self.word >> 5) as u8 & 0x1f;
        let b = (self.word >> 10) as u8 & 0x1f;
        (r, g, b)
    }
}

#[derive(Clone, Debug)]
pub struct Screen {
    pixels: Vec<Rgb555>,
}

impl Screen {
    pub const WIDTH: usize = 160;
    pub const HEIGHT: usize = 144;

    pub fn new() -> Self {
        let pixels = vec![Rgb555::BLACK; Self::WIDTH * Self::HEIGHT];
        Self { pixels }
    }

    pub fn pixels(&self) -> &[Rgb555] {
        &self.pixels
    }

    #[inline]
    pub(crate) fn put_pixel(&mut self, x: u8, y: u8, color: Rgb555) {
        let index = (y as usize) * Self::WIDTH + (x as usize);
        self.pixels[index] = color;
    }
}
