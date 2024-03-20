#[macro_export]
macro_rules! gbc_asm_with {
    ($bytes:expr ; nop ; $($x:tt)*) => {
        $bytes.push(0x00);
        crate::gbc_asm_with!($bytes $($x)* ;);
    };
    ($bytes:expr ; ld bc, $expr:expr ; $($x:tt)*) => {
        let expr: u16 = $expr;
        let high = (expr >> 8) as u8;
        let low = expr as u8;
        $bytes.extend([0x01, high, low]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld de, $expr:expr ; $($x:tt)*) => {
        let expr: u16 = $expr;
        let high = (expr >> 8) as u8;
        let low = expr as u8;
        $bytes.extend([0x11, high, low]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld hl, $expr:expr ; $($x:tt)*) => {
        let expr: u16 = $expr;
        let high = (expr >> 8) as u8;
        let low = expr as u8;
        $bytes.extend([0x21, high, low]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld sp, $expr:expr ; $($x:tt)*) => {
        let expr: u16 = $expr;
        let high = (expr >> 8) as u8;
        let low = expr as u8;
        $bytes.extend([0x31, high, low]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld (bc), a ; $($x:tt)*) => {
        $bytes.push(0x02);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc bc ; $($x:tt)*) => {
        $bytes.push(0x03);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc b ; $($x:tt)*) => {
        $bytes.push(0x04);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc c ; $($x:tt)*) => {
        $bytes.push(0x0c);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc d ; $($x:tt)*) => {
        $bytes.push(0x14);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc e ; $($x:tt)*) => {
        $bytes.push(0x1c);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc h ; $($x:tt)*) => {
        $bytes.push(0x24);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc l ; $($x:tt)*) => {
        $bytes.push(0x2c);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc (hl) ; $($x:tt)*) => {
        $bytes.push(0x34);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; inc a ; $($x:tt)*) => {
        $bytes.push(0x3c);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; dec b ; $($x:tt)*) => {
        $bytes.push(0x05);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld b, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x06, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld c, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x0e, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld d, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x16, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld e, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x1e, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld h, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x26, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld l, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x2e, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld (hl), $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x36, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };
    ($bytes:expr ; ld a, $n:expr ; $($x:tt)*) => {
        let n: u8 = $n;
        $bytes.extend([0x3e, n]);
        crate::gbc_asm_with!($bytes ; $($x)*);
    };

    ($bytes:expr ;) => {};
    ($bytes:expr) => {};
}

#[macro_export]
macro_rules! gbc_asm {
    (; $($x:tt)+) => {{
        let mut bytes = vec![];
        gbc_asm_with!(bytes ; $($x)+ ;);
        bytes
    }};
}
// pub use gbc_asm;
// pub use gbc_asm_with;
