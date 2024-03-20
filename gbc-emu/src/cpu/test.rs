use pretty_assertions::assert_eq;
use rusty_fork::rusty_fork_test;
use test_env_log::test;

use super::{*, InterruptFlags as IF};

const CARRY: GBFlags = GBFlags::CARRY;
const HALF_CARRY: GBFlags = GBFlags::HALF_CARRY;
const SUB: GBFlags = GBFlags::SUB;
const ZERO: GBFlags = GBFlags::ZERO;

macro_rules! def_tests {
    ($($x:tt),* $(,)*) => {
        $(
            def_tests!(@single $x);
        )*
    };
    (@single { name: $name:ident, $($x:tt)* }) => {
        paste::paste! {
//            rusty_fork_test! {
                #[test]
                fn [< test_ $name>]() {
                    let mut system: crate::System;
                    def_tests!(@pre {system: system}, $($x)*);
                    // todo!("system uninit?");
                    let mut n = 0;
                    let mut writes: Vec<WriteRecord> = vec![];
                    // for _ in 0..10 {
                        let new_cycles = system.step();
                        writes.append(&mut system.get_cpu_mut().writes);
                        // if new_cycles == 0 {
                        //     break;
                        // }
                        n += new_cycles;
                    // }
                    let n = n;
                    let cpu = system.get_cpu();
                    def_tests!(@post {cpu: cpu, cycles: n, writes: writes}, $($x)*);
                }
//            }
        }
    };
    // (@single { name: $name:ident, cycles: $cycles:expr, rom: { $($instr:tt)* }, regs: { $($rs:tt)* }, writes: $writes:expr $(,)* }) => {
    //     paste::paste! {
    //         #[test]
    //         fn [< test_ $name>]() {
    //             let cpu;
    //             let n;
    //             def_tests!(@base cpu: cpu, cycles: n, rom: { $($instr)* });
    //             def_tests!(@assert_regs cpu { $($rs)* });
    //             def_tests!(@assert_writes cpu $writes);
    //             assert!(n == $cycles, "CPU cycle count mismatch:\nExpected: {:>3?}\nActual:   {:>3?}", $cycles, n);
    //         }
    //     }
    // };
    (@pre {system: $system:ident}, premem: [$($writes:expr),*] $($x:tt)*) => {
        let mut system = $system.as_ref();
        for (addr, value) in [$($writes),*] {
            debug!("Setting memory at {:#06x} to {:#04x} before run", addr, value);
            super::jit_helper_write(&mut system, addr, value);
        }
        def_tests!(@pre {system: $system} $($x)*);
    };
    (@pre {system: $system:ident}, prereg: {$($reg:ident : $val:expr),+} $($x:tt)*) => {
        let mut system = $system.as_ref();
        $(
            debug!("Setting register `{}` to `{:#x}` before run", stringify!($reg), $val);
            system.cpu.registers.$reg = $val;
        )+
        def_tests!(@pre {system: $system} $($x)*);
    };
    (@pre {system: $system:ident} , rom : { $($instr:tt)* } $($x:tt)*) => {
        let rom = gbc_asm! { $($instr)* };
        let cart = crate::Cart::new_raw(rom);
        $system = crate::System::new(cart);
        def_tests!(@pre {system: $system} $($x)*);
    };
    (@pre $vars:tt, cycles: $_:tt $($x:tt)*) => {
        def_tests!(@pre $vars $($x)*);
    };
    (@pre $vars:tt, regs: $_:tt $($x:tt)*) => {
        def_tests!(@pre $vars $($x)*);
    };
    (@pre {system: $system:ident}, $label:ident : $val:tt $($x:tt)*) => {
        trace!("def_tests! @pre is ignoring {}: {}", stringify!($label), stringify!($val));
        trace!("def_tests! @pre rest is {}", stringify!($($x)*));
        def_tests!(@pre {system: $system} $($x)*);
    };
    (@pre $vars:tt) => {};
    // (@pre $vars:tt,) => {};
    (@post $vars:tt;) => {};
    (@post {cpu: $cpu:ident, cycles: $actual:ident, writes: $writes:ident}, cycles: $expected:literal $($x:tt)*) => {
        assert!($expected == $actual, "Cycle count mismatch! Expected: {}, actual: {}", $expected, $actual);
        def_tests!(@post {cpu: $cpu, cycles: $actual, writes: $writes} $($x)*);
    };
    (@post {cpu: $cpu:ident, cycles: $cycles:ident, writes: $writes:ident}, regs: { $($rs:tt)+ } $($x:tt)*) => {
        let expected_regs = Registers {
            $($rs)*,
            ..Registers::default()
        };
        let actual_regs = $cpu.get_registers();
        assert_eq!(&expected_regs, actual_regs, "Registers mismatch (expected / actual)");
        println!("Registers matched!");
        def_tests!(@post {cpu: $cpu, cycles: $cycles, writes: $writes} $($x)*);
    };
    (@post {cpu: $cpu:ident, cycles: $cycles:ident, writes: $actual:ident}, writes: [$($writes:expr),*] $($x:tt)*) => {
        let expected = [$($writes),*].into_iter().map(WriteRecord::from).collect::<Vec<WriteRecord>>();
        let actual: &[super::WriteRecord] = &$actual;
        assert_eq!(&expected, actual, "Write mismatch (expected / actual)");
        println!("Writes matched!");
        def_tests!(@post {cpu: $cpu, cycles: $cycles, writes: $actual} $($x)*);
    };
    (@post $vars:tt, premem: $_:tt $($x:tt)*) => {
        def_tests!(@post $vars $($x)*);
    };
    (@post $vars:tt, prereg: $_:tt $($x:tt)*) => {
        def_tests!(@post $vars $($x)*);
    };
    (@post $vars:tt, rom: $_:tt $($x:tt)*) => {
        def_tests!(@post $vars $($x)*);
    };
    (@post $vars:tt, $label:ident : $val:tt $($x:tt)*) => {
        panic!("def_tests! @post is ignoring {}: {}", stringify!($label), stringify!($val));
        // error!("def_tests! @post rest is {}", stringify!($($x)*));
        // def_tests!(@post {cpu: $cpu, cycles: $cycles} $($x)*);
    };
    (@post $vars:tt) => {};
}

def_tests! {
    // 0x0x
    { name: x00_nop, cycles: 4, rom: { ; nop }, regs: { pc: 0x0001 } },
    { name: x01_ld_bc_nn, cycles: 12, rom: { ; ld bc, 0x1234 }, regs: { b: 0x12, c: 0x34, pc: 0x0003 } },
    { name: x02_ld_indbc_a, cycles: 8, rom: { ; ld (bc), a }, regs: { pc: 0x0001 }, writes: [(0x0000, 0x00)] },
    { name: x03_inc_bc, cycles: 8, rom: { ; inc bc }, regs: { c: 0x01, pc: 0x0001 } },
    { name: x04_inc_b, cycles: 4, rom: { ; inc b }, regs: { b: 0x01, pc: 0x0001 } },
    { name: x05_dec_b, cycles: 4, rom: { ; dec b }, regs: { b: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x05_dec_b__zero, cycles: 12, rom: { ; ld b, 0x01 ; dec b }, regs: { b: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x06_ld_b_n, cycles: 8, rom: { ; ld b, 0x12 }, regs: { b: 0x12, pc: 0x0002} },
    { name: x07_rlca, cycles: 4, rom: { ; rlca }, regs: { pc: 0x0001 } },
    { name: x07_rlca__carry, cycles: 12, rom: { ; ld a, 0x80 ; rlca }, regs: { a: 0x01, f: CARRY, pc: 0x0003 } },
    { name: x08_ld_indnn_sp, cycles: 20, rom: { ; ld (0x1234), sp }, regs: { pc: 0x0003 }, writes: [(0x1234, 0x00), (0x1235, 0x00)] },
    { name: x09_add_hl_bc, cycles: 20, rom: { ; ld bc, 0x1234 ; add hl, bc }, regs: { b: 0x12, c: 0x34, h: 0x12, l: 0x34, pc: 0x0004 } },
    { name: x0a_ld_a_indbc, cycles: 8, rom: { ; ld a, (bc) }, regs: { a: 0x0a, pc: 0x0001 } },
    { name: x0b_dec_bc, cycles: 8, rom: { ; dec bc }, regs: { b: 0xff, c: 0xff, pc: 0x0001 } },
    { name: x0c_inc_c, cycles: 4, rom: { ; inc c }, regs: { c: 0x01, pc: 0x0001 } },
    { name: x05_dec_c, cycles: 4, rom: { ; dec c }, regs: { c: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x05_dec_c__zero, cycles: 12, rom: { ; ld c, 0x01 ; dec c }, regs: { c: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x0e_ld_c_n, cycles: 8, rom: { ; ld c, 0x12 }, regs: { c: 0x12, pc: 0x0002 } },
    { name: x0f_rrca, cycles: 4, rom: { ; rrca }, regs: { pc: 0x0001 } },
    { name: x0f_rrca__carry, cycles: 12, rom: { ; ld a, 0x1 ; rrca }, regs: { a: 0x80, f: CARRY, pc: 0x0003 } },
    // 0x1x
    { name: x10_stop, cycles: 4, rom: { ; stop }, regs: { pc: 0x0002 } },
    { name: x11_ld_de_nn, cycles: 12, rom: { ; ld de, 0x1234 }, regs: { d: 0x12, e: 0x34, pc: 0x0003 } },
    { name: x12_ld_indde_a, cycles: 8, rom: { ; ld (de), a }, regs: { pc: 0x0001 }, writes: [(0x0000, 0x00)] },
    { name: x13_inc_de, cycles: 8, rom: { ; inc de }, regs: { e: 0x01, pc: 0x0001 } },
    { name: x14_inc_d, cycles: 4, rom: { ; inc d }, regs: { d: 0x01, pc: 0x0001 } },
    { name: x15_dec_d, cycles: 4, rom: { ; dec d }, regs: { d: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x15_dec_d__zero, cycles: 12, rom: { ; ld d, 0x01 ; dec d }, regs: { d: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x16_ld_d_n, cycles: 8, rom: { ; ld d, 0x12 }, regs: { d: 0x12, pc: 0x0002} },
    { name: x17_rla, cycles: 4, rom: { ; rla }, regs: { pc: 0x0001 } },
    { name: x17_rla__carry, cycles: 12, rom: { ; ld a, 0x80 ; rla }, regs: { a: 0, f: CARRY, pc: 0x0003 } },
    { name: x18_jr_s, cycles: 12, rom: { ; jr 0x10 }, regs: { pc: 0x0012 } },
    { name: x19_add_hl_de, cycles: 20, rom: { ; ld de, 0x1234 ; add hl, de }, regs: { d: 0x12, e: 0x34, h: 0x12, l: 0x34, pc: 0x0004 } },
    { name: x1a_ld_a_indde, cycles: 8, rom: { ; ld a, (de) }, regs: { a: 0x1a, pc: 0x0001 } },
    { name: x1b_dec_de, cycles: 8, rom: { ; dec de }, regs: { d: 0xff, e: 0xff, pc: 0x0001 } },
    { name: x1c_inc_e, cycles: 4, rom: { ; inc e }, regs: { e: 0x01, pc: 0x0001 } },
    { name: x15_dec_e, cycles: 4, rom: { ; dec e }, regs: { e: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x15_dec_e__zero, cycles: 12, rom: { ; ld e, 0x01 ; dec e }, regs: { e: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x1e_ld_e_n, cycles: 8, rom: { ; ld e, 0x12 }, regs: { e: 0x12, pc: 0x0002 } },
    { name: x1f_rra, cycles: 4, rom: { ; rra }, regs: { pc: 0x0001 } },
    { name: x1f_rra__carry, cycles: 12, rom: { ; ld a, 0x1 ; rra }, regs: { a: 0x00, f: CARRY, pc: 0x0003 } },
    // 0x2x
    { name: x20_jr_nz_s, cycles: 12, rom: { ; jr nz, 0x10 }, regs: { pc: 0x0012 } },
    { name: x21_ld_hl_nn, cycles: 12, rom: { ; ld hl, 0x1234 }, regs: { h: 0x12, l: 0x34, pc: 0x0003 } },
    { name: x22_ld_indhli_a, cycles: 8, rom: { ; ld (hli), a }, regs: { l: 0x01, pc: 0x0001 }, writes: [(0x0000, 0x00)] },
    { name: x23_inc_hl, cycles: 8, rom: { ; inc hl }, regs: { l: 0x01, pc: 0x0001 } },
    { name: x24_inc_h, cycles: 4, rom: { ; inc h }, regs: { h: 0x01, pc: 0x0001 } },
    { name: x25_dec_h, cycles: 4, rom: { ; dec h }, regs: { h: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x25_dec_h__zero, cycles: 12, rom: { ; ld h, 0x01 ; dec h }, regs: { h: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x26_ld_h_n, cycles: 8, rom: { ; ld h, 0x12 }, regs: { h: 0x12, pc: 0x0002} },
    { name: x27_daa, cycles: 4, rom: { ; daa }, regs: { pc: 0x0001 } },
    { name: x27_daa__adjust, cycles: 24, rom: { ; ld a, 0x22; ld b, 0x39; add a, b; daa }, regs: { a: 0x61, b: 0x39, pc: 0x0006 } },
    { name: x28_jr_z_s, cycles: 8, rom: { ; jr z, 0x10 }, regs: { pc: 0x0002 } },
    { name: x29_add_hl_hl, cycles: 8, rom: { ; add hl, hl }, regs: { pc: 0x0001 } },
    { name: x2a_ld_a_indhli, cycles: 8, rom: { ; ld a, (hli) }, regs: { a: 0x2a, l: 0x01, pc: 0x0001 } },
    { name: x2b_dec_hl, cycles: 8, rom: { ; dec hl }, regs: { h: 0xff, l: 0xff, pc: 0x0001 } },
    { name: x2c_inc_l, cycles: 4, rom: { ; inc l }, regs: { l: 0x01, pc: 0x0001 } },
    { name: x2d_dec_l, cycles: 4, rom: { ; dec l }, regs: { l: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x2d_dec_l__zero, cycles: 12, rom: { ; ld l, 0x01 ; dec l }, regs: { l: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x2e_ld_l_n, cycles: 8, rom: { ; ld l, 0x12 }, regs: { l: 0x12, pc: 0x0002 } },
    { name: x2f_cpl, cycles: 4, rom: { ; cpl }, regs: { a: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    // 0x3x
    { name: x30_jr_nc_s, cycles: 12, rom: { ; jr nc, 0x10 }, regs: { pc: 0x0012 } },
    { name: x31_sp_nn, cycles: 12, rom: { ; ld sp, 0x1234 }, regs: { sp: 0x1234, pc: 0x0003 } },
    { name: x32_ld_indhld_a, cycles: 8, rom: { ; ld (hld), a }, regs: { h: 0xff, l: 0xff, pc: 0x0001 }, writes: [(0x0000, 0x00)] },
    { name: x33_inc_sp, cycles: 8, rom: { ; inc sp }, regs: { sp: 0x0001, pc: 0x0001 } },
    { name: x34_inc_indhl, cycles: 12, rom: { ; inc (hl) }, regs: { pc: 0x0001 }, writes: [(0x0000, 0x35)] },
    { name: x35_dec_indhl, cycles: 12, rom: { ; dec (hl) }, regs: { f: SUB, pc: 0x0001 }, writes: [(0x0000, 0x34)] },
    { name: x35_dec_indhl__zero, cycles: 20, rom: { ; ld h, 0xc0 ; dec (hl) }, regs: { f: SUB | ZERO, h: 0xc0, pc: 0x0003 }, premem: [(0xc000, 0x01)], writes: [(0xc000, 0x00)] },
    { name: x36_ld_indhl_n, cycles: 12, rom: { ; ld (hl), 0x12 }, regs: { pc: 0x0002 }, writes: [(0x0000, 0x12)] },
    { name: x37_scf, cycles: 4, rom: { ; scf }, regs: { f: CARRY, pc: 0x0001 } },
    { name: x38_jr_c_s, cycles: 8, rom: { ; jr z, 0x10 }, regs: { pc: 0x0002 } },
    { name: x39_add_hl_sp, cycles: 8, rom: { ; add hl, sp }, regs: { pc: 0x0001 } },
    { name: x3a_ld_a_indhld, cycles: 8, rom: { ; ld a, (hld) }, regs: { a: 0x3a, h: 0xff, l: 0xff, pc: 0x0001 } },
    { name: x3b_dec_sp, cycles: 8, rom: { ; dec sp }, regs: { sp: 0xffff, pc: 0x0001 } },
    { name: x3c_inc_a, cycles: 4, rom: { ; inc a }, regs: { a: 0x01, pc: 0x0001 } },
    { name: x3d_dec_a, cycles: 4, rom: { ; dec a }, regs: { a: 0xff, f: SUB | HALF_CARRY, pc: 0x0001 } },
    { name: x3d_dec_a__zero, cycles: 12, rom: { ; ld a, 0x01 ; dec a }, regs: { a: 0x00, f: SUB | ZERO, pc: 0x0003 } },
    { name: x3e_ld_a_n, cycles: 8, rom: { ; ld a, 0x12 }, regs: { a: 0x12, pc: 0x0002 } },
    { name: x3f_ccf__on, cycles: 4, rom: { ; ccf }, regs: { f: CARRY, pc: 0x0001 } },
    { name: x3f_ccf__off, cycles: 8, rom: { ; scf ; ccf }, regs: { pc: 0x0002 } },
    // 0x4x
    { name: x40_ld_b_b, cycles: 4, rom: { ; ld b, b }, regs: { pc: 0x0001 } },
    { name: x41_ld_b_c, cycles: 4, rom: { ; ld b, c }, regs: { pc: 0x0001 } },
    { name: x42_ld_b_d, cycles: 4, rom: { ; ld b, d }, regs: { pc: 0x0001 } },
    { name: x43_ld_b_e, cycles: 4, rom: { ; ld b, e }, regs: { pc: 0x0001 } },
    { name: x44_ld_b_h, cycles: 4, rom: { ; ld b, h }, regs: { pc: 0x0001 } },
    { name: x45_ld_b_l, cycles: 4, rom: { ; ld b, l }, regs: { pc: 0x0001 } },
    { name: x46_ld_b_indhl, cycles: 8, rom: { ; ld b, (hl) }, regs: { pc: 0x0001 } },
    { name: x47_ld_b_a, cycles: 4, rom: { ; ld b, a }, regs: { pc: 0x0001 } },
    { name: x48_ld_c_b, cycles: 4, rom: { ; ld c, b }, regs: { pc: 0x0001 } },
    { name: x49_ld_c_c, cycles: 4, rom: { ; ld c, c }, regs: { pc: 0x0001 } },
    { name: x4a_ld_c_d, cycles: 4, rom: { ; ld c, d }, regs: { pc: 0x0001 } },
    { name: x4b_ld_c_e, cycles: 4, rom: { ; ld c, e }, regs: { pc: 0x0001 } },
    { name: x4c_ld_c_h, cycles: 4, rom: { ; ld c, h }, regs: { pc: 0x0001 } },
    { name: x4d_ld_c_l, cycles: 4, rom: { ; ld c, l }, regs: { pc: 0x0001 } },
    { name: x4e_ld_c_indhl, cycles: 8, rom: { ; ld c, (hl) }, regs: { pc: 0x0001 } },
    { name: x4f_ld_c_a, cycles: 4, rom: { ; ld c, a }, regs: { pc: 0x0001 } },
    // // 0x5x
    { name: x50_ld_d_b, cycles: 4, rom: { ; ld d, b }, regs: { pc: 0x0001 } },
    { name: x51_ld_d_c, cycles: 4, rom: { ; ld d, c }, regs: { pc: 0x0001 } },
    { name: x52_ld_d_d, cycles: 4, rom: { ; ld d, d }, regs: { pc: 0x0001 } },
    { name: x53_ld_d_e, cycles: 4, rom: { ; ld d, e }, regs: { pc: 0x0001 } },
    { name: x54_ld_d_h, cycles: 4, rom: { ; ld d, h }, regs: { pc: 0x0001 } },
    { name: x55_ld_d_l, cycles: 4, rom: { ; ld d, l }, regs: { pc: 0x0001 } },
    { name: x56_ld_d_indhl, cycles: 8, rom: { ; ld d, (hl) }, regs: { pc: 0x0001 } },
    { name: x57_ld_d_a, cycles: 4, rom: { ; ld d, a }, regs: { pc: 0x0001 } },
    { name: x58_ld_e_b, cycles: 4, rom: { ; ld e, b }, regs: { pc: 0x0001 } },
    { name: x59_ld_e_c, cycles: 4, rom: { ; ld e, c }, regs: { pc: 0x0001 } },
    { name: x5a_ld_e_d, cycles: 4, rom: { ; ld e, d }, regs: { pc: 0x0001 } },
    { name: x5b_ld_e_e, cycles: 4, rom: { ; ld e, e }, regs: { pc: 0x0001 } },
    { name: x5c_ld_e_h, cycles: 4, rom: { ; ld e, h }, regs: { pc: 0x0001 } },
    { name: x5d_ld_e_l, cycles: 4, rom: { ; ld e, l }, regs: { pc: 0x0001 } },
    { name: x5e_ld_e_indhl, cycles: 8, rom: { ; ld e, (hl) }, regs: { pc: 0x0001 } },
    { name: x5f_ld_e_a, cycles: 4, rom: { ; ld e, a }, regs: { pc: 0x0001 } },
    // // 0x6x
    { name: x60_ld_h_b, cycles: 4, rom: { ; ld h, b }, regs: { pc: 0x0001 } },
    { name: x61_ld_h_c, cycles: 4, rom: { ; ld h, c }, regs: { pc: 0x0001 } },
    { name: x62_ld_h_d, cycles: 4, rom: { ; ld h, d }, regs: { pc: 0x0001 } },
    { name: x63_ld_h_e, cycles: 4, rom: { ; ld h, e }, regs: { pc: 0x0001 } },
    { name: x64_ld_h_h, cycles: 4, rom: { ; ld h, h }, regs: { pc: 0x0001 } },
    { name: x65_ld_h_l, cycles: 4, rom: { ; ld h, l }, regs: { pc: 0x0001 } },
    { name: x66_ld_h_indhl, cycles: 8, rom: { ; ld h, (hl) }, regs: { pc: 0x0001 } },
    { name: x67_ld_h_a, cycles: 4, rom: { ; ld h, a }, regs: { pc: 0x0001 } },
    { name: x68_ld_l_b, cycles: 4, rom: { ; ld l, b }, regs: { pc: 0x0001 } },
    { name: x69_ld_l_c, cycles: 4, rom: { ; ld l, c }, regs: { pc: 0x0001 } },
    { name: x6a_ld_l_d, cycles: 4, rom: { ; ld l, d }, regs: { pc: 0x0001 } },
    { name: x6b_ld_l_e, cycles: 4, rom: { ; ld l, e }, regs: { pc: 0x0001 } },
    { name: x6c_ld_l_h, cycles: 4, rom: { ; ld l, h }, regs: { pc: 0x0001 } },
    { name: x6d_ld_l_l, cycles: 4, rom: { ; ld l, l }, regs: { pc: 0x0001 } },
    { name: x6e_ld_l_indhl, cycles: 8, rom: { ; ld l, (hl) }, regs: { pc: 0x0001 } },
    { name: x6f_ld_l_a, cycles: 4, rom: { ; ld l, a }, regs: { pc: 0x0001 } },
    // // 0x7x
    { name: x70_ld_indhl_b, cycles: 8, rom: { ; ld (hl), b }, regs: { pc: 0x0001 } },
    { name: x71_ld_indhl_c, cycles: 8, rom: { ; ld (hl), c }, regs: { pc: 0x0001 } },
    { name: x72_ld_indhl_d, cycles: 8, rom: { ; ld (hl), d }, regs: { pc: 0x0001 } },
    { name: x73_ld_indhl_e, cycles: 8, rom: { ; ld (hl), e }, regs: { pc: 0x0001 } },
    { name: x74_ld_indhl_h, cycles: 8, rom: { ; ld (hl), h }, regs: { pc: 0x0001 } },
    { name: x75_ld_indhl_l, cycles: 8, rom: { ; ld (hl), l }, regs: { pc: 0x0001 } },
    { name: x76_ld_halt, cycles: 4, rom: { ; halt }, regs: { pc: 0x0001 } },
    { name: x77_ld_indhl_a, cycles: 8, rom: { ; ld (hl), a }, regs: { pc: 0x0001 } },
    { name: x78_ld_a_b, cycles: 4, rom: { ; ld a, b }, regs: { pc: 0x0001 } },
    { name: x79_ld_a_c, cycles: 4, rom: { ; ld a, c }, regs: { pc: 0x0001 } },
    { name: x7a_ld_a_d, cycles: 4, rom: { ; ld a, d }, regs: { pc: 0x0001 } },
    { name: x7b_ld_a_e, cycles: 4, rom: { ; ld a, e }, regs: { pc: 0x0001 } },
    { name: x7c_ld_a_h, cycles: 4, rom: { ; ld a, h }, regs: { pc: 0x0001 } },
    { name: x7d_ld_a_l, cycles: 4, rom: { ; ld a, l }, regs: { pc: 0x0001 } },
    { name: x7e_ld_a_indhl, cycles: 8, rom: { ; ld a, (hl) }, regs: { pc: 0x0001 } },
    { name: x7f_ld_a_a, cycles: 4, rom: { ; ld a, a }, regs: { pc: 0x0001 } },
    // 0x8x
    { name: x80_add_a_b, cycles: 4, rom: { ; add a, b }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x81_add_a_c, cycles: 4, rom: { ; add a, c }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x82_add_a_d, cycles: 4, rom: { ; add a, d }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x83_add_a_e, cycles: 4, rom: { ; add a, e }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x84_add_a_h, cycles: 4, rom: { ; add a, h }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x85_add_a_l, cycles: 4, rom: { ; add a, l }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x86_add_a_indhl, cycles: 8, rom: { ; add a, (hl) }, regs: { a: 0x86, pc: 0x0001 } },
    { name: x87_add_a_a, cycles: 4, rom: { ; add a, a }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x88_adc_a_b, cycles: 4, rom: { ; adc a, b }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x89_adc_a_c, cycles: 4, rom: { ; adc a, c }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x8a_adc_a_d, cycles: 4, rom: { ; adc a, d }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x8b_adc_a_e, cycles: 4, rom: { ; adc a, e }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x8c_adc_a_h, cycles: 4, rom: { ; adc a, h }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x8d_adc_a_l, cycles: 4, rom: { ; adc a, l }, regs: { f: ZERO, pc: 0x0001 } },
    { name: x8e_adc_a_indhl, cycles: 8, rom: { ; adc a, (hl) }, regs: { a: 0x8e, pc: 0x0001 } },
    { name: x8f_adc_a_a, cycles: 4, rom: { ; adc a, a }, regs: { f: ZERO, pc: 0x0001 } },
    // 0x9x
    { name: x90_sub_a_b, cycles: 4, rom: { ; sub a, b }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x91_sub_a_c, cycles: 4, rom: { ; sub a, c }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x92_sub_a_d, cycles: 4, rom: { ; sub a, d }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x93_sub_a_e, cycles: 4, rom: { ; sub a, e }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x94_sub_a_h, cycles: 4, rom: { ; sub a, h }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x95_sub_a_l, cycles: 4, rom: { ; sub a, l }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x96_sub_a_indhl, cycles: 8, rom: { ; sub a, (hl) }, regs: { a: 0x6a, f: SUB | HALF_CARRY | CARRY, pc: 0x0001 } },
    { name: x97_sub_a_a, cycles: 4, rom: { ; sub a, a }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x98_sbc_a_b, cycles: 4, rom: { ; sbc a, b }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x99_sbc_a_c, cycles: 4, rom: { ; sbc a, c }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x9a_sbc_a_d, cycles: 4, rom: { ; sbc a, d }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x9b_sbc_a_e, cycles: 4, rom: { ; sbc a, e }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x9c_sbc_a_h, cycles: 4, rom: { ; sbc a, h }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x9d_sbc_a_l, cycles: 4, rom: { ; sbc a, l }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: x9e_sbc_a_indhl, cycles: 8, rom: { ; sbc a, (hl) }, regs: { a: 0x62, f: SUB | HALF_CARRY | CARRY, pc: 0x0001 } },
    { name: x9f_sbc_a_a, cycles: 4, rom: { ; sbc a, a }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    // 0xax
    { name: xa0_and_a_b, cycles: 4, rom: { ; and a, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa1_and_a_c, cycles: 4, rom: { ; and a, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa2_and_a_d, cycles: 4, rom: { ; and a, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa3_and_a_e, cycles: 4, rom: { ; and a, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa4_and_a_h, cycles: 4, rom: { ; and a, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa5_and_a_l, cycles: 4, rom: { ; and a, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa6_and_a_indhl, cycles: 8, rom: { ; and a, (hl) }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa7_and_a_a, cycles: 4, rom: { ; and a, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0001 } },
    { name: xa8_xor_a_b, cycles: 4, rom: { ; xor a, b }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xa9_xor_a_c, cycles: 4, rom: { ; xor a, c }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xaa_xor_a_d, cycles: 4, rom: { ; xor a, d }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xab_xor_a_e, cycles: 4, rom: { ; xor a, e }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xac_xor_a_h, cycles: 4, rom: { ; xor a, h }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xad_xor_a_l, cycles: 4, rom: { ; xor a, l }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xae_xor_a_indhl, cycles: 8, rom: { ; xor a, (hl) }, regs: { a: 0xae, pc: 0x0001 } },
    { name: xaf_xor_a_a, cycles: 4, rom: { ; xor a, a }, regs: { f: ZERO, pc: 0x0001 } },
    // // 0xxb
    { name: xb0_or_a_b, cycles: 4, rom: { ; or a, b }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb1_or_a_c, cycles: 4, rom: { ; or a, c }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb2_or_a_d, cycles: 4, rom: { ; or a, d }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb3_or_a_e, cycles: 4, rom: { ; or a, e }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb4_or_a_h, cycles: 4, rom: { ; or a, h }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb5_or_a_l, cycles: 4, rom: { ; or a, l }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb6_or_a_indhl, cycles: 8, rom: { ; or a, (hl) }, regs: { a: 0xb6, pc: 0x0001 } },
    { name: xb7_or_a_a, cycles: 4, rom: { ; or a, a }, regs: { f: ZERO, pc: 0x0001 } },
    { name: xb8_cp_a_b, cycles: 4, rom: { ; cp a, b }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xb9_cp_a_c, cycles: 4, rom: { ; cp a, c }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xba_cp_a_d, cycles: 4, rom: { ; cp a, d }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xbb_cp_a_e, cycles: 4, rom: { ; cp a, e }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xbc_cp_a_h, cycles: 4, rom: { ; cp a, h }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xbd_cp_a_l, cycles: 4, rom: { ; cp a, l }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    { name: xbe_cp_a_indhl, cycles: 8, rom: { ; cp a, (hl) }, regs: { f: SUB | HALF_CARRY | CARRY, pc: 0x0001 } },
    { name: xbf_cp_a_a, cycles: 4, rom: { ; cp a, a }, regs: { f: ZERO | SUB, pc: 0x0001 } },
    // 0xcx
    { name: xc0_ret_nz, cycles: 20, rom: { ; ret nz }, regs: { sp: 0x0002, pc: 0x00c0 } },
    { name: xc1_pop_bc, cycles: 12, rom: { ; pop bc }, regs: { b: 0xc1, c: 0x00, sp: 0x0002, pc: 0x0001 } },
    { name: xc2_jp_nz_nn, cycles: 16, rom: { ; jp nz, 0x1234 }, regs: { pc: 0x1234 } },
    { name: xc3_jp_nn, cycles: 16, rom: { ; jp 0x1234 }, regs: { pc: 0x1234 } },
    { name: xc4_call_nz_nn, cycles: 24, rom: { ; call nz, 0x1234 }, regs: { sp: 0xfffe, pc: 0x1234 }, writes: [(0xffff, 0x00), (0xfffe, 0x03)] },
    { name: xc5_push_bc, cycles: 16, rom: { ; push bc }, regs: { sp: 0xfffe, pc: 0x0001 }, writes: [(0xffff, 0x00), (0xfffe, 0x00)] },
    { name: xc6_add_a_n, cycles: 8, rom: { ; add a, 0x12 }, regs: { a: 0x12, pc: 0x0002 } },
    { name: xc7_rst_00, cycles: 16, rom: { ; rst 0x00 }, regs: { sp: 0xfffe, pc: 0x0000 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    { name: xc8_ret_z, cycles: 8, rom: { ; ret z }, regs: { pc: 0x0001 } },
    { name: xc9_ret, cycles: 16, rom: { ; ret }, regs: { sp: 0x0002, pc: 0x00c9 } },
    { name: xca_jp_z_nn, cycles: 12, rom: { ; jp z, 0x1234 }, regs: { pc: 0x0003 } },
    // { name: xcb_prefix_CB, cycles: todo!() }
    { name: xcc_call_z_nn, cycles: 12, rom: { ; call z, 0x1234 }, regs: { pc: 0x0003 } },
    { name: xcd_call_nn, cycles: 24, rom: { ; call 0x1234 }, regs: { sp: 0xfffe, pc: 0x1234 }, writes: [(0xffff, 0x00), (0xfffe, 0x03)] },
    { name: xce_adc_a_n, cycles: 8, rom: { ; adc a, 0x12 }, regs: { a: 0x12, pc: 0x0002 } },
    { name: xcf_rst_08, cycles: 16, rom: { ; rst 0x08 }, regs: { sp: 0xfffe, pc: 0x0008 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    // 0xdx
    { name: xd0_ret_nc, cycles: 20, rom: { ; ret nc }, regs: { sp: 0x0002, pc: 0x00d0 } },
    { name: xd1_pop_de, cycles: 12, rom: { ; pop de }, regs: { d: 0xd1, e: 0x00, sp: 0x0002, pc: 0x0001 } },
    { name: xd2_jp_nc_nn, cycles: 16, rom: { ; jp nc, 0x1234 }, regs: { pc: 0x1234 } },
    { name: xd4_call_nc_nn, cycles: 24, rom: { ; call nc, 0x1234 }, regs: { sp: 0xfffe, pc: 0x1234 }, writes: [(0xffff, 0x00), (0xfffe, 0x03)] },
    { name: xd5_push_de, cycles: 16, rom: { ; push de }, regs: { sp: 0xfffe, pc: 0x0001 }, writes: [(0xffff, 0x00), (0xfffe, 0x00)] },
    { name: xd6_sub_a_n, cycles: 8, rom: { ; sub a, 0x12 }, regs: { a: 0xee, f: SUB | HALF_CARRY | CARRY, pc: 0x0002 } },
    { name: xd7_rst_10, cycles: 16, rom: { ; rst 0x10 }, regs: { sp: 0xfffe, pc: 0x0010 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    { name: xd8_ret_c, cycles: 8, rom: { ; ret c }, regs: { pc: 0x0001 } },
    // { name: xd9_reti, cycles: 16, rom: { ; reti }, regs: { sp: 0xfffe, pc: 0x00c9 } },
    { name: xda_jp_c_nn, cycles: 12, rom: { ; jp z, 0x1234 }, regs: { pc: 0x0003 } },
    { name: xdc_call_c_nn, cycles: 12, rom: { ; call c, 0x1234 }, regs: { pc: 0x0003 } },
    { name: xde_sbc_a_n, cycles: 8, rom: { ; sbc a, 0x12 }, regs: { a: 0xee, f: SUB | HALF_CARRY | CARRY, pc: 0x0002 } },
    { name: xdf_rst_18, cycles: 16, rom: { ; rst 0x18 }, regs: { sp: 0xfffe, pc: 0x0018 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    // 0xex
    { name: xe0_ldh_indn_a, cycles: 12, rom: { ; ldh (0x80), a }, regs: { pc: 0x0002 }, writes: [(0xff80, 0x00)] },
    { name: xe1_pop_hl, cycles: 12, rom: { ; pop hl }, regs: { h: 0xe1, l: 0x00, sp: 0x0002, pc: 0x0001 } },
    { name: xe2_ldh_indc_a, cycles: 16, rom: { ; ld c, 0x80 ; ldh (c), a }, regs: { c: 0x80, pc: 0x0003 }, writes: [(0xff80, 0x00)] },
    { name: xe5_push_hl, cycles: 16, rom: { ; push hl }, regs: { sp: 0xfffe, pc: 0x0001 }, writes: [(0xffff, 0x00), (0xfffe, 0x00)] },
    { name: xe6_and_a_n, cycles: 8, rom: { ; and a, 0x12 }, regs: { a: 0x00, f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xe7_rst_20, cycles: 16, rom: { ; rst 0x20 }, regs: { sp: 0xfffe, pc: 0x0020 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    { name: xe8_add_sp_d, cycles: 16, rom: { ; add sp, 6 }, regs: { sp: 0x0006, pc: 0x0002 } },
    { name: xe9_jp_hl, cycles: 4, rom: { ; jp hl }, regs: { pc: 0x0000 } },
    { name: xea_ld_indnn_a, cycles: 16, rom: { ; ld (0x1234), a }, regs: { pc: 0x0003 }, writes: [(0x1234, 0x00)] },
    { name: xee_xor_a_n, cycles: 8, rom: { ; xor a, 0x12 }, regs: { a: 0x12, pc: 0x0002 } },
    { name: xdf_rst_28, cycles: 16, rom: { ; rst 0x28 }, regs: { sp: 0xfffe, pc: 0x0028 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    // 0xfx
    { name: xf0_ldh_a_indn, cycles: 12, rom: { ; ldh a, (0x80) }, regs: { pc: 0x0002 } },
    { name: xf1_pop_af, cycles: 12, rom: { ; pop af }, regs: { a: 0xf1, sp: 0x0002, pc: 0x0001 } },
    { name: xf2_ldh_a_indc, cycles: 16, rom: { ; ld c, 0x80 ; ldh a, (c) }, regs: { a: 0x00, c: 0x80, pc: 0x0003 } },
    { name: xf3_di, cycles: 4, rom: { ; di }, regs: { pc: 0x0001 } },
    { name: xf5_push_af, cycles: 16, rom: { ; push af }, regs: { sp: 0xfffe, pc: 0x0001 }, writes: [(0xffff, 0x00), (0xfffe, 0x00)] },
    { name: xf6_or_a_n, cycles: 8, rom: { ; or a, 0x12 }, regs: { a: 0x12, pc: 0x0002 } },
    { name: xf7_rst_30, cycles: 16, rom: { ; rst 0x30 }, regs: { sp: 0xfffe, pc: 0x0030 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    { name: xf8_ld_hl_sp_d, cycles: 12, rom: { ; ld hl, sp + 6 }, regs: { h: 0x00, l: 0x06, pc: 0x0002 } },
    { name: xf9_ld_sp_hl, cycles: 8, rom: { ; ld sp, hl }, regs: { sp: 0x0000, pc: 0x0001 } },
    { name: xfa_ld_a_indnn, cycles: 16, rom: { ; ld a, (0x1234) }, regs: { pc: 0x0003 } },
    { name: xfb_ei, cycles: 4, rom: { ; ei }, regs: { pc: 0x0001, interrupt_master_enable: 1 } },
    { name: xfe_cp_a_n, cycles: 8, rom: { ; cp a, 0x12 }, regs: { f: SUB | HALF_CARRY | CARRY, pc: 0x0002 } },
    { name: xff_rst_38, cycles: 16, rom: { ; rst 0x38 }, regs: { sp: 0xfffe, pc: 0x0038 }, writes: [(0xffff, 0x00), (0xfffe, 0x01)] },
    // 0xcb RLC
    { name: xcb_00_rlc_b, cycles: 8, rom: { ; rlc b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_00_rlc_b__carry, cycles: 8, rom: { ; rlc b }, regs: { b: 0x03, f: CARRY, pc: 0x0002 }, prereg: { b: 0x81 } },
    { name: xcb_01_rlc_c, cycles: 8, rom: { ; rlc c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_01_rlc_c__carry, cycles: 8, rom: { ; rlc c }, regs: { c: 0x03, f: CARRY, pc: 0x0002 }, prereg: { c: 0x81 } },
    { name: xcb_02_rlc_d, cycles: 8, rom: { ; rlc d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_02_rlc_d__carry, cycles: 8, rom: { ; rlc d }, regs: { d: 0x03, f: CARRY, pc: 0x0002 }, prereg: { d: 0x81 } },
    { name: xcb_03_rlc_e, cycles: 8, rom: { ; rlc e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_03_rlc_e__carry, cycles: 8, rom: { ; rlc e }, regs: { e: 0x03, f: CARRY, pc: 0x0002 }, prereg: { e: 0x81 } },
    { name: xcb_04_rlc_h, cycles: 8, rom: { ; rlc h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_04_rlc_h__carry, cycles: 8, rom: { ; rlc h }, regs: { h: 0x03, f: CARRY, pc: 0x0002 }, prereg: { h: 0x81 } },
    { name: xcb_05_rlc_l, cycles: 8, rom: { ; rlc l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_05_rlc_l__carry, cycles: 8, rom: { ; rlc l }, regs: { l: 0x03, f: CARRY, pc: 0x0002 }, prereg: { l: 0x81 } },
    { name: xcb_06_rlc_indhl, cycles: 16, rom: { ; rlc (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0x97)] },
    { name: xcb_07_rlc_a, cycles: 8, rom: { ; rlc a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_07_rlc_a__carry, cycles: 8, rom: { ; rlc a }, regs: { a: 0x03, f: CARRY, pc: 0x0002 }, prereg: { a: 0x81 } },
    // 0xcb RRC
    { name: xcb_08_rrc_b, cycles: 8, rom: { ; rrc b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_08_rrc_b__carry, cycles: 8, rom: { ; rrc b }, regs: { b: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { b: 0x81 } },
    { name: xcb_09_rrc_c, cycles: 8, rom: { ; rrc c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_09_rrc_c__carry, cycles: 8, rom: { ; rrc c }, regs: { c: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { c: 0x81 } },
    { name: xcb_0a_rrc_d, cycles: 8, rom: { ; rrc d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_0a_rrc_d__carry, cycles: 8, rom: { ; rrc d }, regs: { d: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { d: 0x81 } },
    { name: xcb_0b_rrc_e, cycles: 8, rom: { ; rrc e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_0b_rrc_e__carry, cycles: 8, rom: { ; rrc e }, regs: { e: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { e: 0x81 } },
    { name: xcb_0c_rrc_h, cycles: 8, rom: { ; rrc h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_0c_rrc_h__carry, cycles: 8, rom: { ; rrc h }, regs: { h: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { h: 0x81 } },
    { name: xcb_0d_rrc_l, cycles: 8, rom: { ; rrc l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_0d_rrc_l__carry, cycles: 8, rom: { ; rrc l }, regs: { l: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { l: 0x81 } },
    { name: xcb_0e_rrc_indhl, cycles: 16, rom: { ; rrc (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0xe5)] },
    { name: xcb_0f_rrc_a, cycles: 8, rom: { ; rrc a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_0f_rrc_a__carry, cycles: 8, rom: { ; rrc a }, regs: { a: 0xc0, f: CARRY, pc: 0x0002 }, prereg: { a: 0x81 } },
    // 0xcb RL
    { name: xcb_10_rl_b, cycles: 8, rom: { ; rl b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_10_rl_b__from_carry, cycles: 8, rom: { ; rl b }, regs: { b: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_10_rl_b__into_carry, cycles: 8, rom: { ; rl b }, regs: { b: 0x02, f: CARRY, pc: 0x0002 }, prereg: { b: 0x81 } },
    { name: xcb_11_rl_c, cycles: 8, rom: { ; rl c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_11_rl_c__from_carry, cycles: 8, rom: { ; rl c }, regs: { c: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_11_rl_c__into_carry, cycles: 8, rom: { ; rl c }, regs: { c: 0x02, f: CARRY, pc: 0x0002 }, prereg: { c: 0x81 } },
    { name: xcb_12_rl_d, cycles: 8, rom: { ; rl d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_12_rl_d__from_carry, cycles: 8, rom: { ; rl d }, regs: { d: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_12_rl_d__into_carry, cycles: 8, rom: { ; rl d }, regs: { d: 0x02, f: CARRY, pc: 0x0002 }, prereg: { d: 0x81 } },
    { name: xcb_13_rl_e, cycles: 8, rom: { ; rl e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_13_rl_e__from_carry, cycles: 8, rom: { ; rl e }, regs: { e: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_13_rl_e__into_carry, cycles: 8, rom: { ; rl e }, regs: { e: 0x02, f: CARRY, pc: 0x0002 }, prereg: { e: 0x81 } },
    { name: xcb_14_rl_h, cycles: 8, rom: { ; rl h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_14_rl_h__from_carry, cycles: 8, rom: { ; rl h }, regs: { h: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_14_rl_h__into_carry, cycles: 8, rom: { ; rl h }, regs: { h: 0x02, f: CARRY, pc: 0x0002 }, prereg: { h: 0x81 } },
    { name: xcb_15_rl_l, cycles: 8, rom: { ; rl l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_15_rl_l__from_carry, cycles: 8, rom: { ; rl l }, regs: { l: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_15_rl_l__into_carry, cycles: 8, rom: { ; rl l }, regs: { l: 0x02, f: CARRY, pc: 0x0002 }, prereg: { l: 0x81 } },
    { name: xcb_16_rl_indhl, cycles: 16, rom: { ; rl (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0x96)] },
    { name: xcb_17_rl_a, cycles: 8, rom: { ; rl a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_17_rl_a__from_carry, cycles: 8, rom: { ; rl a }, regs: { a: 0x01, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_17_rl_a__into_carry, cycles: 8, rom: { ; rl a }, regs: { a: 0x02, f: CARRY, pc: 0x0002 }, prereg: { a: 0x81 } },
    // 0xcb RR
    { name: xcb_18_rr_b, cycles: 8, rom: { ; rr b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_18_rr_b__from_carry, cycles: 8, rom: { ; rr b }, regs: { b: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_18_rr_b__into_carry, cycles: 8, rom: { ; rr b }, regs: { b: 0x40, f: CARRY, pc: 0x0002 }, prereg: { b: 0x81 } },
    { name: xcb_19_rr_c, cycles: 8, rom: { ; rr c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_19_rr_c__from_carry, cycles: 8, rom: { ; rr c }, regs: { c: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_19_rr_c__into_carry, cycles: 8, rom: { ; rr c }, regs: { c: 0x40, f: CARRY, pc: 0x0002 }, prereg: { c: 0x81 } },
    { name: xcb_1a_rr_d, cycles: 8, rom: { ; rr d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_1a_rr_d__from_carry, cycles: 8, rom: { ; rr d }, regs: { d: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_1a_rr_d__into_carry, cycles: 8, rom: { ; rr d }, regs: { d: 0x40, f: CARRY, pc: 0x0002 }, prereg: { d: 0x81 } },
    { name: xcb_1b_rr_e, cycles: 8, rom: { ; rr e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_1b_rr_e__from_carry, cycles: 8, rom: { ; rr e }, regs: { e: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_1b_rr_e__into_carry, cycles: 8, rom: { ; rr e }, regs: { e: 0x40, f: CARRY, pc: 0x0002 }, prereg: { e: 0x81 } },
    { name: xcb_1c_rr_h, cycles: 8, rom: { ; rr h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_1c_rr_h__from_carry, cycles: 8, rom: { ; rr h }, regs: { h: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_1c_rr_h__into_carry, cycles: 8, rom: { ; rr h }, regs: { h: 0x40, f: CARRY, pc: 0x0002 }, prereg: { h: 0x81 } },
    { name: xcb_1d_rr_l, cycles: 8, rom: { ; rr l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_1d_rr_l__from_carry, cycles: 8, rom: { ; rr l }, regs: { l: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_1d_rr_l__into_carry, cycles: 8, rom: { ; rr l }, regs: { l: 0x40, f: CARRY, pc: 0x0002 }, prereg: { l: 0x81 } },
    { name: xcb_1e_rr_indhl, cycles: 16, rom: { ; rr (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0x65)] },
    { name: xcb_1f_rr_a, cycles: 8, rom: { ; rr a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_1f_rr_a__from_carry, cycles: 8, rom: { ; rr a }, regs: { a: 0x80, pc: 0x0002 }, prereg: { f: CARRY } },
    { name: xcb_1f_rr_a__into_carry, cycles: 8, rom: { ; rr a }, regs: { a: 0x40, f: CARRY, pc: 0x0002 }, prereg: { a: 0x81 } },
    // 0xcb SLA
    { name: xcb_20_sla_b, cycles: 8, rom: { ; sla b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_20_sla_b__into_carry, cycles: 8, rom: { ; sla b }, regs: { b: 0x80, f: CARRY, pc: 0x0002 }, prereg: { b: 0xc0 } },
    { name: xcb_21_sla_c, cycles: 8, rom: { ; sla c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_21_sla_c__into_carry, cycles: 8, rom: { ; sla c }, regs: { c: 0x80, f: CARRY, pc: 0x0002 }, prereg: { c: 0xc0 } },
    { name: xcb_22_sla_d, cycles: 8, rom: { ; sla d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_22_sla_d__into_carry, cycles: 8, rom: { ; sla d }, regs: { d: 0x80, f: CARRY, pc: 0x0002 }, prereg: { d: 0xc0 } },
    { name: xcb_23_sla_e, cycles: 8, rom: { ; sla e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_23_sla_e__into_carry, cycles: 8, rom: { ; sla e }, regs: { e: 0x80, f: CARRY, pc: 0x0002 }, prereg: { e: 0xc0 } },
    { name: xcb_24_sla_h, cycles: 8, rom: { ; sla h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_24_sla_h__into_carry, cycles: 8, rom: { ; sla h }, regs: { h: 0x80, f: CARRY, pc: 0x0002 }, prereg: { h: 0xc0 } },
    { name: xcb_25_sla_l, cycles: 8, rom: { ; sla l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_25_sla_l__into_carry, cycles: 8, rom: { ; sla l}, regs: { l: 0x80, f: CARRY, pc: 0x0002 }, prereg: { l: 0xc0 } },
    { name: xcb_26_sla_indhl, cycles: 16, rom: { ; sla (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0x96)] },
    { name: xcb_27_sla_a, cycles: 8, rom: { ; sla a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_27_sla_a__into_carry, cycles: 8, rom: { ; sla a }, regs: { a: 0x80, f: CARRY, pc: 0x0002 }, prereg: { a: 0xc0 } },
    // 0xcb SRA
    { name: xcb_28_sra_b, cycles: 8, rom: { ; sra b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_28_sra_b__into_carry, cycles: 8, rom: { ; sra b }, regs: { b: 0x01, f: CARRY, pc: 0x0002 }, prereg: { b: 0x03 } },
    { name: xcb_28_sra_b__signed, cycles: 8, rom: { ; sra b }, regs: { b: 0xc0, pc: 0x0002 }, prereg: { b: 0x80 } },
    { name: xcb_29_sra_c, cycles: 8, rom: { ; sra c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_29_sra_c__into_carry, cycles: 8, rom: { ; sra c }, regs: { c: 0x01, f: CARRY, pc: 0x0002 }, prereg: { c: 0x03 } },
    { name: xcb_29_sra_c__signed, cycles: 8, rom: { ; sra c }, regs: { c: 0xc0, pc: 0x0002 }, prereg: { c: 0x80 } },
    { name: xcb_2a_sra_d, cycles: 8, rom: { ; sra d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_2a_sra_d__into_carry, cycles: 8, rom: { ; sra d }, regs: { d: 0x01, f: CARRY, pc: 0x0002 }, prereg: { d: 0x03 } },
    { name: xcb_2a_sra_d__signed, cycles: 8, rom: { ; sra d }, regs: { d: 0xc0, pc: 0x0002 }, prereg: { d: 0x80 } },
    { name: xcb_2b_sra_e, cycles: 8, rom: { ; sra e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_2b_sra_e__into_carry, cycles: 8, rom: { ; sra e }, regs: { e: 0x01, f: CARRY, pc: 0x0002 }, prereg: { e: 0x03 } },
    { name: xcb_2b_sra_e__signed, cycles: 8, rom: { ; sra e }, regs: { e: 0xc0, pc: 0x0002 }, prereg: { e: 0x80 } },
    { name: xcb_2c_sra_h, cycles: 8, rom: { ; sra h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_2c_sra_h__into_carry, cycles: 8, rom: { ; sra h }, regs: { h: 0x01, f: CARRY, pc: 0x0002 }, prereg: { h: 0x03 } },
    { name: xcb_2c_sra_h__signed, cycles: 8, rom: { ; sra h }, regs: { h: 0xc0, pc: 0x0002 }, prereg: { h: 0x80 } },
    { name: xcb_2d_sra_l, cycles: 8, rom: { ; sra l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_2d_sra_l__into_carry, cycles: 8, rom: { ; sra l }, regs: { l: 0x01, f: CARRY, pc: 0x0002 }, prereg: { l: 0x03 } },
    { name: xcb_2d_sra_l__signed, cycles: 8, rom: { ; sra l }, regs: { l: 0xc0, pc: 0x0002 }, prereg: { l: 0x80 } },
    { name: xcb_2e_sra_indhl, cycles: 16, rom: { ; sra (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0xe5)] },
    { name: xcb_2f_sra_a, cycles: 8, rom: { ; sra a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_2f_sra_a__into_carry, cycles: 8, rom: { ; sra a }, regs: { a: 0x01, f: CARRY, pc: 0x0002 }, prereg: { a: 0x03 } },
    { name: xcb_2f_sra_a__signed, cycles: 8, rom: { ; sra a }, regs: { a: 0xc0, pc: 0x0002 }, prereg: { a: 0x80 } },
    // 0xcb SWAP
    { name: xcb_30_swap_b, cycles: 8, rom: { ; swap b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_30_swap_b__nonzero, cycles: 8, rom: { ; swap b }, regs: { b: 0x21, pc: 0x0002 }, prereg: { b: 0x12 } },
    { name: xcb_31_swap_c, cycles: 8, rom: { ; swap c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_31_swap_c__nonzero, cycles: 8, rom: { ; swap c }, regs: { c: 0x21, pc: 0x0002 }, prereg: { c: 0x12 } },
    { name: xcb_32_swap_d, cycles: 8, rom: { ; swap d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_32_swap_d__nonzero, cycles: 8, rom: { ; swap d }, regs: { d: 0x21, pc: 0x0002 }, prereg: { d: 0x12 } },
    { name: xcb_33_swap_e, cycles: 8, rom: { ; swap e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_33_swap_e__nonzero, cycles: 8, rom: { ; swap e }, regs: { e: 0x21, pc: 0x0002 }, prereg: { e: 0x12 } },
    { name: xcb_34_swap_h, cycles: 8, rom: { ; swap h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_34_swap_h__nonzero, cycles: 8, rom: { ; swap h }, regs: { h: 0x21, pc: 0x0002 }, prereg: { h: 0x12 } },
    { name: xcb_35_swap_l, cycles: 8, rom: { ; swap l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_35_swap_l__nonzero, cycles: 8, rom: { ; swap l }, regs: { l: 0x21, pc: 0x0002 }, prereg: { l: 0x12 } },
    { name: xcb_36_swap_indhl, cycles: 16, rom: { ; swap (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xbc)] },
    { name: xcb_37_swap_a, cycles: 8, rom: { ; swap a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_37_swap_a__nonzero, cycles: 8, rom: { ; swap a }, regs: { a: 0x21, pc: 0x0002 }, prereg: { a: 0x12 } },
    // 0xcb SRL
    { name: xcb_38_srl_b, cycles: 8, rom: { ; srl b }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_38_srl_b__into_carry, cycles: 8, rom: { ; srl b }, regs: { b: 0x01, f: CARRY, pc: 0x0002 }, prereg: { b: 0x03 } },
    { name: xcb_38_srl_b__signed, cycles: 8, rom: { ; srl b }, regs: { b: 0x40, pc: 0x0002 }, prereg: { b: 0x80 } },
    { name: xcb_39_srl_c, cycles: 8, rom: { ; srl c }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_39_srl_c__into_carry, cycles: 8, rom: { ; srl c }, regs: { c: 0x01, f: CARRY, pc: 0x0002 }, prereg: { c: 0x03 } },
    { name: xcb_39_srl_c__signed, cycles: 8, rom: { ; srl c }, regs: { c: 0x40, pc: 0x0002 }, prereg: { c: 0x80 } },
    { name: xcb_3a_srl_d, cycles: 8, rom: { ; srl d }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_3a_srl_d__into_carry, cycles: 8, rom: { ; srl d }, regs: { d: 0x01, f: CARRY, pc: 0x0002 }, prereg: { d: 0x03 } },
    { name: xcb_3a_srl_d__signed, cycles: 8, rom: { ; srl d }, regs: { d: 0x40, pc: 0x0002 }, prereg: { d: 0x80 } },
    { name: xcb_3b_srl_e, cycles: 8, rom: { ; srl e }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_3b_srl_e__into_carry, cycles: 8, rom: { ; srl e }, regs: { e: 0x01, f: CARRY, pc: 0x0002 }, prereg: { e: 0x03 } },
    { name: xcb_3b_srl_e__signed, cycles: 8, rom: { ; srl e }, regs: { e: 0x40, pc: 0x0002 }, prereg: { e: 0x80 } },
    { name: xcb_3c_srl_h, cycles: 8, rom: { ; srl h }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_3c_srl_h__into_carry, cycles: 8, rom: { ; srl h }, regs: { h: 0x01, f: CARRY, pc: 0x0002 }, prereg: { h: 0x03 } },
    { name: xcb_3c_srl_h__signed, cycles: 8, rom: { ; srl h }, regs: { h: 0x40, pc: 0x0002 }, prereg: { h: 0x80 } },
    { name: xcb_3d_srl_l, cycles: 8, rom: { ; srl l }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_3d_srl_l__into_carry, cycles: 8, rom: { ; srl l }, regs: { l: 0x01, f: CARRY, pc: 0x0002 }, prereg: { l: 0x03 } },
    { name: xcb_3d_srl_l__signed, cycles: 8, rom: { ; srl l }, regs: { l: 0x40, pc: 0x0002 }, prereg: { l: 0x80 } },
    { name: xcb_3e_srl_indhl, cycles: 16, rom: { ; srl (hl) }, regs: { f: CARRY, pc: 0x0002 }, writes: [(0x0000, 0x65)] },
    { name: xcb_3f_srl_a, cycles: 8, rom: { ; srl a }, regs: { f: ZERO, pc: 0x0002 } },
    { name: xcb_3f_srl_a__into_carry, cycles: 8, rom: { ; srl a }, regs: { a: 0x01, f: CARRY, pc: 0x0002 }, prereg: { a: 0x03 } },
    { name: xcb_3f_srl_a__signed, cycles: 8, rom: { ; srl a }, regs: { a: 0x40, pc: 0x0002 }, prereg: { a: 0x80 } },
    // 0xcb BIT 0
    { name: xcb_40_bit_0_b__zero, cycles: 8, rom: { ; bit 0, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_40_bit_0_b__nonzero, cycles: 8, rom: { ; bit 0, b }, regs: { b: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 0 } },
    { name: xcb_41_bit_0_c__zero, cycles: 8, rom: { ; bit 0, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_41_bit_0_c__nonzero, cycles: 8, rom: { ; bit 0, c }, regs: { c: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 0 } },
    { name: xcb_42_bit_0_d__zero, cycles: 8, rom: { ; bit 0, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_42_bit_0_d__nonzero, cycles: 8, rom: { ; bit 0, d }, regs: { d: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 0 } },
    { name: xcb_43_bit_0_e__zero, cycles: 8, rom: { ; bit 0, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_43_bit_0_e__nonzero, cycles: 8, rom: { ; bit 0, e }, regs: { e: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 0 } },
    { name: xcb_44_bit_0_h__zero, cycles: 8, rom: { ; bit 0, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_44_bit_0_h__nonzero, cycles: 8, rom: { ; bit 0, h }, regs: { h: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 0 } },
    { name: xcb_45_bit_0_l__zero, cycles: 8, rom: { ; bit 0, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_45_bit_0_l__nonzero, cycles: 8, rom: { ; bit 0, l }, regs: { l: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 0 } },
    { name: xcb_46_bit_0_indhl, cycles: 16, rom: { ; bit 0, (hl) }, regs: { f: HALF_CARRY, pc: 0x0002 } },
    { name: xcb_47_bit_0_a__zero, cycles: 8, rom: { ; bit 0, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_47_bit_0_a__nonzero, cycles: 8, rom: { ; bit 0, a }, regs: { a: 1 << 0, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 0 } },
    // 0xcb BIT 1
    { name: xcb_48_bit_1_b__zero, cycles: 8, rom: { ; bit 1, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_48_bit_1_b__nonzero, cycles: 8, rom: { ; bit 1, b }, regs: { b: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 1 } },
    { name: xcb_49_bit_1_c__zero, cycles: 8, rom: { ; bit 1, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_49_bit_1_c__nonzero, cycles: 8, rom: { ; bit 1, c }, regs: { c: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 1 } },
    { name: xcb_4a_bit_1_d__zero, cycles: 8, rom: { ; bit 1, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4a_bit_1_d__nonzero, cycles: 8, rom: { ; bit 1, d }, regs: { d: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 1 } },
    { name: xcb_4b_bit_1_e__zero, cycles: 8, rom: { ; bit 1, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4b_bit_1_e__nonzero, cycles: 8, rom: { ; bit 1, e }, regs: { e: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 1 } },
    { name: xcb_4c_bit_1_h__zero, cycles: 8, rom: { ; bit 1, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4c_bit_1_h__nonzero, cycles: 8, rom: { ; bit 1, h }, regs: { h: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 1 } },
    { name: xcb_4d_bit_1_l__zero, cycles: 8, rom: { ; bit 1, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4d_bit_1_l__nonzero, cycles: 8, rom: { ; bit 1, l }, regs: { l: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 1 } },
    { name: xcb_4e_bit_1_indhl, cycles: 16, rom: { ; bit 1, (hl) }, regs: { f: HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4f_bit_1_a__zero, cycles: 8, rom: { ; bit 1, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_4f_bit_1_a__nonzero, cycles: 8, rom: { ; bit 1, a }, regs: { a: 1 << 1, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 1 } },
    // 0xcb BIT 2
    { name: xcb_50_bit_2_b__zero, cycles: 8, rom: { ; bit 2, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_50_bit_2_b__nonzero, cycles: 8, rom: { ; bit 2, b }, regs: { b: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 2 } },
    { name: xcb_51_bit_2_c__zero, cycles: 8, rom: { ; bit 2, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_51_bit_2_c__nonzero, cycles: 8, rom: { ; bit 2, c }, regs: { c: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 2 } },
    { name: xcb_52_bit_2_d__zero, cycles: 8, rom: { ; bit 2, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_52_bit_2_d__nonzero, cycles: 8, rom: { ; bit 2, d }, regs: { d: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 2 } },
    { name: xcb_53_bit_2_e__zero, cycles: 8, rom: { ; bit 2, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_53_bit_2_e__nonzero, cycles: 8, rom: { ; bit 2, e }, regs: { e: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 2 } },
    { name: xcb_54_bit_2_h__zero, cycles: 8, rom: { ; bit 2, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_54_bit_2_h__nonzero, cycles: 8, rom: { ; bit 2, h }, regs: { h: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 2 } },
    { name: xcb_55_bit_2_l__zero, cycles: 8, rom: { ; bit 2, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_55_bit_2_l__nonzero, cycles: 8, rom: { ; bit 2, l }, regs: { l: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 2 } },
    { name: xcb_56_bit_2_indhl, cycles: 16, rom: { ; bit 2, (hl) }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_57_bit_2_a__zero, cycles: 8, rom: { ; bit 2, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_57_bit_2_a__nonzero, cycles: 8, rom: { ; bit 2, a }, regs: { a: 1 << 2, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 2 } },
    // 0xcb BIT 3
    { name: xcb_58_bit_3_b__zero, cycles: 8, rom: { ; bit 3, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_58_bit_3_b__nonzero, cycles: 8, rom: { ; bit 3, b }, regs: { b: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 3 } },
    { name: xcb_59_bit_3_c__zero, cycles: 8, rom: { ; bit 3, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_59_bit_3_c__nonzero, cycles: 8, rom: { ; bit 3, c }, regs: { c: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 3 } },
    { name: xcb_5a_bit_3_d__zero, cycles: 8, rom: { ; bit 3, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5a_bit_3_d__nonzero, cycles: 8, rom: { ; bit 3, d }, regs: { d: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 3 } },
    { name: xcb_5b_bit_3_e__zero, cycles: 8, rom: { ; bit 3, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5b_bit_3_e__nonzero, cycles: 8, rom: { ; bit 3, e }, regs: { e: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 3 } },
    { name: xcb_5c_bit_3_h__zero, cycles: 8, rom: { ; bit 3, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5c_bit_3_h__nonzero, cycles: 8, rom: { ; bit 3, h }, regs: { h: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 3 } },
    { name: xcb_5d_bit_3_l__zero, cycles: 8, rom: { ; bit 3, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5d_bit_3_l__nonzero, cycles: 8, rom: { ; bit 3, l }, regs: { l: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 3 } },
    { name: xcb_5e_bit_3_indhl, cycles: 16, rom: { ; bit 3, (hl) }, regs: { f: HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5f_bit_3_a__zero, cycles: 8, rom: { ; bit 3, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_5f_bit_3_a__nonzero, cycles: 8, rom: { ; bit 3, a }, regs: { a: 1 << 3, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 3 } },
    // 0xcb BIT 4
    { name: xcb_60_bit_4_b__zero, cycles: 8, rom: { ; bit 4, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_60_bit_4_b__nonzero, cycles: 8, rom: { ; bit 4, b }, regs: { b: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 4 } },
    { name: xcb_61_bit_4_c__zero, cycles: 8, rom: { ; bit 4, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_61_bit_4_c__nonzero, cycles: 8, rom: { ; bit 4, c }, regs: { c: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 4 } },
    { name: xcb_62_bit_4_d__zero, cycles: 8, rom: { ; bit 4, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_62_bit_4_d__nonzero, cycles: 8, rom: { ; bit 4, d }, regs: { d: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 4 } },
    { name: xcb_63_bit_4_e__zero, cycles: 8, rom: { ; bit 4, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_63_bit_4_e__nonzero, cycles: 8, rom: { ; bit 4, e }, regs: { e: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 4 } },
    { name: xcb_64_bit_4_h__zero, cycles: 8, rom: { ; bit 4, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_64_bit_4_h__nonzero, cycles: 8, rom: { ; bit 4, h }, regs: { h: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 4 } },
    { name: xcb_65_bit_4_l__zero, cycles: 8, rom: { ; bit 4, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_65_bit_4_l__nonzero, cycles: 8, rom: { ; bit 4, l }, regs: { l: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 4 } },
    { name: xcb_66_bit_4_indhl, cycles: 16, rom: { ; bit 4, (hl) }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_67_bit_4_a__zero, cycles: 8, rom: { ; bit 4, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_67_bit_4_a__nonzero, cycles: 8, rom: { ; bit 4, a }, regs: { a: 1 << 4, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 4 } },
    // 0xcb BIT 5
    { name: xcb_68_bit_5_b__zero, cycles: 8, rom: { ; bit 5, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_68_bit_5_b__nonzero, cycles: 8, rom: { ; bit 5, b }, regs: { b: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 5 } },
    { name: xcb_69_bit_5_c__zero, cycles: 8, rom: { ; bit 5, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_69_bit_5_c__nonzero, cycles: 8, rom: { ; bit 5, c }, regs: { c: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 5 } },
    { name: xcb_6a_bit_5_d__zero, cycles: 8, rom: { ; bit 5, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6a_bit_5_d__nonzero, cycles: 8, rom: { ; bit 5, d }, regs: { d: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 5 } },
    { name: xcb_6b_bit_5_e__zero, cycles: 8, rom: { ; bit 5, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6b_bit_5_e__nonzero, cycles: 8, rom: { ; bit 5, e }, regs: { e: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 5 } },
    { name: xcb_6c_bit_5_h__zero, cycles: 8, rom: { ; bit 5, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6c_bit_5_h__nonzero, cycles: 8, rom: { ; bit 5, h }, regs: { h: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 5 } },
    { name: xcb_6d_bit_5_l__zero, cycles: 8, rom: { ; bit 5, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6d_bit_5_l__nonzero, cycles: 8, rom: { ; bit 5, l }, regs: { l: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 5 } },
    { name: xcb_6e_bit_5_indhl, cycles: 16, rom: { ; bit 5, (hl) }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6f_bit_5_a__zero, cycles: 8, rom: { ; bit 5, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_6f_bit_5_a__nonzero, cycles: 8, rom: { ; bit 5, a }, regs: { a: 1 << 5, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 5 } },
    // 0xcb BIT 6
    { name: xcb_70_bit_6_b__zero, cycles: 8, rom: { ; bit 6, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_70_bit_6_b__nonzero, cycles: 8, rom: { ; bit 6, b }, regs: { b: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 6 } },
    { name: xcb_71_bit_6_c__zero, cycles: 8, rom: { ; bit 6, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_71_bit_6_c__nonzero, cycles: 8, rom: { ; bit 6, c }, regs: { c: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 6 } },
    { name: xcb_72_bit_6_d__zero, cycles: 8, rom: { ; bit 6, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_72_bit_6_d__nonzero, cycles: 8, rom: { ; bit 6, d }, regs: { d: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 6 } },
    { name: xcb_73_bit_6_e__zero, cycles: 8, rom: { ; bit 6, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_73_bit_6_e__nonzero, cycles: 8, rom: { ; bit 6, e }, regs: { e: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 6 } },
    { name: xcb_74_bit_6_h__zero, cycles: 8, rom: { ; bit 6, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_74_bit_6_h__nonzero, cycles: 8, rom: { ; bit 6, h }, regs: { h: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 6 } },
    { name: xcb_75_bit_6_l__zero, cycles: 8, rom: { ; bit 6, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_75_bit_6_l__nonzero, cycles: 8, rom: { ; bit 6, l }, regs: { l: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 6 } },
    { name: xcb_76_bit_6_indhl, cycles: 16, rom: { ; bit 6, (hl) }, regs: { f: HALF_CARRY, pc: 0x0002 } },
    { name: xcb_77_bit_6_a__zero, cycles: 8, rom: { ; bit 6, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_77_bit_6_a__nonzero, cycles: 8, rom: { ; bit 6, a }, regs: { a: 1 << 6, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 6 } },
    // 0xcb BIT 7
    { name: xcb_78_bit_7_b__zero, cycles: 8, rom: { ; bit 7, b }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_78_bit_7_b__nonzero, cycles: 8, rom: { ; bit 7, b }, regs: { b: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { b: 1 << 7 } },
    { name: xcb_79_bit_7_c__zero, cycles: 8, rom: { ; bit 7, c }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_79_bit_7_c__nonzero, cycles: 8, rom: { ; bit 7, c }, regs: { c: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { c: 1 << 7 } },
    { name: xcb_7a_bit_7_d__zero, cycles: 8, rom: { ; bit 7, d }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7a_bit_7_d__nonzero, cycles: 8, rom: { ; bit 7, d }, regs: { d: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { d: 1 << 7 } },
    { name: xcb_7b_bit_7_e__zero, cycles: 8, rom: { ; bit 7, e }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7b_bit_7_e__nonzero, cycles: 8, rom: { ; bit 7, e }, regs: { e: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { e: 1 << 7 } },
    { name: xcb_7c_bit_7_h__zero, cycles: 8, rom: { ; bit 7, h }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7c_bit_7_h__nonzero, cycles: 8, rom: { ; bit 7, h }, regs: { h: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { h: 1 << 7 } },
    { name: xcb_7d_bit_7_l__zero, cycles: 8, rom: { ; bit 7, l }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7d_bit_7_l__nonzero, cycles: 8, rom: { ; bit 7, l }, regs: { l: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { l: 1 << 7 } },
    { name: xcb_7e_bit_7_indhl, cycles: 16, rom: { ; bit 7, (hl) }, regs: { f: HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7f_bit_7_a__zero, cycles: 8, rom: { ; bit 7, a }, regs: { f: ZERO | HALF_CARRY, pc: 0x0002 } },
    { name: xcb_7f_bit_7_a__nonzero, cycles: 8, rom: { ; bit 7, a }, regs: { a: 1 << 7, f: HALF_CARRY, pc: 0x0002 }, prereg: { a: 1 << 7 } },
    // 0xcb RES 0
    { name: xcb_80_res_0_b__zero, cycles: 8, rom: { ; res 0, b }, regs: { pc: 0x0002 } },
    { name: xcb_80_res_0_b__nonzero, cycles: 8, rom: { ; res 0, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 0 } },
    { name: xcb_81_res_0_c__zero, cycles: 8, rom: { ; res 0, c }, regs: { pc: 0x0002 } },
    { name: xcb_81_res_0_c__nonzero, cycles: 8, rom: { ; res 0, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 0 } },
    { name: xcb_82_res_0_d__zero, cycles: 8, rom: { ; res 0, d }, regs: { pc: 0x0002 } },
    { name: xcb_82_res_0_d__nonzero, cycles: 8, rom: { ; res 0, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 0 } },
    { name: xcb_83_res_0_e__zero, cycles: 8, rom: { ; res 0, e }, regs: { pc: 0x0002 } },
    { name: xcb_83_res_0_e__nonzero, cycles: 8, rom: { ; res 0, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 0 } },
    { name: xcb_84_res_0_h__zero, cycles: 8, rom: { ; res 0, h }, regs: { pc: 0x0002 } },
    { name: xcb_84_res_0_h__nonzero, cycles: 8, rom: { ; res 0, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 0 } },
    { name: xcb_85_res_0_l__zero, cycles: 8, rom: { ; res 0, l }, regs: { pc: 0x0002 } },
    { name: xcb_85_res_0_l__nonzero, cycles: 8, rom: { ; res 0, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 0 } },
    { name: xcb_86_res_0_indhl, cycles: 16, rom: { ; res 0, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xca)] },
    { name: xcb_87_res_0_a__zero, cycles: 8, rom: { ; res 0, a }, regs: { pc: 0x0002 } },
    { name: xcb_87_res_0_a__nonzero, cycles: 8, rom: { ; res 0, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 0 } },
    // 0xcb RES 1
    { name: xcb_88_res_1_b__zero, cycles: 8, rom: { ; res 1, b }, regs: { pc: 0x0002 } },
    { name: xcb_88_res_1_b__nonzero, cycles: 8, rom: { ; res 1, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 1 } },
    { name: xcb_89_res_1_c__zero, cycles: 8, rom: { ; res 1, c }, regs: { pc: 0x0002 } },
    { name: xcb_89_res_1_c__nonzero, cycles: 8, rom: { ; res 1, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 1 } },
    { name: xcb_8a_res_1_d__zero, cycles: 8, rom: { ; res 1, d }, regs: { pc: 0x0002 } },
    { name: xcb_8a_res_1_d__nonzero, cycles: 8, rom: { ; res 1, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 1 } },
    { name: xcb_8b_res_1_e__zero, cycles: 8, rom: { ; res 1, e }, regs: { pc: 0x0002 } },
    { name: xcb_8b_res_1_e__nonzero, cycles: 8, rom: { ; res 1, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 1 } },
    { name: xcb_8c_res_1_h__zero, cycles: 8, rom: { ; res 1, h }, regs: { pc: 0x0002 } },
    { name: xcb_8c_res_1_h__nonzero, cycles: 8, rom: { ; res 1, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 1 } },
    { name: xcb_8d_res_1_l__zero, cycles: 8, rom: { ; res 1, l }, regs: { pc: 0x0002 } },
    { name: xcb_8d_res_1_l__nonzero, cycles: 8, rom: { ; res 1, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 1 } },
    { name: xcb_8e_res_1_indhl, cycles: 16, rom: { ; res 1, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xc9)] },
    { name: xcb_8f_res_1_a__zero, cycles: 8, rom: { ; res 1, a }, regs: { pc: 0x0002 } },
    { name: xcb_8f_res_1_a__nonzero, cycles: 8, rom: { ; res 1, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 1 } },
    // 0xcb RES 2
    { name: xcb_90_res_2_b__zero, cycles: 8, rom: { ; res 2, b }, regs: { pc: 0x0002 } },
    { name: xcb_90_res_2_b__nonzero, cycles: 8, rom: { ; res 2, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 2 } },
    { name: xcb_91_res_2_c__zero, cycles: 8, rom: { ; res 2, c }, regs: { pc: 0x0002 } },
    { name: xcb_91_res_2_c__nonzero, cycles: 8, rom: { ; res 2, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 2 } },
    { name: xcb_92_res_2_d__zero, cycles: 8, rom: { ; res 2, d }, regs: { pc: 0x0002 } },
    { name: xcb_92_res_2_d__nonzero, cycles: 8, rom: { ; res 2, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 2 } },
    { name: xcb_93_res_2_e__zero, cycles: 8, rom: { ; res 2, e }, regs: { pc: 0x0002 } },
    { name: xcb_93_res_2_e__nonzero, cycles: 8, rom: { ; res 2, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 2 } },
    { name: xcb_94_res_2_h__zero, cycles: 8, rom: { ; res 2, h }, regs: { pc: 0x0002 } },
    { name: xcb_94_res_2_h__nonzero, cycles: 8, rom: { ; res 2, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 2 } },
    { name: xcb_95_res_2_l__zero, cycles: 8, rom: { ; res 2, l }, regs: { pc: 0x0002 } },
    { name: xcb_95_res_2_l__nonzero, cycles: 8, rom: { ; res 2, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 2 } },
    { name: xcb_96_res_2_indhl, cycles: 16, rom: { ; res 2, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_97_res_2_a__zero, cycles: 8, rom: { ; res 2, a }, regs: { pc: 0x0002 } },
    { name: xcb_97_res_2_a__nonzero, cycles: 8, rom: { ; res 2, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 2 } },
    // 0xcb RES 3
    { name: xcb_98_res_3_b__zero, cycles: 8, rom: { ; res 3, b }, regs: { pc: 0x0002 } },
    { name: xcb_98_res_3_b__nonzero, cycles: 8, rom: { ; res 3, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 3 } },
    { name: xcb_99_res_3_c__zero, cycles: 8, rom: { ; res 3, c }, regs: { pc: 0x0002 } },
    { name: xcb_99_res_3_c__nonzero, cycles: 8, rom: { ; res 3, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 3 } },
    { name: xcb_9a_res_3_d__zero, cycles: 8, rom: { ; res 3, d }, regs: { pc: 0x0002 } },
    { name: xcb_9a_res_3_d__nonzero, cycles: 8, rom: { ; res 3, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 3 } },
    { name: xcb_9b_res_3_e__zero, cycles: 8, rom: { ; res 3, e }, regs: { pc: 0x0002 } },
    { name: xcb_9b_res_3_e__nonzero, cycles: 8, rom: { ; res 3, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 3 } },
    { name: xcb_9c_res_3_h__zero, cycles: 8, rom: { ; res 3, h }, regs: { pc: 0x0002 } },
    { name: xcb_9c_res_3_h__nonzero, cycles: 8, rom: { ; res 3, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 3 } },
    { name: xcb_9d_res_3_l__zero, cycles: 8, rom: { ; res 3, l }, regs: { pc: 0x0002 } },
    { name: xcb_9d_res_3_l__nonzero, cycles: 8, rom: { ; res 3, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 3 } },
    { name: xcb_9e_res_3_indhl, cycles: 16, rom: { ; res 3, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xc3)] },
    { name: xcb_9f_res_3_a__zero, cycles: 8, rom: { ; res 3, a }, regs: { pc: 0x0002 } },
    { name: xcb_9f_res_3_a__nonzero, cycles: 8, rom: { ; res 3, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 3 } },
    // 0xcb RES 4
    { name: xcb_a0_res_4_b__zero, cycles: 8, rom: { ; res 4, b }, regs: { pc: 0x0002 } },
    { name: xcb_a0_res_4_b__nonzero, cycles: 8, rom: { ; res 4, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 4 } },
    { name: xcb_a1_res_4_c__zero, cycles: 8, rom: { ; res 4, c }, regs: { pc: 0x0002 } },
    { name: xcb_a1_res_4_c__nonzero, cycles: 8, rom: { ; res 4, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 4 } },
    { name: xcb_a2_res_4_d__zero, cycles: 8, rom: { ; res 4, d }, regs: { pc: 0x0002 } },
    { name: xcb_a2_res_4_d__nonzero, cycles: 8, rom: { ; res 4, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 4 } },
    { name: xcb_a3_res_4_e__zero, cycles: 8, rom: { ; res 4, e }, regs: { pc: 0x0002 } },
    { name: xcb_a3_res_4_e__nonzero, cycles: 8, rom: { ; res 4, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 4 } },
    { name: xcb_a4_res_4_h__zero, cycles: 8, rom: { ; res 4, h }, regs: { pc: 0x0002 } },
    { name: xcb_a4_res_4_h__nonzero, cycles: 8, rom: { ; res 4, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 4 } },
    { name: xcb_a5_res_4_l__zero, cycles: 8, rom: { ; res 4, l }, regs: { pc: 0x0002 } },
    { name: xcb_a5_res_4_l__nonzero, cycles: 8, rom: { ; res 4, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 4 } },
    { name: xcb_a6_res_4_indhl, cycles: 16, rom: { ; res 4, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_a7_res_4_a__zero, cycles: 8, rom: { ; res 4, a }, regs: { pc: 0x0002 } },
    { name: xcb_a7_res_4_a__nonzero, cycles: 8, rom: { ; res 4, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 4 } },
    // 0xcb RES 5
    { name: xcb_a8_res_5_b__zero, cycles: 8, rom: { ; res 5, b }, regs: { pc: 0x0002 } },
    { name: xcb_a8_res_5_b__nonzero, cycles: 8, rom: { ; res 5, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 5 } },
    { name: xcb_a9_res_5_c__zero, cycles: 8, rom: { ; res 5, c }, regs: { pc: 0x0002 } },
    { name: xcb_a9_res_5_c__nonzero, cycles: 8, rom: { ; res 5, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 5 } },
    { name: xcb_aa_res_5_d__zero, cycles: 8, rom: { ; res 5, d }, regs: { pc: 0x0002 } },
    { name: xcb_aa_res_5_d__nonzero, cycles: 8, rom: { ; res 5, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 5 } },
    { name: xcb_ab_res_5_e__zero, cycles: 8, rom: { ; res 5, e }, regs: { pc: 0x0002 } },
    { name: xcb_ab_res_5_e__nonzero, cycles: 8, rom: { ; res 5, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 5 } },
    { name: xcb_ac_res_5_h__zero, cycles: 8, rom: { ; res 5, h }, regs: { pc: 0x0002 } },
    { name: xcb_ac_res_5_h__nonzero, cycles: 8, rom: { ; res 5, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 5 } },
    { name: xcb_ad_res_5_l__zero, cycles: 8, rom: { ; res 5, l }, regs: { pc: 0x0002 } },
    { name: xcb_ad_res_5_l__nonzero, cycles: 8, rom: { ; res 5, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 5 } },
    { name: xcb_ae_res_5_indhl, cycles: 16, rom: { ; res 5, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_af_res_5_a__zero, cycles: 8, rom: { ; res 5, a }, regs: { pc: 0x0002 } },
    { name: xcb_af_res_5_a__nonzero, cycles: 8, rom: { ; res 5, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 5 } },
    // 0xcb RES 6
    { name: xcb_b0_res_6_b__zero, cycles: 8, rom: { ; res 6, b }, regs: { pc: 0x0002 } },
    { name: xcb_b0_res_6_b__nonzero, cycles: 8, rom: { ; res 6, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 6 } },
    { name: xcb_b1_res_6_c__zero, cycles: 8, rom: { ; res 6, c }, regs: { pc: 0x0002 } },
    { name: xcb_b1_res_6_c__nonzero, cycles: 8, rom: { ; res 6, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 6 } },
    { name: xcb_b2_res_6_d__zero, cycles: 8, rom: { ; res 6, d }, regs: { pc: 0x0002 } },
    { name: xcb_b2_res_6_d__nonzero, cycles: 8, rom: { ; res 6, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 6 } },
    { name: xcb_b3_res_6_e__zero, cycles: 8, rom: { ; res 6, e }, regs: { pc: 0x0002 } },
    { name: xcb_b3_res_6_e__nonzero, cycles: 8, rom: { ; res 6, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 6 } },
    { name: xcb_b4_res_6_h__zero, cycles: 8, rom: { ; res 6, h }, regs: { pc: 0x0002 } },
    { name: xcb_b4_res_6_h__nonzero, cycles: 8, rom: { ; res 6, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 6 } },
    { name: xcb_b5_res_6_l__zero, cycles: 8, rom: { ; res 6, l }, regs: { pc: 0x0002 } },
    { name: xcb_b5_res_6_l__nonzero, cycles: 8, rom: { ; res 6, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 6 } },
    { name: xcb_b6_res_6_indhl, cycles: 16, rom: { ; res 6, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0x8b)] },
    { name: xcb_b7_res_6_a__zero, cycles: 8, rom: { ; res 6, a }, regs: { pc: 0x0002 } },
    { name: xcb_b7_res_6_a__nonzero, cycles: 8, rom: { ; res 6, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 6 } },
    // 0xcb RES 7
    { name: xcb_b8_res_7_b__zero, cycles: 8, rom: { ; res 7, b }, regs: { pc: 0x0002 } },
    { name: xcb_b8_res_7_b__nonzero, cycles: 8, rom: { ; res 7, b }, regs: { pc: 0x0002 }, prereg: { b: 1 << 7 } },
    { name: xcb_b9_res_7_c__zero, cycles: 8, rom: { ; res 7, c }, regs: { pc: 0x0002 } },
    { name: xcb_b9_res_7_c__nonzero, cycles: 8, rom: { ; res 7, c }, regs: { pc: 0x0002 }, prereg: { c: 1 << 7 } },
    { name: xcb_ba_res_7_d__zero, cycles: 8, rom: { ; res 7, d }, regs: { pc: 0x0002 } },
    { name: xcb_ba_res_7_d__nonzero, cycles: 8, rom: { ; res 7, d }, regs: { pc: 0x0002 }, prereg: { d: 1 << 7 } },
    { name: xcb_bb_res_7_e__zero, cycles: 8, rom: { ; res 7, e }, regs: { pc: 0x0002 } },
    { name: xcb_bb_res_7_e__nonzero, cycles: 8, rom: { ; res 7, e }, regs: { pc: 0x0002 }, prereg: { e: 1 << 7 } },
    { name: xcb_bc_res_7_h__zero, cycles: 8, rom: { ; res 7, h }, regs: { pc: 0x0002 } },
    { name: xcb_bc_res_7_h__nonzero, cycles: 8, rom: { ; res 7, h }, regs: { pc: 0x0002 }, prereg: { h: 1 << 7 } },
    { name: xcb_bd_res_7_l__zero, cycles: 8, rom: { ; res 7, l }, regs: { pc: 0x0002 } },
    { name: xcb_bd_res_7_l__nonzero, cycles: 8, rom: { ; res 7, l }, regs: { pc: 0x0002 }, prereg: { l: 1 << 7 } },
    { name: xcb_be_res_7_indhl, cycles: 16, rom: { ; res 7, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0x4b)] },
    { name: xcb_bf_res_7_a__zero, cycles: 8, rom: { ; res 7, a }, regs: { pc: 0x0002 } },
    { name: xcb_bf_res_7_a__nonzero, cycles: 8, rom: { ; res 7, a }, regs: { pc: 0x0002 }, prereg: { a: 1 << 7 } },


    // 0xcb SET 0
    { name: xcb_c0_set_0_b, cycles: 8, rom: { ; set 0, b }, regs: { b: 1 << 0, pc: 0x0002 } },
    { name: xcb_c1_set_0_c, cycles: 8, rom: { ; set 0, c }, regs: { c: 1 << 0, pc: 0x0002 } },
    { name: xcb_c2_set_0_d, cycles: 8, rom: { ; set 0, d }, regs: { d: 1 << 0, pc: 0x0002 } },
    { name: xcb_c3_set_0_e, cycles: 8, rom: { ; set 0, e }, regs: { e: 1 << 0, pc: 0x0002 } },
    { name: xcb_c4_set_0_h, cycles: 8, rom: { ; set 0, h }, regs: { h: 1 << 0, pc: 0x0002 } },
    { name: xcb_c5_set_0_l, cycles: 8, rom: { ; set 0, l }, regs: { l: 1 << 0, pc: 0x0002 } },
    { name: xcb_c6_set_0_indhl, cycles: 16, rom: { ; set 0, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_c7_set_0_a, cycles: 8, rom: { ; set 0, a }, regs: { a: 1 << 0, pc: 0x0002 } },
    // 0xcb SET 1
    { name: xcb_c8_set_1_b, cycles: 8, rom: { ; set 1, b }, regs: { b: 1 << 1, pc: 0x0002 } },
    { name: xcb_c9_set_1_c, cycles: 8, rom: { ; set 1, c }, regs: { c: 1 << 1, pc: 0x0002 } },
    { name: xcb_ca_set_1_d, cycles: 8, rom: { ; set 1, d }, regs: { d: 1 << 1, pc: 0x0002 } },
    { name: xcb_cb_set_1_e, cycles: 8, rom: { ; set 1, e }, regs: { e: 1 << 1, pc: 0x0002 } },
    { name: xcb_cc_set_1_h, cycles: 8, rom: { ; set 1, h }, regs: { h: 1 << 1, pc: 0x0002 } },
    { name: xcb_cd_set_1_l, cycles: 8, rom: { ; set 1, l }, regs: { l: 1 << 1, pc: 0x0002 } },
    { name: xcb_ce_set_1_indhl, cycles: 16, rom: { ; set 1, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_cf_set_1_a, cycles: 8, rom: { ; set 1, a }, regs: { a: 1 << 1, pc: 0x0002 } },
    // 0xcb SET 2
    { name: xcb_d0_set_2_b, cycles: 8, rom: { ; set 2, b }, regs: { b: 1 << 2, pc: 0x0002 } },
    { name: xcb_d1_set_2_c, cycles: 8, rom: { ; set 2, c }, regs: { c: 1 << 2, pc: 0x0002 } },
    { name: xcb_d2_set_2_d, cycles: 8, rom: { ; set 2, d }, regs: { d: 1 << 2, pc: 0x0002 } },
    { name: xcb_d3_set_2_e, cycles: 8, rom: { ; set 2, e }, regs: { e: 1 << 2, pc: 0x0002 } },
    { name: xcb_d4_set_2_h, cycles: 8, rom: { ; set 2, h }, regs: { h: 1 << 2, pc: 0x0002 } },
    { name: xcb_d5_set_2_l, cycles: 8, rom: { ; set 2, l }, regs: { l: 1 << 2, pc: 0x0002 } },
    { name: xcb_d6_set_2_indhl, cycles: 16, rom: { ; set 2, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcf)] },
    { name: xcb_d7_set_2_a, cycles: 8, rom: { ; set 2, a }, regs: { a: 1 << 2, pc: 0x0002 } },
    // 0xcb SET 3
    { name: xcb_d8_set_3_b, cycles: 8, rom: { ; set 3, b }, regs: { b: 1 << 3, pc: 0x0002 } },
    { name: xcb_d9_set_3_c, cycles: 8, rom: { ; set 3, c }, regs: { c: 1 << 3, pc: 0x0002 } },
    { name: xcb_da_set_3_d, cycles: 8, rom: { ; set 3, d }, regs: { d: 1 << 3, pc: 0x0002 } },
    { name: xcb_db_set_3_e, cycles: 8, rom: { ; set 3, e }, regs: { e: 1 << 3, pc: 0x0002 } },
    { name: xcb_dc_set_3_h, cycles: 8, rom: { ; set 3, h }, regs: { h: 1 << 3, pc: 0x0002 } },
    { name: xcb_dd_set_3_l, cycles: 8, rom: { ; set 3, l }, regs: { l: 1 << 3, pc: 0x0002 } },
    { name: xcb_de_set_3_indhl, cycles: 16, rom: { ; set 3, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_df_set_3_a, cycles: 8, rom: { ; set 3, a }, regs: { a: 1 << 3, pc: 0x0002 } },
    // 0xcb SET 4
    { name: xcb_e0_set_4_b, cycles: 8, rom: { ; set 4, b }, regs: { b: 1 << 4, pc: 0x0002 } },
    { name: xcb_e1_set_4_c, cycles: 8, rom: { ; set 4, c }, regs: { c: 1 << 4, pc: 0x0002 } },
    { name: xcb_e2_set_4_d, cycles: 8, rom: { ; set 4, d }, regs: { d: 1 << 4, pc: 0x0002 } },
    { name: xcb_e3_set_4_e, cycles: 8, rom: { ; set 4, e }, regs: { e: 1 << 4, pc: 0x0002 } },
    { name: xcb_e4_set_4_h, cycles: 8, rom: { ; set 4, h }, regs: { h: 1 << 4, pc: 0x0002 } },
    { name: xcb_e5_set_4_l, cycles: 8, rom: { ; set 4, l }, regs: { l: 1 << 4, pc: 0x0002 } },
    { name: xcb_e6_set_4_indhl, cycles: 16, rom: { ; set 4, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xdb)] },
    { name: xcb_e7_set_4_a, cycles: 8, rom: { ; set 4, a }, regs: { a: 1 << 4, pc: 0x0002 } },
    // 0xcb SET 5
    { name: xcb_e8_set_5_b, cycles: 8, rom: { ; set 5, b }, regs: { b: 1 << 5, pc: 0x0002 } },
    { name: xcb_e9_set_5_c, cycles: 8, rom: { ; set 5, c }, regs: { c: 1 << 5, pc: 0x0002 } },
    { name: xcb_ea_set_5_d, cycles: 8, rom: { ; set 5, d }, regs: { d: 1 << 5, pc: 0x0002 } },
    { name: xcb_eb_set_5_e, cycles: 8, rom: { ; set 5, e }, regs: { e: 1 << 5, pc: 0x0002 } },
    { name: xcb_ec_set_5_h, cycles: 8, rom: { ; set 5, h }, regs: { h: 1 << 5, pc: 0x0002 } },
    { name: xcb_ed_set_5_l, cycles: 8, rom: { ; set 5, l }, regs: { l: 1 << 5, pc: 0x0002 } },
    { name: xcb_ee_set_5_indhl, cycles: 16, rom: { ; set 5, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xeb)] },
    { name: xcb_ef_set_5_a, cycles: 8, rom: { ; set 5, a }, regs: { a: 1 << 5, pc: 0x0002 } },
    // 0xcb SET 6
    { name: xcb_f0_set_6_b, cycles: 8, rom: { ; set 6, b }, regs: { b: 1 << 6, pc: 0x0002 } },
    { name: xcb_f1_set_6_c, cycles: 8, rom: { ; set 6, c }, regs: { c: 1 << 6, pc: 0x0002 } },
    { name: xcb_f2_set_6_d, cycles: 8, rom: { ; set 6, d }, regs: { d: 1 << 6, pc: 0x0002 } },
    { name: xcb_f3_set_6_e, cycles: 8, rom: { ; set 6, e }, regs: { e: 1 << 6, pc: 0x0002 } },
    { name: xcb_f4_set_6_h, cycles: 8, rom: { ; set 6, h }, regs: { h: 1 << 6, pc: 0x0002 } },
    { name: xcb_f5_set_6_l, cycles: 8, rom: { ; set 6, l }, regs: { l: 1 << 6, pc: 0x0002 } },
    { name: xcb_f6_set_6_indhl, cycles: 16, rom: { ; set 6, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_f7_set_6_a, cycles: 8, rom: { ; set 6, a }, regs: { a: 1 << 6, pc: 0x0002 } },
    // 0xcb SET 7
    { name: xcb_f8_set_7_b, cycles: 8, rom: { ; set 7, b }, regs: { b: 1 << 7, pc: 0x0002 } },
    { name: xcb_f9_set_7_c, cycles: 8, rom: { ; set 7, c }, regs: { c: 1 << 7, pc: 0x0002 } },
    { name: xcb_fa_set_7_d, cycles: 8, rom: { ; set 7, d }, regs: { d: 1 << 7, pc: 0x0002 } },
    { name: xcb_fb_set_7_e, cycles: 8, rom: { ; set 7, e }, regs: { e: 1 << 7, pc: 0x0002 } },
    { name: xcb_fc_set_7_h, cycles: 8, rom: { ; set 7, h }, regs: { h: 1 << 7, pc: 0x0002 } },
    { name: xcb_fd_set_7_l, cycles: 8, rom: { ; set 7, l }, regs: { l: 1 << 7, pc: 0x0002 } },
    { name: xcb_fe_set_7_indhl, cycles: 16, rom: { ; set 7, (hl) }, regs: { pc: 0x0002 }, writes: [(0x0000, 0xcb)] },
    { name: xcb_ff_set_7_a, cycles: 8, rom: { ; set 7, a }, regs: { a: 1 << 7, pc: 0x0002 } },
}
