use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, BatchSize};

use gb_color_emu::{cart::Cart, system::GameBoyColor};

fn one_thousand_cycles(cart: Cart) -> GameBoyColor {
    let mut system = GameBoyColor::new(cart);
    for _ in 0..1_000 {
        system.step_cycle();
    }
    system
}

fn bench_1k_cycles(c: &mut Criterion) {
    let mut group = c.benchmark_group("1_000 cycles");
    const CASES: &[(u8, &str)] = &[
        (0x00, "NOP"),
        (0x3c, "INC A"),
        (0x3e, "LD A, n"),
        (0x22, "LD (nn), HL"),
    ];
    for (opcode, label) in CASES.iter().copied() {
        let rom = vec![opcode; 10_000];
        let cart = Cart::new_no_mbc(&rom);
        group.bench_function(BenchmarkId::from_parameter(label), |b| {
            let setup = || Cart::new_no_mbc(rom.clone());
            let routine = |cart| one_thousand_cycles(cart);
            b.iter_batched(setup, routine, BatchSize::SmallInput);
        });
    }
}

criterion_group!(benches, bench_1k_cycles);
criterion_main!(benches);
