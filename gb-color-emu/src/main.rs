use gb_color_emu::{cart::Cart, system::GameBoyColor};

#[allow(unused)]
fn run_game() {
    // const GAME: &str = "roms/SpaceInvasion01.gb";
    const GAME: &str = "roms/geometrix.gbc";
    let rom = std::fs::read(GAME).unwrap();
    let cart = Cart::new(rom);
    let mut system = GameBoyColor::new(cart);

    const CYCLES: u32 = 100_000_000;
    println!("Running cart for 10M cycles...");
    let start = std::time::Instant::now();
    for _ in 0..CYCLES {
        system.step_cycle();
    }
    let elapsed = start.elapsed();
    println!("Finished with 10M cycles in {:?} ({:?} per cycle, target is 240ns)", elapsed, elapsed / CYCLES);
}

fn main() {
    pretty_env_logger::init();

    // let mut rng = rand::thread_rng();
    // let screen = gb_color_emu::screen::Screen::random(&mut rng);
    // let screen = gb_color_emu::screen::Screen::gradient();
    // println!("{}", screen.display_terminal());
    run_game();
}
