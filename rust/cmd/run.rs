use std::fs::File;
use std::num::ParseIntError;
use std::path::PathBuf;
use std::str::FromStr;

use rand::prelude::Distribution;
use structopt::StructOpt;
use rand::SeedableRng;

use crate::mem::Mem;
use crate::rtl::{self, Peripherals};

use super::SharedArgs;

#[derive(Debug)]
struct Addr(u64);
impl FromStr for Addr {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("0x") {
            return Ok(Self(u64::from_str_radix(&s[2..], 16)?));
        } else {
            return Ok(Self(s.parse()?));
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(
    setting = structopt::clap::AppSettings::TrailingVarArg,
    setting = structopt::clap::AppSettings::AllowLeadingHyphen,
)]
pub struct RunArgs {
    /// The memory file used to initialize the memory. If not provided, then the memory will be initialized to zero
    #[structopt(short, long)]
    mem: Option<PathBuf>,

    /// Base address of the provided memory file
    #[structopt(long, default_value = "0x80000000")]
    mem_base: Addr,

    /// Enable Spike-style tohost/fromhost interface. If true, timeout or isa testcase failure will result in an non-zero exit code.
    #[structopt(long, short)]
    spike: bool,

    /// Spike-style interface base address
    #[structopt(long, default_value = "0x80001000")]
    spike_base: Addr,

    /// Tracing waveform
    #[structopt(short, long)]
    trace: Option<PathBuf>,

    /// Extra argument passed directly to verilator
    extra: Vec<String>,
}

impl RunArgs {
    pub fn run(self, shared: SharedArgs) -> anyhow::Result<()> {
        log::debug!("Creating mem...");
        let mut mem = Mem::default();

        if let Some(mem_file) = self.mem {
            let f = File::open(mem_file)?;
            mem.init_with(f, self.mem_base.0)?;
        }

        log::debug!("Creating peripherals...");
        let spike = if self.spike { Some(self.spike_base.0) } else { None };
        let peripherals = Peripherals::new(mem, spike);

        log::debug!("Creating system...");
        let mut delay_rng = rand_pcg::Pcg64::seed_from_u64(shared.bus_delay_seed);
        let delay_dist = rand::distributions::Uniform::new(shared.bus_delay_lb, shared.bus_delay_ub);
        let delay_randomizer = move || -> u64 {
            delay_dist.sample(&mut delay_rng) as u64
        };
        let mut cpu = rtl::System::new(&self.extra, &self.trace, peripherals, delay_randomizer, shared.mem_burst_interval);
        cpu.set_rst(true);

        let mut isa_passed = false;

        for cycle in 0..shared.cycles {
            if cycle == shared.reset_for {
                cpu.set_rst(false);
            }

            cpu.tick()?;
            let isa_result = cpu.result();

            if isa_result == Some(true) {
                isa_passed = true;
            }

            if isa_result.is_some() {
                log::info!("Terminated after cycle: {}", cycle);
                break;
            }
        }

        log::debug!("Simulation done");

        if self.spike && !isa_passed {
            Err(anyhow::anyhow!("ISA test failed"))
        } else {
            Ok(())
        }
    }
}
