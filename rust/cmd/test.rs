use std::{sync::Arc, path::PathBuf, process::Command, str::FromStr, sync::Mutex, thread};

use anyhow::Error;
use indicatif::{MultiProgress, ProgressBar};

use super::SharedArgs;

use structopt::StructOpt;

enum RunnerEvent {
    Case(PathBuf),
    Done,
}

fn run_internal<I>(it: I, threads: usize, shared: SharedArgs) -> Result<(), Error>
where
    I: IntoIterator<Item = PathBuf>,
{
    let mprog = MultiProgress::new();
    let (tx, rx) = crossbeam_channel::bounded(0);
    let failed = Arc::new(Mutex::new(Vec::new()));
    let exe_path = std::env::current_exe()?;

    for t in 0..threads {
        let prog = mprog.add(ProgressBar::new_spinner());
        prog.enable_steady_tick(100);
        prog.set_prefix(format!("[{}/{}]", t+1, threads));

        let crx = rx.clone();
        let cexe = exe_path.clone();
        let cfailed = failed.clone();
        let cycles = shared.cycles.to_string();
        let reset_for = shared.reset_for.to_string();

        let _ = thread::spawn(move || {
            for ev in crx {
                let path = match ev {
                    RunnerEvent::Done => break,
                    RunnerEvent::Case(c) => c,
                };

                let args = vec![
                    "run",
                    "--spike",
                    "-m", path.to_str().unwrap(),
                    "--cycles", &cycles,
                    "--reset-for", &reset_for,
                ];

                prog.set_message(format!("Running: {}...", path.display()));
                let output = Command::new(&cexe).args(args).output().unwrap();
                if !output.status.success() {
                    cfailed.lock().unwrap().push(path);
                }
            }
            prog.finish_with_message("Waiting...");
        });
    }

    let disp_thread = thread::spawn(move || {
        mprog.join_and_clear().unwrap();
    });

    let mut cnt = 0;
    for path in it {
        tx.send(RunnerEvent::Case(path))?;
        cnt += 1;
    }

    for _ in 0..threads {
        tx.send(RunnerEvent::Done)?;
    }

    disp_thread.join().unwrap();

    let guard = failed.lock().unwrap();
    let failed_cnt = guard.len();
    if failed_cnt == 0 {
        log::info!("All {} tests passed!", cnt);
        Ok(())
    } else {
        log::error!("{}/{} tests failed:", failed_cnt, cnt);

        for f in guard.iter() {
            log::error!("  {}", f.display());
        }

        Err(anyhow::anyhow!("Test failed"))
    }
}

#[derive(Debug)]
struct Parallelism(usize);
impl FromStr for Parallelism {
    type Err = <usize as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        usize::from_str(s).map(Self)
    }
}

impl Default for Parallelism {
    fn default() -> Self {
        Self(num_cpus::get())
    }
}

impl ToString for Parallelism {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[derive(StructOpt, Debug)]
pub struct TestArgs {
    /// A list file as bare binary executable. If not present, mill will try to read from stdin and use that as the file list (one path per line)
    #[structopt(short, long)]
    files: Option<Vec<PathBuf>>,

    /// Job parallelism
    #[structopt(default_value, short, long)]
    jobs: Parallelism,
}

impl TestArgs {
    pub fn run(self, shared: SharedArgs) -> Result<(), Error> {
        match self.files {
            Some(vec) => run_internal(vec, self.jobs.0, shared),
            None => {
                use std::io::prelude::*;
                let it = std::io::stdin();
                let lines = it.lock().lines();
                let lines = lines.map(|l| PathBuf::from(l.unwrap()));
                run_internal(lines, self.jobs.0, shared)
            }
        }
    }
}
