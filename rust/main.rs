mod cmd;
mod mem;
mod rtl;

use anyhow::Error;
use cmd::Args;

#[paw::main]
fn main(args: Args) -> Result<(), Error> {
    env_logger::init();
    args.run()
}
