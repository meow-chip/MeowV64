#[cxx::bridge(namespace = "meowv64::bridge")]
mod ffi {
    // TODO: handle memory write
    #[repr(u8)]
    #[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
    enum MemOp {
        Read = 0,
        Write = 1,
    }

    #[derive(Default, PartialEq, PartialOrd, Eq, Ord, Debug)]
    struct MemCmd {
        id: u64,
        addr: u64,
        op: MemOp,
        size: u8,
        burst: u8,
    }

    #[derive(Default, PartialEq, PartialOrd, Eq, Ord, Debug)]
    struct MemWrite {
        data: u64,
    }
 
    #[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
    struct MemResp {
        id: u64,
        data: u64,
    }

    unsafe extern "C++" {
        include!("bridge.h");

        type CPU;
        pub fn set_int(self: Pin<&mut CPU>, n: usize, set: bool);
        pub fn set_rst(self: Pin<&mut CPU>, rst: bool);
        pub fn tick(self: Pin<&mut CPU>) -> bool;

        pub fn mem_cmd_accept(self: Pin<&mut CPU>, dest: &mut MemCmd) -> bool;
        pub fn mem_write_accept(self: Pin<&mut CPU>, dest: &mut MemWrite) -> bool;
        pub fn mem_resp_enqueue(self: Pin<&mut CPU>, data: &MemResp) -> bool;

        pub fn init(args: &Vec<String>, trace: &str) -> UniquePtr<CPU>;
    }
}

impl Default for ffi::MemOp {
    fn default() -> Self {
        Self::Read
    }
}

use std::{path::PathBuf, collections::BinaryHeap, cmp::Reverse};

use cxx::UniquePtr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum DownlinkEv {
    Cmd(ffi::MemCmd),
    Write(ffi::MemWrite),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct PendingEv<E> {
    scheduled_at: Reverse<u64>,
    enqueued_at: Reverse<u64>, // As secondary key
    ev: E,
}

use crate::mem::Mem;
pub struct System<F: FnMut() -> u64> {
    backend: UniquePtr<ffi::CPU>,
    peripherals: Peripherals,

    tick: u64,

    downlink: BinaryHeap<PendingEv<DownlinkEv>>,
    uplink: BinaryHeap<PendingEv<ffi::MemResp>>,

    delay_randomizer: F,
    mem_burst_interval: u64,
}
impl<F: FnMut() -> u64> System<F> {
    pub fn new(
        extra: &Vec<String>,
        trace: &Option<PathBuf>,
        peripherals: Peripherals,
        delay_randomizer: F,
        mem_burst_interval: u64,
    ) -> Self {
        let backend = ffi::init(
            &extra,
            trace
                .as_ref()
                .and_then(|p| p.as_os_str().to_str())
                .unwrap_or(""),
        );

        Self {
            backend,
            peripherals,

            tick: 0,
            delay_randomizer,
            mem_burst_interval,

            uplink: Default::default(),
            downlink: Default::default(),
        }
    }

    pub fn tick(&mut self) -> anyhow::Result<()> {
        if let Some(r) = self.uplink.peek() {
            if r.scheduled_at.0 <= self.tick {
                log::debug!("[{}] Enqueue mem resp: {:?}", self.tick, r.ev);
                let enqueued = self.backend.as_mut().unwrap().mem_resp_enqueue(&r.ev);
                if enqueued {
                    self.uplink.pop();
                }
            }
        }

        let finished = self.backend.as_mut().unwrap().tick();
        if finished {
            return Err(anyhow::anyhow!("Unexpected finish from HDL"));
        }
        self.tick += 1;

        // Fetch datas
        let mut cmd = ffi::MemCmd::default();
        let mut write = ffi::MemWrite::default();
        if self.backend.as_mut().unwrap().mem_cmd_accept(&mut cmd) {
            log::debug!("[{}] Got cmd: {:?}", self.tick, cmd);
            self.downlink.push(PendingEv {
                scheduled_at: Reverse(self.tick + (self.delay_randomizer)()),
                enqueued_at: Reverse(self.tick),
                ev: DownlinkEv::Cmd(cmd),
            })
        }
        if self.backend.as_mut().unwrap().mem_write_accept(&mut write) {
            log::debug!("[{}] Got write: {:?}", self.tick, write);
            self.downlink.push(PendingEv {
                scheduled_at: Reverse(self.tick + (self.delay_randomizer)()),
                enqueued_at: Reverse(self.tick),
                ev: DownlinkEv::Write(write),
            })
        }

        // Handles events. Also handle at most one event per cycle
        if let Some(e) = self.downlink.peek() {
            if e.scheduled_at.0 <= self.tick {
                log::debug!("[{}] Processing: {:?}", self.tick, e);
                match e.ev {
                    DownlinkEv::Cmd(ref c) => {
                        match c.op {
                            ffi::MemOp::Read => {
                                let mut buf = Vec::new();
                                self.peripherals.read(c.addr, c.size, c.burst, &mut buf)?;
                                let burst_arrival = self.tick + (self.delay_randomizer)();
                                for (idx, data) in buf.into_iter().enumerate() {
                                    let scheduled_at = burst_arrival + idx as u64 * self.mem_burst_interval as u64;
                                    self.uplink.push(PendingEv {
                                        scheduled_at: Reverse(scheduled_at),
                                        enqueued_at: Reverse(self.tick),
                                        ev: ffi::MemResp {
                                            id: c.id,
                                            data,
                                        }
                                    })
                                }
                            },
                            ffi::MemOp::Write => unimplemented!("Mem write not implemented"),
                            o => return Err(anyhow::anyhow!("Unexpected mem op {}", o.repr)),
                        }
                    },
                    DownlinkEv::Write(_) => unimplemented!("Mem write not implemented"),
                }

                self.downlink.pop();
            }
        }

        Ok(())
    }

    pub fn set_rst(&mut self, rst: bool) {
        self.backend.as_mut().unwrap().set_rst(rst);
    }

    pub fn result(&self) -> Option<bool> {
        self.peripherals.result.clone()
    }
}

struct AddrGen {
    base: u64,
    size: u8,
    burst: u8,

    cnt: u8,
}

struct AddrPack {
    base: u64,
    size: u8,
    offset: u8,
}

impl AddrGen {
    fn new(base: u64, raw_size: u8, burst: u8) -> anyhow::Result<Self> {
        let size = 1 << raw_size;

        if base % size != 0 {
            return Err(anyhow::anyhow!("Base address 0x{:x} not aligned to size {} (raw {})", base, size, raw_size));
        }

        Ok(AddrGen {
            base,
            size: size as u8,
            burst,

            cnt: 0,
        })
    }
}

impl Iterator for AddrGen {
    type Item = AddrPack;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cnt == self.burst {
            return None;
        }

        // Align base addressg
        let alignment = self.burst.checked_next_power_of_two().map(u64::from).unwrap_or(256) * self.size as u64;
        let base_offset = self.base % alignment;
        let base_aligned = self.base - base_offset;

        let translated = base_aligned + (base_offset + self.cnt as u64 * self.size as u64) % alignment;

        // Align to 8-byte
        let translated_offset = translated % 8;
        let translated_base = translated - translated_offset;

        self.cnt += 1;

        Some(AddrPack {
            base: translated_base,
            size: self.size,
            offset: translated_offset as u8,
        })
    }
}

pub struct Peripherals {
    mem: Mem,
    spike: Option<u64>,
    result: Option<bool>,
}

impl Peripherals {
    pub fn new(mem: Mem, spike: Option<u64>) -> Self {
        Self {
            mem,
            spike,
            result: None,
        }
    }

    // Wrap-around burst reading
    pub fn read(&mut self, base: u64, raw_size: u8, burst: u8, buf: &mut Vec<u64>) -> anyhow::Result<()> {
        log::debug!("reading: 0x{:x} x {}", base, burst);
        for pack in AddrGen::new(base, raw_size, burst)? {
            let raw_readout = self.mem.mem.get(&pack.base).cloned().unwrap_or(0);
            // Bus is aligned, we can ignore shift and size
            // TODO: fill random data out side of current burst
            log::debug!("-> : 0x{:x}", raw_readout);
            buf.push(raw_readout);
        }
        Ok(())
    }

    pub fn write(&mut self, base: u64, size: u8, data: u64, be: u8) -> anyhow::Result<()> {
        if Some(base) == self.spike {
            // TODO: check be
            // tohost
            if data == 1 {
                log::info!("ISA test passes");
                self.result = Some(true);
            } else if (data & 1) == 1 {
                log::error!("ISA test failed case: {}", data >> 1);
                self.result = Some(false);
            } else {
                log::info!("tohost: {}", data);
            }
        } else {
            log::debug!("writing: 0x{:x} <- 0x{:x} / 0b{:b}", base, data, be);
            let orig = self.mem.mem.get(&base).cloned().unwrap_or(0);
            let mut buffer = orig.to_le_bytes();
            let writing = data.to_le_bytes();
            for i in 0..8 {
                if (be & (1 << i)) != 0 {
                    buffer[i] = writing[i];
                }
            }


            let flatten = u64::from_le_bytes(buffer);
            if flatten != orig {
                self.mem.mem.insert(base, flatten);
            }
        }

        Ok(())
    }
}