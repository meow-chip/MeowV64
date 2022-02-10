fn main() {
    let dst = cmake::build("bridge");
    println!(
        "cargo:rustc-link-search=native={}",
        dst.join("lib").display()
    );
    println!("cargo:rustc-link-lib=dylib=meowv64rtl");

    // Rerun on build.rs changes
    println!("cargo:rerun-if-changed=build.rs");

    // Rerun on bridge and bridge/* changes
    println!("cargo:rerun-if-changed=bridge");
    for entry in glob::glob("bridge/*").unwrap() {
        println!("cargo:rerun-if-changed={}", entry.unwrap().display());
    }

    // Rerun on Multicore.v
    println!("cargo:rerun-if-changed=Multicore.v");
}
