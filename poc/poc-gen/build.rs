fn main() {
    println!("cargo:rerun-if-changed=poc/");

    tonic_build::configure()
        .build_client(true)
        .build_server(true)
        .compile(&["poc/poc.proto"], &["poc"])
        .unwrap();
}
