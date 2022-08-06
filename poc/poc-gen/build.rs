fn main() {
    println!("cargo:rerun-if-changed=../poc.proto");

    tonic_build::configure()
        .build_client(true)
        .build_server(true)
        .compile(&["../poc.proto"], &[".."])
        .unwrap();
}
