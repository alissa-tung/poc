use std::collections::HashMap;
use std::env;

use poc_gen::o_msg::i_msg::UnionMsg;
use poc_gen::o_msg::IMsg;
use poc_gen::poc_service_client::PocServiceClient;
use poc_gen::{OMsg, PocMsg};
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::StreamExt;

#[tokio::main]
async fn main() {
    let addr = env::var("RS_SERVER_ADDR").unwrap();
    let mut client = PocServiceClient::connect(format!("http://{addr}"))
        .await
        .unwrap();

    let n = 8;
    for i in 0..n {
        let request = gen_o_msg(n, i);
        println!("{:#?}", client.poc_echo(request).await.unwrap())
    }
    for i in 0..n {
        let request = gen_poc_msg(i);
        client
            .server_streaming(request)
            .await
            .into_iter()
            .take(n)
            .map(|x| println!("{:#?}", x))
            .for_each(drop);
    }

    let (tx, rx) = mpsc::channel(n);
    let request = ReceiverStream::new(rx);
    let response = client.client_streaming(request);
    tokio::spawn(async move {
        for i in 0..n {
            let request = gen_poc_msg(i);
            tx.send(request).await.unwrap();
        }
    });
    println!("{:#?}", response.await.unwrap());

    let (tx, rx) = mpsc::channel(n);
    let request = ReceiverStream::new(rx);
    let mut response = client.bidi_streaming(request).await.unwrap().into_inner();
    tokio::spawn(async move {
        for i in 0..n {
            let request = gen_poc_msg(i);
            tx.send(request).await.unwrap();
        }
    });
    while let Some(x) = response.next().await {
        println!("{:#?}", x.unwrap())
    }
}

fn gen_o_msg(n: usize, i: usize) -> OMsg {
    use UnionMsg::*;

    let xs = (0..n)
        .into_iter()
        .map(|x| IMsg {
            union_msg: Some(match i % 4 {
                0 => DoubleMsg((i + x) as f64),
                1 => BoolMsg(((i + x) % 2) == 0),
                2 => StringMsg(format!("{i} {x}")),
                3 => BytesMsg(format!("{i} {x}").into_bytes()),
                _ => panic!(),
            }),
        })
        .collect::<Vec<_>>();

    let ys = (0..n)
        .into_iter()
        .map(|x| (format!("{i} {x}"), format!("{n} {x}")))
        .collect::<HashMap<String, String>>();

    OMsg { xs, ys }
}

fn gen_poc_msg(i: usize) -> PocMsg {
    PocMsg {
        xs: format!("==== {i} ===="),
    }
}
