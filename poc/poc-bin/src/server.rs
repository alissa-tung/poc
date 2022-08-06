#![feature(box_syntax)]

use std::collections::HashMap;
use std::env;
use std::pin::Pin;

use log::warn;
use poc_gen::o_msg::i_msg::UnionMsg;
use poc_gen::o_msg::IMsg;
use poc_gen::poc_service_server::{PocService, PocServiceServer};
use poc_gen::{OMsg, PocMsg};
use tokio::sync::mpsc;
use tokio_stream::wrappers::{ReceiverStream, UnboundedReceiverStream};
use tokio_stream::{Stream, StreamExt};
use tonic::{Request, Response, Status, Streaming};

struct ServerContext {}

type ResponseStream = Pin<Box<dyn Stream<Item = Result<PocMsg, Status>> + Send>>;

type ResultResponse<A> = Result<Response<A>, Status>;

#[tonic::async_trait]
impl PocService for ServerContext {
    type BidiStreamingStream = ResponseStream;
    type ServerStreamingStream = ResponseStream;

    async fn poc_echo(&self, request: Request<OMsg>) -> ResultResponse<OMsg> {
        use UnionMsg::*;

        let request = request.into_inner();
        let xs = request
            .xs
            .into_iter()
            .map(|x| {
                let x = x.union_msg.map(|x| match x {
                    DoubleMsg(x) => DoubleMsg(-x),
                    BoolMsg(x) => BoolMsg(!x),
                    StringMsg(x) => StringMsg(x.chars().rev().collect::<String>()),
                    BytesMsg(x) => BytesMsg(x.into_iter().rev().collect::<Vec<_>>()),
                });
                IMsg { union_msg: x }
            })
            .collect::<Vec<IMsg>>();
        let ys = request
            .ys
            .into_iter()
            .map(|(k, v)| (format!("__{k}__"), v.chars().rev().collect::<String>()))
            .collect::<HashMap<_, _>>();
        let request = OMsg { xs, ys };
        Ok(Response::new(request))
    }

    async fn server_streaming(
        &self,
        request: Request<PocMsg>,
    ) -> ResultResponse<Self::ServerStreamingStream> {
        let buffer_size = 128;
        let (tx, rx) = mpsc::channel::<Result<PocMsg, Status>>(buffer_size);
        let response = ReceiverStream::new(rx);
        let response = Box::pin(response);

        let request = request.into_inner();
        tokio::spawn(async move {
            for _ in 0..buffer_size {
                tx.send(Result::<_, Status>::Ok(request.clone()))
                    .await
                    .map(|_| ())
                    .unwrap_or_else(|err| warn!("Send error: {err}"))
            }
        });

        Ok(Response::new(response as Self::ServerStreamingStream))
    }

    async fn client_streaming(
        &self,
        request: Request<Streaming<PocMsg>>,
    ) -> ResultResponse<PocMsg> {
        let message = request.into_inner().message().await?.ok_or_else(|| {
            Status::aborted("Failed to fetch the next message from client request")
        })?;
        Ok(Response::new(message))
    }

    async fn bidi_streaming(
        &self,
        request: Request<Streaming<PocMsg>>,
    ) -> ResultResponse<Self::BidiStreamingStream> {
        let (tx, rx) = mpsc::unbounded_channel::<Result<PocMsg, Status>>();
        let response = UnboundedReceiverStream::new(rx);
        let response = Box::pin(response);

        let mut request = request.into_inner();

        tokio::spawn(async move {
            while let Some(message) = request.next().await {
                tx.send(message)
                    .map(|_| ())
                    .unwrap_or_else(|err| warn!("Send error: {err}"))
            }
        });

        Ok(Response::new(response as Self::BidiStreamingStream))
    }
}

#[tokio::main]
async fn main() {
    let addr = env::var("RS_SERVER_ADDR").unwrap();
    let server = ServerContext {};
    tonic::transport::Server::builder()
        .add_service(PocServiceServer::new(server))
        .serve(addr.parse().unwrap())
        .await
        .unwrap();
}
