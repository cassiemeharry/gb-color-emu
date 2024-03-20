use futures::prelude::{*, future::BoxFuture};
use pin_project::pin_project;
use tokio::sync::{RwLock, watch::Receiver};
use std::sync::Arc;

use super::{Rgb, ScreenBuffer};

// #[pin_project]
// #[derive(Clone, Debug)]
// pub struct Vp8Stream {
//     #[pin]
//     yuv: Vec<u8>,
//     buffer: Arc<RwLock<ScreenBuffer>>,
//     update_channel: Arc<Receiver<i64>>,
//     // Safety: the BoxFuture isn't actually 'static, as it contains references
//     // to this containing struct. 
//     inner: Option<BoxFuture<'static, Option<vpx_encode::Frame<'static>>>>,
// }

// impl Vp8Stream {
//     pub fn new() -> Self {
//         Self {
//             yuv: Vec::with_capacity(1024 * 4),
//             buffer,
//             update_channel: Arc::new(update_channel),
//             inner: None,
//         }
//     }

//     fn get_next(yuv: Pin<&mut Vec<u8>>, )
// }

// impl Stream for Vp8Stream {
//     type Item = vpx_encode::Frame<'static>;

//     fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
//         let this = self.project();
//         if this.inner.is_none() {
//             this.inner = Some(Box::pin(Self::get_next(&mut this.yuv, self.buffer.clone(), self.update_channel.clone())));
//         }
//         todo!()
//         // match this.inner.as_mut() {
//         // }
//     }
// }

struct Vp8StreamState {
    buffer: Arc<RwLock<ScreenBuffer>>,
    encoder: vpx_encode::Encoder,
    update_channel: Receiver<i64>,
    yuv: Vec<u8>,
}

impl Vp8StreamState {
    fn new(buffer: Arc<RwLock<ScreenBuffer>>, update_channel: Receiver<i64>) -> Self {
        let config = vpx_encode::Config {
            width: super::SCREEN_WIDTH as u32,
            height: super::SCREEN_HEIGHT as u32,
            timebase: [1, 30],
            bitrate: 1_000,
            codec: vpx_encode::VideoCodecId::VP8,
        };
        let encoder = vpx_encode::Encoder::new(config).unwrap();
        let mut yuv = Vec::with_capacity(1024);
        rgb_to_yuv(&mut yuv, &[Rgb::BLACK; super::SCREEN_WIDTH * super::SCREEN_HEIGHT]);
        Self {
            buffer,
            encoder,
            update_channel,
            yuv,
        }
    }
}

pub struct Frame {
    pub data: Box<[u8]>,
    pub key: bool,
    pub pts: i64,
}

async fn unfold_vp8_stream(mut state: Vp8StreamState) -> Option<(Vec<Frame>, Vp8StreamState)> {
    if let Err(_) = state.update_channel.changed().await {
        return None;
    }
    let frame_number: i64 = {
        let fn_ref = state.update_channel.borrow_and_update();
        *fn_ref
    };
    {
        let buffer = state.buffer.read().await;
        rgb_to_yuv(&mut state.yuv, buffer.pixels());
    }
    let mut output_frames = vec![];
    let frames = match state.encoder.encode(frame_number, &state.yuv) {
        Ok(frames) => frames,
        Err(e) => {
            error!("Failed to encode screen as VP8: {}", e);
            return None;
        }
    };
    for frame in frames {
        let f = Frame {
            data: Vec::from(frame.data).into_boxed_slice(),
            key: frame.key,
            pts: frame.pts,
        };
        output_frames.push(f);
    }
    Some((output_frames, state))
}

pub fn vp8_stream(buffer: Arc<RwLock<ScreenBuffer>>, update_channel: Receiver<i64>) -> impl Stream<Item = Vec<Frame>> {
    let state = Vp8StreamState::new(buffer, update_channel);
    futures::stream::unfold(state, unfold_vp8_stream)
}

fn rgb_to_yuv(yuv: &mut Vec<u8>, rgb: &[Rgb]) {
    yuv.clear();

    const HEIGHT: usize = super::SCREEN_HEIGHT;
    const WIDTH: usize = super::SCREEN_WIDTH;
    let stride = rgb.len() / HEIGHT;

    #[inline]
    fn factor(pixel: Rgb, r: i32, g: i32, b: i32, x: i32) -> u8 {
        let r = r * pixel.r as i32;
        let g = g * pixel.g as i32;
        let b = b * pixel.b as i32;
        let sum = r + g + b + 128;
        (sum / 256 + x).max(0).min(255) as u8
    }

    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            let i = y * stride + x;
            let pixel = rgb[i];
            let y = factor(pixel, 66, 129, 25, 16);
            yuv.push(y.min(255).max(0) as u8);
        }
    }

    for y in (0..HEIGHT).step_by(2) {
        for x in (0..WIDTH).step_by(2) {
            let i = y * stride + x;
            let pixel = rgb[i];
            let u = factor(pixel, -38, -74, 112, 128);
            yuv.push(u);
        }
    }

    for y in (0..HEIGHT).step_by(2) {
        for x in (0..WIDTH).step_by(2) {
            let i = y * stride + x;
            let pixel = rgb[i];
            let v = factor(pixel, 112, -94, -18, 128);
            yuv.push(v);
        }
    }
}
