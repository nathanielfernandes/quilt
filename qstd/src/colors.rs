use common::error::Error;
use interpreter::{
    builtins::*,
    generate_builtins,
    value::{make_value_array, Value},
    vm::*,
};

generate_builtins! {
    ///Color manipulation functions.
    [export=colors]

    ///Create a color from RGBA values.
    fn @rgba(r: u8, g: u8, b: u8, a: u8) -> color {
        Value::Color([r, g, b, a])
    }

    ///Create a color from RGB values.
    fn @rgb(r: u8, g: u8, b: u8) -> color {
        Value::Color([r, g, b, 255])
    }

    ///Create a color from HSLA values.
    fn @hsla(h: double, s: double, l: double, a: double) -> color {
        Value::Color(hsla_to_rgba(h, s, l, a))
    }

    ///Create a color from HSL values.
    fn @hsl(h: double, s: double, l: double) -> color {
        Value::Color(hsla_to_rgba(h, s, l, 1.0))
    }

    ///Create a color from HSVA values.
    fn @hsva(h: double, s: double, v: double, a: double) -> color {
        Value::Color(hsva_to_rgba(h, s, v, a))
    }

    ///Create a color from HSV values.
    fn @hsv(h: double, s: double, v: double) -> color {
        Value::Color(hsva_to_rgba(h, s, v, 1.0))
    }

    ///Get the Red value of a color.
    fn @r(c: color) -> int {
        Value::Int(c[0] as i64)
    }

    ///Get the Green value of a color.
    fn @g(c: color) -> int {
        Value::Int(c[1] as i64)
    }

    ///Get the Blue value of a color.
    fn @b(c: color) -> int {
        Value::Int(c[2] as i64)
    }

    ///Get the Alpha value of a color.
    fn @a(c: color) -> int {
        Value::Int(c[3] as i64)
    }

    ///Compute the luminance of a color.
    fn @luma(c: color) -> float {
        Value::Float(c[0] as f64 * 0.2126 + c[1] as f64  * 0.7152 + c[2] as f64  * 0.0722)
    }

    ///Compute the hue of a color.
    fn @hue(c: color) -> float {
        Value::Float(c[0] as f64 * 0.299 + c[1] as f64  * 0.587 + c[2] as f64  * 0.114)
    }

    ///Compute the saturation of a color.
    fn @saturation(c: color) -> float {
        Value::Float((c[0] as f64 - 0.5).abs() + (c[1] as f64 - 0.5).abs() + (c[2] as f64 - 0.5).abs())
    }

    ///Compute the brightness of a color.
    fn @brightness(c: color) -> float {
        Value::Float(c[0] as f64 * 0.299 + c[1] as f64  * 0.587 + c[2] as f64  * 0.114)
    }

    ///Compute the contrast of a color.
    fn @contrast(c: color) -> float {
        Value::Float((c[0] as f64 - 0.5).abs() + (c[1] as f64 - 0.5).abs() + (c[2] as f64 - 0.5).abs())
    }

    ///Compute the difference between two colors.
    fn @cdifference(a: color, b: color) -> float {
        Value::Float(((a[0] as f64 - b[0] as f64).abs() + (a[1] as f64 - b[1] as f64).abs() + (a[2] as f64 - b[2] as f64).abs()) / 3.0)
    }

    ///Invert a color.
    fn @invert(c: color) -> color {
        Value::Color([255 - c[0], 255 - c[1], 255 - c[2], c[3]])
    }

    ///Mix two colors.
    fn @mix(c1: color, c2: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c1[0] as f64 * (1.0 - t) + c2[0] as f64 * t).round() as u8;
        let g = (c1[1] as f64 * (1.0 - t) + c2[1] as f64 * t).round() as u8;
        let b = (c1[2] as f64 * (1.0 - t) + c2[2] as f64 * t).round() as u8;
        let a = (c1[3] as f64 * (1.0 - t) + c2[3] as f64 * t).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Lighten a color.
    fn @lighten(c: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c[0] as f64 * (1.0 - t) + 255.0 * t).round() as u8;
        let g = (c[1] as f64 * (1.0 - t) + 255.0 * t).round() as u8;
        let b = (c[2] as f64 * (1.0 - t) + 255.0 * t).round() as u8;
        let a = (c[3] as f64 * (1.0 - t) + 255.0 * t).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Darken a color.
    fn @darken(c: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c[0] as f64 * (1.0 - t)).round() as u8;
        let g = (c[1] as f64 * (1.0 - t)).round() as u8;
        let b = (c[2] as f64 * (1.0 - t)).round() as u8;
        let a = (c[3] as f64 * (1.0 - t)).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Saturate a color.
    fn @saturate(c: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c[0] as f64 * (1.0 - t) + c[0] as f64 * t).round() as u8;
        let g = (c[1] as f64 * (1.0 - t) + c[1] as f64 * t).round() as u8;
        let b = (c[2] as f64 * (1.0 - t) + c[2] as f64 * t).round() as u8;
        let a = (c[3] as f64 * (1.0 - t) + c[3] as f64 * t).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Desaturate a color.
    fn @desaturate(c: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c[0] as f64 * (1.0 - t) + c[0] as f64 * t).round() as u8;
        let g = (c[1] as f64 * (1.0 - t) + c[1] as f64 * t).round() as u8;
        let b = (c[2] as f64 * (1.0 - t) + c[2] as f64 * t).round() as u8;
        let a = (c[3] as f64 * (1.0 - t) + c[3] as f64 * t).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Fade a color.
    fn @fade(c: color, t: double) -> color {
        let t = t.max(0.0).min(1.0);

        let r = (c[0] as f64 * (1.0 - t)).round() as u8;
        let g = (c[1] as f64 * (1.0 - t)).round() as u8;
        let b = (c[2] as f64 * (1.0 - t)).round() as u8;
        let a = (c[3] as f64 * (1.0 - t)).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Get the complement of a color.
    fn @complement(c: color) -> color {
        let r = (c[0] as f64 * -1.0).round() as u8;
        let g = (c[1] as f64 * -1.0).round() as u8;
        let b = (c[2] as f64 * -1.0).round() as u8;
        let a = (c[3] as f64 * -1.0).round() as u8;

        Value::Color([r, g, b, a])
    }

    ///Get white or black depending on the luminance of a color.
    fn @contrast_color(c: color) -> color {
        if c[0] as f64 * 0.299 + c[1] as f64 * 0.587 + c[2] as f64 * 0.114 > 186.0 {
            Value::Color([0, 0, 0, c[3]])
        } else {
            Value::Color([255, 255, 255, c[3]])
        }
    }

    ///Random color.
    fn @random_color() -> color {
        let wide = rand::random::<u32>();
        let r = (wide & 0xFF) as u8;
        let g = ((wide >> 8) & 0xFF) as u8;
        let b = ((wide >> 16) & 0xFF) as u8;

        Value::Color([r, g, b, 255])
    }

    ///Random pastel color.
    fn @random_pastel_color() -> color {
        let wide = rand::random::<u32>();
        let r = ((wide & 0xFF) as f64 * 0.5 + 127.5) as u8;
        let g = (((wide >> 8) & 0xFF) as f64 * 0.5 + 127.5) as u8;
        let b = (((wide >> 16) & 0xFF) as f64 * 0.5 + 127.5) as u8;

        Value::Color([r, g, b, 255])
    }

    ///Random rainbow color.
    fn @random_rainbow_color() -> color {
        // random choice
        RAINBOW[rand::random::<usize>() % RAINBOW.len()].into()
    }

    [options]
    ///Get a list of 10 colors on the rainbow.
    fn @rainbow_colors() -> list {
        make_value_array(RAINBOW.iter().map(|c| Value::Color(*c)).collect::<Vec<Value>>(), options.array_max_size)?
    }
}

const RAINBOW: [[u8; 4]; 10] = [
    [0xFF, 0, 0, 255],
    [0xFF, 0x87, 0, 255],
    [0xFF, 0xD3, 0, 255],
    [0xDE, 0xFF, 0x0A, 255],
    [0xA1, 0xFF, 0x0A, 255],
    [0x0A, 0xFF, 0x99, 255],
    [0x0A, 0xEF, 0xFF, 255],
    [0x14, 0x7D, 0xF5, 255],
    [0x58, 0x0A, 0xFF, 255],
    [0xBE, 0x0A, 0xFF, 255],
];

#[inline]
fn hsla_to_rgba(h: f64, s: f64, l: f64, a: f64) -> [u8; 4] {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h_ = h / 60.0;
    let x = c * (1.0 - (h_ % 2.0 - 1.0).abs());
    let m = l - c / 2.0;

    let (r, g, b) = if h_ < 1.0 {
        (c, x, 0.0)
    } else if h_ < 2.0 {
        (x, c, 0.0)
    } else if h_ < 3.0 {
        (0.0, c, x)
    } else if h_ < 4.0 {
        (0.0, x, c)
    } else if h_ < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    [
        ((r + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((g + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((b + m).max(0.0).min(1.0) * 255.0).round() as u8,
        (a * 255.0).round() as u8,
    ]
}

#[inline]
fn hsva_to_rgba(h: f64, s: f64, v: f64, a: f64) -> [u8; 4] {
    let c = v * s;
    let h_ = h / 60.0;
    let x = c * (1.0 - (h_ % 2.0 - 1.0).abs());
    let m = v - c;

    let (r, g, b) = if h_ < 1.0 {
        (c, x, 0.0)
    } else if h_ < 2.0 {
        (x, c, 0.0)
    } else if h_ < 3.0 {
        (0.0, c, x)
    } else if h_ < 4.0 {
        (0.0, x, c)
    } else if h_ < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    [
        ((r + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((g + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((b + m).max(0.0).min(1.0) * 255.0).round() as u8,
        (a * 255.0).round() as u8,
    ]
}
