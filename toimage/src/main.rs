use cairo::{Context, FontSlant, FontWeight, ImageSurface, Pattern};
use core::ops::Range;
use std::{fs::File, io};
use thiserror::Error;
use visualizer::{
    layout::{layout, LayoutError, Rect, VisuallySized},
    Recipe, Step,
};

const COLOR_WHITE: u32 = 0xffffffff;
const COLOR_BLACK: u32 = 0xff000000;
const COLOR_GRAY: u32 = 0xffe2e3e9;
const COLOR_BLUE: u32 = 0xff2499ee;

const NODE_WIDTH: f64 = 150.0;
const SPACING: f64 = 20.0;
const MARGIN_PAGE: f64 = 10.0;
const MARGIN_BOX: f64 = 10.0;
const SPACING_TEXT: f64 = 5.0;

fn hex(ctx: &cairo::Context, argb: u32) -> Pattern {
    let a = 0xff & argb >> 24;
    let r = 0xff & argb >> 16;
    let g = 0xff & argb >> 8;
    let b = 0xff & argb;

    ctx.set_source_rgba(
        r as f64 / 255.0,
        g as f64 / 255.0,
        b as f64 / 255.0,
        a as f64 / 255.0,
    );

    ctx.source()
}

struct Colors {
    background: Pattern,
    box_: Pattern,
    text: Pattern,
    line: Pattern,
}

impl Colors {
    fn new(ctx: &cairo::Context) -> Self {
        Self {
            background: hex(ctx, COLOR_GRAY),
            box_: hex(ctx, COLOR_WHITE),
            text: hex(ctx, COLOR_BLACK),
            line: hex(ctx, COLOR_BLUE),
        }
    }
}

fn font_set_title(ctx: &cairo::Context) {
    ctx.select_font_face("sans-serif", FontSlant::Normal, FontWeight::Bold);
    ctx.set_font_size(20.0);
}

fn font_set_note(ctx: &cairo::Context) {
    ctx.select_font_face("sans-serif", FontSlant::Italic, FontWeight::Normal);
    ctx.set_font_size(12.0);
}

fn font_set_ingredients(ctx: &cairo::Context) {
    ctx.select_font_face("sans-serif", FontSlant::Normal, FontWeight::Normal);
    ctx.set_font_size(12.0);
}

fn font_set_utensils(ctx: &cairo::Context) {
    ctx.select_font_face("sans-serif", FontSlant::Normal, FontWeight::Normal);
    ctx.set_font_size(12.0);
}

fn break_text_mono(max_width: usize, text: &str) -> Vec<Range<usize>> {
    let mut res: Vec<Range<usize>> = Default::default();
    let mut line: Range<usize> = 0..0;
    let mut word_start: Option<usize> = None;
    let mut x = 0;
    for (i, c) in text.chars().enumerate() {
        if c == '\n' || x == max_width {
            res.push(line);
            if let Some(ws) = word_start {
                x = i - ws;
                line = ws..ws;
            } else {
                x = 0;
                line = i..i;
            }
            if c == '\n' {
                line.start = i + 1;
                line.end = i + 1;
                continue;
            }
        }
        if c.is_whitespace() {
            if word_start.is_some() {
                line.end = i;
                word_start = None;
            }
        } else if word_start.is_none() {
            word_start = Some(i);
        }
        x += 1;
    }
    if word_start.is_some() {
        line.end = text.len();
    }
    if !line.is_empty() {
        res.push(line);
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn break_text_mono_few_words() {
        let text = "Mix items";
        let result = break_text_mono(24, text);
        println!(
            "{:?}",
            result
                .iter()
                .map(|range| &text[range.clone()])
                .collect::<Vec<_>>()
        );
        assert_eq!(result, vec![0..9]);
    }

    #[test]
    fn break_text_mono_many_words() {
        let text = "butter, milk, flour, sugar, eggs, chocolate chips, etc.";
        let result = break_text_mono(24, text);
        println!(
            "{:?}",
            result
                .iter()
                .map(|range| &text[range.clone()])
                .collect::<Vec<_>>()
        );
        assert_eq!(result, vec![0..20, 21..43, 44..55])
    }
}

fn draw_text(
    ctx: &cairo::Context,
    (text, lines): &(String, Vec<Range<usize>>),
) -> Result<(), cairo::Error> {
    for line in lines.iter().cloned() {
        ctx.show_text(&text[line])?;
    }
    Ok(())
}

struct SizedStep {
    width: f64,
    height_time: f64,
    height_text: f64,
    text_title: (String, Vec<Range<usize>>),
    text_note: Option<(String, Vec<Range<usize>>)>,
    text_ingredients: Option<(String, Vec<Range<usize>>)>,
    text_utensils: Option<(String, Vec<Range<usize>>)>,
}

impl SizedStep {
    fn new(step: Step, width: f64) -> Result<Self, ()> {
        let height_time = step.time_s as f64;
        let mut height_text = 0.0;

        let tw = width as usize / 5;
        let lines = break_text_mono(tw, &step.title);
        height_text += 20.0 * lines.len() as f64;
        let text_title = (step.title, lines);
        let text_note = step.note.map(|note| {
            let lines = break_text_mono(tw, &note);
            height_text += 12.0 * lines.len() as f64;
            (note, lines)
        });
        let ingredients = step.ingredients.join(", ");
        let text_ingredients = if !ingredients.is_empty() {
            let lines = break_text_mono(tw, &ingredients);
            height_text += 12.0 * lines.len() as f64;
            Some((ingredients, lines))
        } else {
            None
        };
        let utensils = step.utensils.join(", ");
        let text_utensils = if !utensils.is_empty() {
            let lines = break_text_mono(tw, &utensils);
            height_text += 12.0 * lines.len() as f64;
            Some((utensils, lines))
        } else {
            None
        };

        if text_ingredients.is_some() || text_utensils.is_some() {
            height_text += SPACING_TEXT;
        }

        Ok(Self {
            width,
            height_time,
            height_text,
            text_title,
            text_note,
            text_ingredients,
            text_utensils,
        })
    }
}

impl VisuallySized<f64> for SizedStep {
    fn get_width(&self) -> f64 {
        self.width
    }

    fn get_height(&self) -> f64 {
        // self.height_text
        self.height_time
    }
}

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    Layout(#[from] LayoutError),
    #[error(transparent)]
    Cairo(#[from] cairo::Error),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    RonDe(#[from] ron::error::SpannedError),
}

impl From<cairo::IoError> for MainError {
    fn from(error: cairo::IoError) -> Self {
        match error {
            cairo::IoError::Cairo(error) => MainError::Cairo(error),
            cairo::IoError::Io(error) => MainError::Io(error),
        }
    }
}

fn main() -> Result<(), MainError> {
    let file = File::open("recipe_toscana.ron")?;
    // let file = File::open("recipe_single.ron")?;

    let recipe: Recipe = ron::de::from_reader(&file)?;

    let edges = recipe.edges();
    let nodes = recipe
        .steps
        .into_iter()
        .map(|step| SizedStep::new(step, NODE_WIDTH))
        .collect::<Result<Vec<_>, ()>>()
        .unwrap();

    // 00
    //  |
    // 01
    //  |
    // 02 04
    //  |  |
    // 03 05
    //  |/
    // 06 08
    //  |/
    // 09 07
    //  |/
    // 10 11
    //  |/
    // 12

    let paint_surface = ImageSurface::create(cairo::Format::Rgb24, 1, 1)?;
    let paint_ctx = Context::new(&paint_surface)?;
    let colors = Colors::new(&paint_ctx);

    let rects = layout(&nodes, &edges, SPACING)?;
    let bounds = Rect::bounded(&rects).expect("`rects` should contain more than one element");
    let nodes = nodes.into_iter().zip(rects).collect::<Vec<_>>();
    let surface = ImageSurface::create(
        cairo::Format::Rgb24,
        (bounds.width + MARGIN_PAGE * 2.0).ceil() as i32,
        (bounds.height + MARGIN_PAGE * 2.0).ceil() as i32,
    )?;
    let ctx = Context::new(&surface)?;
    ctx.translate(MARGIN_PAGE, MARGIN_PAGE);

    // draw background
    ctx.set_source(colors.background)?;
    ctx.paint()?;

    // draw boxes ///////////
    ctx.set_source(colors.box_)?;
    for (step, rect) in nodes.iter() {
        ctx.rectangle(rect.x, rect.y, rect.width, step.height_text);
        ctx.fill()?;
    }

    // draw lines ///////////
    ctx.set_source(colors.line)?;
    for (_step, rect) in nodes.iter() {
        ctx.move_to(rect.right(), rect.top());
        ctx.line_to(rect.right(), rect.bottom());
        ctx.stroke()?;
    }

    for (from, to) in edges.iter().copied() {
        let from_rect = &nodes[from].1;
        let to_rect = &nodes[to].1;
        ctx.move_to(from_rect.right(), from_rect.bottom());
        ctx.curve_to(
            from_rect.right(),
            from_rect.bottom() - 100.0,
            to_rect.right(),
            to_rect.bottom() + 50.0,
            to_rect.right(),
            to_rect.bottom(),
        );
        ctx.stroke()?;
    }

    // draw text ////////////
    ctx.set_source(colors.text)?;
    for (step, rect) in nodes.iter() {
        let mut y = rect.y;

        font_set_title(&ctx);
        y += 20.0;
        ctx.move_to(rect.x, y);
        draw_text(&ctx, &step.text_title)?;

        if let Some(text) = &step.text_note {
            font_set_note(&ctx);
            y += 12.0;
            ctx.move_to(rect.x, y);
            draw_text(&ctx, text)?;
        }

        y += SPACING_TEXT;

        if let Some(text) = &step.text_ingredients {
            font_set_ingredients(&ctx);
            y += 12.0;
            ctx.move_to(rect.x, y);
            draw_text(&ctx, text)?;
        }

        if let Some(text) = &step.text_utensils {
            font_set_utensils(&ctx);
            y += 12.0;
            ctx.move_to(rect.x, y);
            draw_text(&ctx, text)?;
        }
    }

    let mut file = File::create("output.png")?;
    surface.write_to_png(&mut file)?;

    Ok(())
}
