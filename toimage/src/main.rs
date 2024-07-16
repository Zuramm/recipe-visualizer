use cairo::{Context, ImageSurface, Pattern};
use pango::{ffi::PANGO_SCALE, AttrInt, AttrList, FontDescription};
use pangocairo::functions::{create_layout, show_layout};
use std::{f64, fs::File, io};
use thiserror::Error;
use visualizer::{
    layout::{layout, LayoutError, Rect, Timed, VisuallySized},
    Recipe, Step,
};

const COLOR_WHITE: u32 = 0xffffffff;
const COLOR_BLACK: u32 = 0xff000000;
const COLOR_GRAY: u32 = 0xffe2e3e9;
const COLOR_BLUE: u32 = 0xff2499ee;

const NODE_WIDTH: f64 = 150.0;
const SPACING: f64 = 20.0;
const MARGIN_PAGE: f64 = 40.0;
const MARGIN_BOX: f64 = 10.0;
const SPACING_TEXT: f64 = 5.0;

const FONT_SIZE_TITLE: f64 = 20.0;
const FONT_SIZE_NOTE: f64 = 12.0;
const FONT_SIZE_INGREDIENTS: f64 = 12.0;
const FONT_SIZE_UTENSILS: f64 = 12.0;

const LINE_HEIGHT_TITLE: f64 = 24.0;
const LINE_HEIGHT_NOTE: f64 = 14.0;
const LINE_HEIGHT_INGREDIENTS: f64 = 14.0;
const LINE_HEIGHT_UTENSILS: f64 = 14.0;

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

struct Fonts {
    title: FontDescription,
    note: FontDescription,
    ingredients: FontDescription,
    utensils: FontDescription,
}

impl Fonts {
    fn new() -> Self {
        // NOTE: Font size is the distance between the baseline and the ascender line. In other
        // words the height of capital letters like I.
        let mut title = FontDescription::new();
        title.set_family("Inria Serif");
        title.set_absolute_size(FONT_SIZE_TITLE * PANGO_SCALE as f64);
        title.set_weight(pango::Weight::Bold);

        let mut note = FontDescription::new();
        note.set_family("Inria Serif");
        note.set_absolute_size(FONT_SIZE_NOTE * PANGO_SCALE as f64);
        note.set_style(pango::Style::Italic);

        let mut ingredients = FontDescription::new();
        ingredients.set_family("Inria Serif");
        ingredients.set_absolute_size(FONT_SIZE_INGREDIENTS * PANGO_SCALE as f64);

        let mut utensils = FontDescription::new();
        utensils.set_family("Inria Serif");
        utensils.set_absolute_size(FONT_SIZE_UTENSILS * PANGO_SCALE as f64);

        Self {
            title,
            note,
            ingredients,
            utensils,
        }
    }
}

struct SizedStep {
    width: f64,
    height_time: f64,
    height_text: f64,
    text_title: pango::Layout,
    text_note: Option<pango::Layout>,
    text_ingredients: Option<pango::Layout>,
    text_utensils: Option<pango::Layout>,
}

impl SizedStep {
    fn new(step: Step, width: f64, ctx: &cairo::Context, fonts: &Fonts) -> Result<Self, ()> {
        let height_time = step.time_s as f64;
        let mut height_text = MARGIN_BOX * 2.0;

        let tw = ((width - MARGIN_BOX * 2.0) * PANGO_SCALE as f64) as i32;

        let attrs = AttrList::new();
        attrs.insert(AttrInt::new_line_height_absolute(
            (LINE_HEIGHT_TITLE * PANGO_SCALE as f64) as i32,
        ));
        let text_title = create_layout(ctx);
        text_title.set_width(tw);
        text_title.set_font_description(Some(&fonts.title));
        text_title.set_attributes(Some(&attrs));
        text_title.set_text(&step.title);
        let (_, lh) = text_title.size();
        height_text += lh as f64 / PANGO_SCALE as f64;

        let text_note = step.note.map(|note| {
            let attrs = AttrList::new();
            attrs.insert(AttrInt::new_line_height_absolute(
                (LINE_HEIGHT_NOTE * PANGO_SCALE as f64) as i32,
            ));
            let layout = create_layout(ctx);
            layout.set_width(tw);
            layout.set_font_description(Some(&fonts.note));
            layout.set_attributes(Some(&attrs));
            layout.set_text(&note);
            let (_, lh) = layout.size();
            height_text += lh as f64 / PANGO_SCALE as f64;
            layout
        });

        let ingredients = step.ingredients.join(", ");
        let text_ingredients = if !step.ingredients.is_empty() {
            let attrs = AttrList::new();
            attrs.insert(AttrInt::new_line_height_absolute(
                (LINE_HEIGHT_INGREDIENTS * PANGO_SCALE as f64) as i32,
            ));
            let layout = create_layout(ctx);
            layout.set_width(tw);
            layout.set_font_description(Some(&fonts.ingredients));
            layout.set_attributes(Some(&attrs));
            layout.set_text(&ingredients);
            let (_, lh) = layout.size();
            height_text += lh as f64 / PANGO_SCALE as f64;
            Some(layout)
        } else {
            None
        };

        let utensils = format!("using {}", step.utensils.join(", "));
        let text_utensils = if !step.utensils.is_empty() {
            let attrs = AttrList::new();
            attrs.insert(AttrInt::new_line_height_absolute(
                (LINE_HEIGHT_UTENSILS * PANGO_SCALE as f64) as i32,
            ));
            let layout = create_layout(ctx);
            layout.set_width(tw);
            layout.set_font_description(Some(&fonts.utensils));
            layout.set_attributes(Some(&attrs));
            layout.set_text(&utensils);
            let (_, lh) = layout.size();
            height_text += lh as f64 / PANGO_SCALE as f64;
            Some(layout)
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
        self.height_text
        // self.height_time
    }
}

impl Timed for SizedStep {
    fn get_duration(&self) -> f64 {
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
    // 0
    // let file = File::open("recipe_single.ron")?;

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
    let file = File::open("recipe_toscana.ron")?;

    let recipe: Recipe = ron::de::from_reader(&file)?;

    let paint_surface = ImageSurface::create(cairo::Format::Rgb24, 1, 1)?;
    let paint_ctx = Context::new(&paint_surface)?;
    let colors = Colors::new(&paint_ctx);
    let fonts = Fonts::new();

    let edges = recipe.edges();
    let nodes = recipe
        .steps
        .into_iter()
        .map(|step| SizedStep::new(step, NODE_WIDTH, &paint_ctx, &fonts))
        .collect::<Result<Vec<_>, ()>>()
        .unwrap();

    let rects = layout(&nodes, &edges, SPACING, SPACING)?;
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
    ctx.set_source(&colors.background)?;
    ctx.paint()?;

    // draw boxes ///////////
    ctx.set_source(&colors.box_)?;
    for (step, rect) in nodes.iter() {
        ctx.rectangle(rect.x, rect.y, rect.width, step.height_text);
        ctx.fill()?;
    }

    for (from, to) in edges.iter().copied() {
        let from_rect = &nodes[from].1;
        let to_rect = &nodes[to].1;
        ctx.move_to(from_rect.left(), from_rect.top());
        ctx.curve_to(
            from_rect.left(),
            from_rect.top() - SPACING / 1.0,
            to_rect.left(),
            to_rect.bottom(),
            to_rect.left(),
            to_rect.bottom() - SPACING / 1.0,
        );
        ctx.line_to(to_rect.right(), to_rect.bottom());
        ctx.curve_to(
            to_rect.right(),
            to_rect.bottom() + SPACING / 1.0,
            from_rect.right(),
            from_rect.top() - SPACING / 1.0,
            from_rect.right(),
            from_rect.top(),
        );
        ctx.fill()?;
    }

    // draw lines ///////////
    let connection_offset_y = MARGIN_BOX + FONT_SIZE_TITLE / 2.0;
    ctx.set_source(&colors.line)?;
    for (_step, rect) in nodes.iter() {
        ctx.move_to(rect.left(), rect.top() + connection_offset_y);
        ctx.line_to(rect.left(), rect.bottom() - SPACING);
        ctx.stroke()?;
        let radius = 3.0;
        ctx.move_to(rect.left(), rect.top());
        ctx.arc(
            rect.left(),
            rect.top() + connection_offset_y,
            radius,
            0.0,
            f64::consts::PI * 2.0,
        );
        ctx.fill()?;
    }

    for (from, to) in edges.iter().copied() {
        let from_rect = &nodes[from].1;
        let to_rect = &nodes[to].1;
        ctx.move_to(from_rect.left(), from_rect.top() + connection_offset_y);
        ctx.curve_to(
            from_rect.left(),
            from_rect.top() + connection_offset_y - 50.0,
            to_rect.left(),
            to_rect.bottom() - SPACING + 50.0,
            to_rect.left(),
            to_rect.bottom() - SPACING,
        );
        ctx.stroke()?;
    }

    // draw text ////////////
    for (step, rect) in nodes.iter() {
        let mut y = rect.y + MARGIN_BOX + 1.0;

        ctx.move_to(rect.x + 1.5 + MARGIN_BOX, y + 1.5);
        ctx.set_source(&colors.background)?;
        show_layout(&ctx, &step.text_title);
        ctx.move_to(rect.x + MARGIN_BOX, y);
        ctx.set_source(&colors.text)?;
        show_layout(&ctx, &step.text_title);
        y += step.text_title.size().1 as f64 / PANGO_SCALE as f64;

        if let Some(text) = &step.text_note {
            ctx.move_to(rect.x + 1.0 + MARGIN_BOX, y + 1.0);
            ctx.set_source(&colors.background)?;
            show_layout(&ctx, text);
            ctx.move_to(rect.x + MARGIN_BOX, y);
            ctx.set_source(&colors.text)?;
            show_layout(&ctx, text);
            y += text.size().1 as f64 / PANGO_SCALE as f64;
        }

        y += SPACING_TEXT;

        if let Some(text) = &step.text_ingredients {
            ctx.move_to(rect.x + 1.0 + MARGIN_BOX, y + 1.0);
            ctx.set_source(&colors.background)?;
            show_layout(&ctx, text);
            ctx.move_to(rect.x + MARGIN_BOX, y);
            ctx.set_source(&colors.text)?;
            show_layout(&ctx, text);
            y += text.size().1 as f64 / PANGO_SCALE as f64;
        }

        if let Some(text) = &step.text_utensils {
            ctx.move_to(rect.x + 1.0 + MARGIN_BOX, y + 1.0);
            ctx.set_source(&colors.background)?;
            show_layout(&ctx, text);
            ctx.move_to(rect.x + MARGIN_BOX, y);
            ctx.set_source(&colors.text)?;
            show_layout(&ctx, text);
            // y += text.size().1 as f64 / PANGO_SCALE as f64;
        }
    }

    let mut file = File::create("output.png")?;
    surface.write_to_png(&mut file)?;

    Ok(())
}
