use cairo::{Context, ImageSurface, Pattern};
use clap::Parser;
use hyphenation::{Hyphenator, Load};
use pango::{ffi::PANGO_SCALE, AttrInt, AttrList, FontDescription};
use pangocairo::functions::{create_layout, show_layout};
use std::{f64, fs::File, io, path::PathBuf};
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

const FONT_SIZE_RECIPE_TITLE: f64 = 20.0;
const FONT_SIZE_TITLE: f64 = 20.0;
const FONT_SIZE_NOTE: f64 = 12.0;
const FONT_SIZE_INGREDIENTS: f64 = 12.0;
const FONT_SIZE_UTENSILS: f64 = 12.0;

const LINE_HEIGHT_RECIPE_TITLE: f64 = 24.0;
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
    recipe_title: FontDescription,
    title: FontDescription,
    note: FontDescription,
    ingredients: FontDescription,
    utensils: FontDescription,
}

impl Fonts {
    fn new() -> Self {
        let mut recipe_title = FontDescription::new();
        recipe_title.set_family("Inria Serif");
        recipe_title.set_absolute_size(FONT_SIZE_RECIPE_TITLE * PANGO_SCALE as f64);
        recipe_title.set_weight(pango::Weight::Bold);

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
            recipe_title,
            title,
            note,
            ingredients,
            utensils,
        }
    }
}

fn place_hyphens(hyphen: &hyphenation::Standard, string: &str) -> String {
    let mut hyphen_breaks = hyphen.hyphenate(string).breaks;
    hyphen_breaks.insert(0, 0);

    let mut hyphenated = String::new();

    for window in hyphen_breaks.windows(2) {
        let start = window[0];
        let end = window[1];
        hyphenated.push_str(&string[start..end]);
        hyphenated.push('\u{00ad}');
    }

    hyphenated.push_str(&string[*hyphen_breaks.last().unwrap()..]);

    hyphenated
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
    fn new(
        step: Step,
        width: f64,
        ctx: &cairo::Context,
        fonts: &Fonts,
        hyphen: &hyphenation::Standard,
    ) -> Result<Self, ()> {
        let height_time = step.time_s as f64;
        let mut height_text = MARGIN_BOX * 2.0;

        let tw = ((width - MARGIN_BOX * 2.0) * PANGO_SCALE as f64) as i32;

        let title = place_hyphens(hyphen, &step.title);
        let attrs = AttrList::new();
        attrs.insert(AttrInt::new_line_height_absolute(
            (LINE_HEIGHT_TITLE * PANGO_SCALE as f64) as i32,
        ));
        let text_title = create_layout(ctx);
        text_title.set_width(tw);
        text_title.set_font_description(Some(&fonts.title));
        text_title.set_attributes(Some(&attrs));
        text_title.set_text(&title);
        let (_, lh) = text_title.size();
        height_text += lh as f64 / PANGO_SCALE as f64;

        let text_note = step.note.map(|note| {
            let note = place_hyphens(hyphen, &note);
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

        let ingredients = place_hyphens(hyphen, &step.ingredients.join(", "));
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

        let utensils = place_hyphens(hyphen, &format!("using {}", step.utensils.join(", ")));
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

fn get_layout_size(layout: &pango::Layout) -> (f64, f64) {
    let (w, h) = layout.size();
    (w as f64 / PANGO_SCALE as f64, h as f64 / PANGO_SCALE as f64)
}

/// Generate an image for a recipe with a name, ingredients, and a graph for the instructions
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to recipe file in ron format
    recipe: PathBuf,
}

#[derive(Debug, Error)]
enum MainError {
    #[error("Layout error: {0}")]
    Layout(#[from] LayoutError),
    #[error("Cairo error: {0}")]
    Cairo(#[from] cairo::Error),
    #[error("Io error: {0}")]
    Io(#[from] io::Error),
    #[error("Ron error: {0}")]
    RonDe(#[from] ron::error::SpannedError),
    #[error("Hyphenation error: {0}")]
    Hyphenation(#[from] hyphenation::load::Error),
}

impl From<cairo::IoError> for MainError {
    fn from(error: cairo::IoError) -> Self {
        match error {
            cairo::IoError::Cairo(error) => MainError::Cairo(error),
            cairo::IoError::Io(error) => MainError::Io(error),
        }
    }
}

fn main_() -> Result<(), MainError> {
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
    // let file = File::open("recipe_toscana.ron")?;

    let args = Args::parse();

    let file = File::open(args.recipe)?;
    let recipe: Recipe = ron::de::from_reader(&file)?;

    let en_us = hyphenation::Standard::from_embedded(hyphenation::Language::EnglishUS)?;

    let paint_surface = ImageSurface::create(cairo::Format::Rgb24, 1, 1)?;
    let paint_ctx = Context::new(&paint_surface)?;
    let colors = Colors::new(&paint_ctx);
    let fonts = Fonts::new();

    let edges = recipe.edges();
    let nodes = recipe
        .steps
        .into_iter()
        .map(|step| SizedStep::new(step, NODE_WIDTH, &paint_ctx, &fonts, &en_us))
        .collect::<Result<Vec<_>, ()>>()
        .unwrap();

    let rects = layout(&nodes, &edges, SPACING, SPACING)?;
    let bounds = Rect::bounded(&rects).expect("`rects` should contain more than one element");
    let nodes = nodes.into_iter().zip(rects).collect::<Vec<_>>();

    let attrs = AttrList::new();
    attrs.insert(AttrInt::new_line_height_absolute(
        (LINE_HEIGHT_RECIPE_TITLE * PANGO_SCALE as f64) as i32,
    ));
    let text_recipe_title = create_layout(&paint_ctx);
    text_recipe_title.set_width((bounds.width * PANGO_SCALE as f64) as i32);
    text_recipe_title.set_font_description(Some(&fonts.recipe_title));
    text_recipe_title.set_attributes(Some(&attrs));
    text_recipe_title.set_text(&recipe.name);
    let (_, lh) = text_recipe_title.size();
    let height_recipe_title = lh as f64 / PANGO_SCALE as f64;

    let mut width_ingredients = (0.0f64, 0.0f64);
    let mut height_ingredients = 0.0;
    let mut text_ingredients = Vec::<(Option<pango::Layout>, pango::Layout)>::new();
    let attrs = AttrList::new();
    attrs.insert(AttrInt::new_line_height_absolute(
        (LINE_HEIGHT_INGREDIENTS * PANGO_SCALE as f64) as i32,
    ));
    for ingredient in recipe.ingredients {
        let mut height = 0.0;

        let text_amount = ingredient.amount.map(|amount| {
            let layout = create_layout(&paint_ctx);
            layout.set_font_description(Some(&fonts.ingredients));
            layout.set_attributes(Some(&attrs));
            layout.set_text(&amount);
            let (lw, lh) = layout.size();
            let width = lw as f64 / PANGO_SCALE as f64;
            width_ingredients.0 = width_ingredients.0.max(width);
            height = lh as f64 / PANGO_SCALE as f64;
            layout
        });

        let name = match ingredient.comment {
            Some(comment) => format!("{} ({})", ingredient.name, comment),
            None => ingredient.name,
        };
        let text_name = create_layout(&paint_ctx);
        text_name.set_font_description(Some(&fonts.ingredients));
        text_name.set_attributes(Some(&attrs));
        text_name.set_text(&name);
        let (lw, lh) = text_name.size();
        let width = lw as f64 / PANGO_SCALE as f64;
        width_ingredients.1 = width_ingredients.1.max(width);
        height = height.max(lh as f64 / PANGO_SCALE as f64);

        height_ingredients += height;
        text_ingredients.push((text_amount, text_name));
    }

    let surface = ImageSurface::create(
        cairo::Format::Rgb24,
        (MARGIN_PAGE * 2.0
            + width_ingredients.0
            + SPACING_TEXT
            + width_ingredients.1
            + SPACING
            + bounds.width)
            .ceil() as i32,
        (MARGIN_PAGE * 2.0
            + height_recipe_title
            + SPACING
            + f64::max(height_ingredients, bounds.height))
        .ceil() as i32,
    )?;
    let ctx = Context::new(&surface)?;
    ctx.translate(MARGIN_PAGE, MARGIN_PAGE);

    // draw background //////
    ctx.set_source(&colors.background)?;
    ctx.paint()?;

    // draw title ///////////
    ctx.set_source(&colors.text)?;
    ctx.move_to(0.0, 0.0);
    show_layout(&ctx, &text_recipe_title);
    ctx.translate(0.0, lh as f64 / PANGO_SCALE as f64 + SPACING);

    // draw ingredients /////
    let mut y = 0.0;
    for (amount, name) in text_ingredients {
        let mut height = 0.0;
        if let Some(amount) = amount {
            let (lw, lh) = get_layout_size(&amount);
            ctx.move_to(width_ingredients.0 - lw, y);
            show_layout(&ctx, &amount);
            height = lh;
        }
        ctx.move_to(width_ingredients.0 + SPACING_TEXT, y);
        show_layout(&ctx, &name);
        height = height.max(get_layout_size(&name).1);
        y += height;
    }
    ctx.translate(
        width_ingredients.0 + SPACING_TEXT + width_ingredients.1 + SPACING,
        0.0,
    );

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

fn main() {
    if let Err(error) = main_() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
