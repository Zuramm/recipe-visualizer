use cairo::{Context, ImageSurface, Pattern};
use clap::Parser;
use hyphenation::{Hyphenator, Load};
use pango::{ffi::PANGO_SCALE, AttrInt, AttrList, FontDescription};
use pangocairo::functions::{create_layout, show_layout};
use std::{f64, fs::File, io, path::PathBuf};
use thiserror::Error;
use visualizer::{
    layout::{layout, LayoutError, Rect, Timed},
    Ingredient, Recipe, Step,
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

struct TextLayout {
    layout: pango::Layout,
    bounds: Rect<f64>,
}

impl TextLayout {
    fn new(
        ctx: &cairo::Context,
        max_width: Option<(&hyphenation::Standard, f64)>,
        font: &FontDescription,
        text: &str,
        line_height: f64,
    ) -> Self {
        let text = if let Some((hyphen, _)) = max_width {
            place_hyphens(hyphen, text)
        } else {
            text.to_owned()
        };
        let attrs = AttrList::new();
        attrs.insert(AttrInt::new_line_height_absolute(
            (line_height * PANGO_SCALE as f64) as i32,
        ));
        let layout = create_layout(ctx);
        if let Some((_, max_width)) = max_width {
            layout.set_width((max_width * PANGO_SCALE as f64) as i32);
        }
        layout.set_font_description(Some(font));
        layout.set_attributes(Some(&attrs));
        layout.set_text(&text);
        let (width, height) = layout.size();
        let bounds = Rect::sized(
            width as f64 / PANGO_SCALE as f64,
            height as f64 / PANGO_SCALE as f64,
        );
        Self { layout, bounds }
    }

    fn draw(&self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        ctx.save()?;
        ctx.move_to(self.bounds.x, self.bounds.y);

        show_layout(ctx, &self.layout);

        ctx.restore()?;
        Ok(())
    }
}

struct StepLayout {
    time_s: u32,
    layout_title: TextLayout,
    layout_note: Option<TextLayout>,
    layout_ingredients: Option<TextLayout>,
    layout_utensils: Option<TextLayout>,
    bounds: Rect<f64>,
    is_leaf: bool,
}

impl StepLayout {
    fn new(
        ctx: &cairo::Context,
        hyphen: &hyphenation::Standard,
        fonts: &Fonts,
        step: Step,
    ) -> Result<Self, ()> {
        let mut height = MARGIN_BOX;

        let text_width = Some((hyphen, NODE_WIDTH - MARGIN_BOX * 2.0));

        let mut layout_title = TextLayout::new(
            ctx,
            text_width,
            &fonts.title,
            &step.title,
            LINE_HEIGHT_TITLE,
        );
        layout_title.bounds.y = height;
        height += layout_title.bounds.height;

        let layout_note = step.note.map(|note| {
            let mut layout = TextLayout::new(ctx, text_width, &fonts.note, &note, LINE_HEIGHT_NOTE);
            layout.bounds.y = height;
            height += layout.bounds.height;
            layout
        });

        if !step.ingredients.is_empty() || !step.utensils.is_empty() {
            height += SPACING_TEXT;
        }

        let layout_ingredients = if !step.ingredients.is_empty() {
            let ingredients = place_hyphens(hyphen, &step.ingredients.join(", "));
            let mut layout = TextLayout::new(
                ctx,
                text_width,
                &fonts.ingredients,
                &ingredients,
                LINE_HEIGHT_INGREDIENTS,
            );
            layout.bounds.y = height;
            height += layout.bounds.height;
            Some(layout)
        } else {
            None
        };

        let layout_utensils = if !step.utensils.is_empty() {
            let utensils = place_hyphens(hyphen, &format!("using {}", step.utensils.join(", ")));
            let mut layout = TextLayout::new(
                ctx,
                text_width,
                &fonts.utensils,
                &utensils,
                LINE_HEIGHT_UTENSILS,
            );
            layout.bounds.y = height;
            height += layout.bounds.height;
            Some(layout)
        } else {
            None
        };

        height += MARGIN_BOX;

        Ok(Self {
            time_s: step.time_s as u32,
            layout_title,
            layout_note,
            layout_ingredients,
            layout_utensils,
            bounds: Rect {
                x: 0.0,
                y: 0.0,
                width: NODE_WIDTH,
                height,
            },
            is_leaf: step.requires.is_empty(),
        })
    }

    fn draw_connection_background(
        &self,
        other: &Self,
        ctx: &cairo::Context,
    ) -> Result<(), cairo::Error> {
        let from_rect = &self.bounds;
        let to_rect = &other.bounds;

        ctx.save()?;
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

        ctx.restore()?;
        Ok(())
    }

    fn draw_background(&self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        let rect = &self.bounds;

        ctx.rectangle(rect.x, rect.y, rect.width, rect.height);
        ctx.fill()?;

        Ok(())
    }

    fn draw_line(&self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        let connection_offset_y = MARGIN_BOX + FONT_SIZE_TITLE / 2.0;
        let rect = &self.bounds;

        ctx.save()?;
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

        ctx.restore()?;
        Ok(())
    }

    fn draw_connection_line(&self, other: &Self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        let connection_offset_y = MARGIN_BOX + FONT_SIZE_TITLE / 2.0;
        let from_rect = &self.bounds;
        let to_rect = &other.bounds;

        ctx.save()?;
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

        ctx.restore()?;
        Ok(())
    }

    fn draw_text(&self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        let rect = &self.bounds;

        ctx.save()?;
        ctx.translate(rect.x + MARGIN_BOX, rect.y + 1.0);

        self.layout_title.draw(ctx)?;

        if let Some(layout) = &self.layout_note {
            layout.draw(ctx)?;
        }

        if let Some(layout) = &self.layout_ingredients {
            layout.draw(ctx)?;
        }

        if let Some(layout) = &self.layout_utensils {
            layout.draw(ctx)?;
        }

        ctx.restore()?;
        Ok(())
    }
}

impl Timed for StepLayout {
    fn get_duration(&self) -> u32 {
        self.time_s
    }
}

struct GraphLayout {
    layouts: Vec<StepLayout>,
    bounds: Rect<f64>,
}

impl GraphLayout {
    fn new(
        ctx: &cairo::Context,
        hyphen: &hyphenation::Standard,
        fonts: &Fonts,
        steps: Vec<Step>,
        edges: &[(usize, usize)],
    ) -> Result<Self, LayoutError> {
        let mut nodes = steps
            .into_iter()
            .map(|step| StepLayout::new(ctx, hyphen, fonts, step))
            .collect::<Result<Vec<_>, ()>>()
            .unwrap();

        let rects = layout(&nodes, edges)?;
        let bounds = Rect::bounded_nodes(rects.iter().copied())
            .expect("`rects` should contain more than one element");

        let mut rows = vec![Vec::<(usize, u32)>::new(); bounds.height as usize];
        let rects = rects.into_iter().enumerate().collect::<Vec<_>>();
        for (i, (x, y)) in rects {
            rows[y as usize].push((i, x));
        }

        let mut column = vec![0.0; bounds.width as usize];
        let mut last_row = 0.0;

        for row in rows {
            let mut height = 0.0f64;
            for (i, x) in row.iter().copied() {
                nodes[i].bounds.x = x as f64 * (NODE_WIDTH + SPACING);
                nodes[i].bounds.y = if nodes[i].is_leaf {
                    column[x as usize]
                } else {
                    last_row
                };
                column[x as usize] = nodes[i].bounds.bottom() + SPACING;
                height = height.max(nodes[i].bounds.bottom());
            }

            last_row = height;

            for (i, x) in row.iter().copied() {
                if nodes[i].is_leaf {
                    nodes[i].bounds.y = last_row - nodes[i].bounds.height;
                    column[x as usize] = nodes[i].bounds.bottom() + SPACING;
                }
            }

            last_row += SPACING;
        }

        let bounds = Rect::bounded(nodes.iter().map(|node| &node.bounds))
            .expect("there should be as least one node");

        Ok(Self {
            layouts: nodes,
            bounds,
        })
    }

    fn draw(
        &self,
        ctx: &cairo::Context,
        colors: &Colors,
        edges: &[(usize, usize)],
        show_ids: bool,
    ) -> Result<(), cairo::Error> {
        ctx.save()?;
        ctx.translate(self.bounds.x, self.bounds.y);

        // draw boxes ///////////
        ctx.set_source(&colors.box_)?;
        for step in self.layouts.iter() {
            step.draw_background(ctx)?;
        }

        for (from, to) in edges.iter().copied() {
            let from_rect = &self.layouts[from];
            let to_rect = &self.layouts[to];
            from_rect.draw_connection_background(to_rect, ctx)?;
        }

        // draw lines ///////////
        ctx.set_source(&colors.line)?;
        for step in self.layouts.iter() {
            step.draw_line(ctx)?;
        }

        for (from, to) in edges.iter().copied() {
            let from_rect = &self.layouts[from];
            let to_rect = &self.layouts[to];
            from_rect.draw_connection_line(to_rect, ctx)?;
        }

        // draw text ////////////
        ctx.translate(1.5, 1.5);
        ctx.set_source(&colors.background)?;
        for step in self.layouts.iter() {
            step.draw_text(ctx)?;
        }

        ctx.translate(-1.5, -1.5);
        ctx.set_source(&colors.text)?;
        for step in self.layouts.iter() {
            step.draw_text(ctx)?;
        }

        if show_ids {
            ctx.set_source_rgb(1.0, 0.0, 0.0);
            for (i, step) in self.layouts.iter().enumerate() {
                let rect = &step.bounds;
                ctx.move_to(rect.left(), rect.bottom());
                ctx.show_text(&i.to_string())?;
            }
        }

        ctx.restore()?;

        Ok(())
    }
}

struct IngredientsLayout {
    layouts: Vec<(Option<TextLayout>, TextLayout)>,
    bounds: Rect<f64>,
}

impl IngredientsLayout {
    fn new(ctx: &cairo::Context, fonts: &Fonts, ingredients: Vec<Ingredient>) -> Self {
        let mut width_amount = 0.0f64;
        let mut width_name = 0.0f64;
        let mut height_total = 0.0f64;

        let mut layouts = ingredients
            .into_iter()
            .map(|ingredient| {
                let mut height = 0.0f64;

                let layout_amount = ingredient.amount.map(|amount| {
                    let mut layout = TextLayout::new(
                        ctx,
                        None,
                        &fonts.ingredients,
                        &amount,
                        LINE_HEIGHT_INGREDIENTS,
                    );
                    layout.bounds.y = height_total;

                    width_amount = width_amount.max(layout.bounds.width);
                    height = layout.bounds.height;

                    layout
                });

                let name = match ingredient.comment {
                    Some(comment) => format!("{} ({})", ingredient.name, comment),
                    None => ingredient.name,
                };
                let mut layout_name = TextLayout::new(
                    ctx,
                    None,
                    &fonts.ingredients,
                    &name,
                    LINE_HEIGHT_INGREDIENTS,
                );
                layout_name.bounds.y = height_total;

                width_name = width_name.max(layout_name.bounds.width);
                height = height.max(layout_name.bounds.height);
                height_total += height;

                (layout_amount, layout_name)
            })
            .collect::<Vec<_>>();

        for (layout_amount, layout_name) in layouts.iter_mut() {
            if let Some(layout_amount) = layout_amount {
                layout_amount.bounds.x = width_amount - layout_amount.bounds.width;
            }
            layout_name.bounds.x = width_amount + SPACING_TEXT;
        }

        IngredientsLayout {
            layouts,
            bounds: Rect::sized(width_amount + SPACING_TEXT + width_name, height_total),
        }
    }

    fn draw(&self, ctx: &cairo::Context) -> Result<(), cairo::Error> {
        ctx.save()?;
        ctx.translate(self.bounds.x, self.bounds.y);

        for (amount, name) in self.layouts.iter() {
            if let Some(amount) = amount {
                amount.draw(ctx)?;
            }
            name.draw(ctx)?;
        }

        ctx.restore()?;

        Ok(())
    }
}

/// Generate an image for a recipe with a name, ingredients, and a graph for the instructions
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to recipe file in ron format
    recipe: PathBuf,

    output: Option<PathBuf>,

    /// Show the id of each node; for debugging purposes
    #[arg(long)]
    show_ids: bool,
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

    let file = File::open(&args.recipe)?;
    let recipe: Recipe = ron::de::from_reader(&file)?;
    let edges = recipe.edges();

    let en_us = hyphenation::Standard::from_embedded(hyphenation::Language::EnglishUS)?;

    let surface = ImageSurface::create(cairo::Format::Rgb24, 1, 1)?;
    let ctx = Context::new(&surface)?;
    let colors = Colors::new(&ctx);
    let fonts = Fonts::new();

    let mut layout_ingredients = IngredientsLayout::new(&ctx, &fonts, recipe.ingredients);

    let mut layout_graph = GraphLayout::new(&ctx, &en_us, &fonts, recipe.steps, &edges)?;
    layout_graph.bounds.x = layout_ingredients.bounds.width + SPACING;

    let layout_name = TextLayout::new(
        &ctx,
        Some((
            &en_us,
            layout_ingredients.bounds.height + SPACING + layout_graph.bounds.width,
        )),
        &fonts.recipe_title,
        &recipe.name,
        LINE_HEIGHT_RECIPE_TITLE,
    );

    layout_ingredients.bounds.y = layout_name.bounds.height + SPACING;
    layout_graph.bounds.y = layout_name.bounds.height + SPACING;

    let surface = ImageSurface::create(
        cairo::Format::Rgb24,
        (MARGIN_PAGE * 2.0 + layout_ingredients.bounds.width + SPACING + layout_graph.bounds.width)
            .ceil() as i32,
        (MARGIN_PAGE * 2.0
            + layout_name.bounds.height
            + SPACING
            + f64::max(layout_ingredients.bounds.height, layout_graph.bounds.height))
        .ceil() as i32,
    )?;
    let ctx = Context::new(&surface)?;
    ctx.translate(MARGIN_PAGE, MARGIN_PAGE);

    ctx.set_source(&colors.background)?;
    ctx.paint()?;

    ctx.set_source(&colors.text)?;

    layout_name.draw(&ctx)?;

    layout_ingredients.draw(&ctx)?;

    layout_graph.draw(&ctx, &colors, &edges, args.show_ids)?;

    let output_path = args
        .output
        .clone()
        .unwrap_or(args.recipe.with_extension("png"));
    let mut file = File::create(output_path)?;
    surface.write_to_png(&mut file)?;

    Ok(())
}

fn main() {
    if let Err(error) = main_() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
