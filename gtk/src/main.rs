use std::fmt;

use gtk::{cairo, gdk, prelude::*};

mod layout;

const APP_ID: &str = "com.zuramm.RecipeVisualizer";
const SPACING: i32 = 10;
const TIME_SCALE: i32 = 50;

// TODO: add start and end time for interactive mode
#[derive(Debug, Default, Clone, PartialEq)]
struct Step {
    time_s: usize,
    title: String,
    note: Option<String>,
    ingredients: Vec<String>,
    utensils: Vec<String>,
}

impl Step {
    fn create_wigdet(&self) -> gtk::Box {
        let container = gtk::Box::new(gtk::Orientation::Vertical, 0);
        container.set_width_request(120);
        container.add_css_class("step");

        let title_label = gtk::Label::new(Some(&self.title));
        title_label.set_xalign(0.0);
        title_label.set_wrap(true);
        title_label.add_css_class("title");
        container.append(&title_label);

        if let Some(note) = &self.note {
            let note_label = gtk::Label::new(Some(note));
            note_label.set_xalign(0.0);
            note_label.set_wrap(true);
            note_label.set_margin_bottom(5);
            note_label.add_css_class("note");
            container.append(&note_label);
        } else {
            title_label.set_margin_bottom(2);
        }

        if !self.ingredients.is_empty() {
            let ingredients_label = gtk::Label::new(Some(&self.ingredients.join(", ")));
            ingredients_label.set_xalign(0.0);
            ingredients_label.set_wrap(true);
            ingredients_label.add_css_class("ingredients");
            container.append(&ingredients_label);
        }

        if !self.utensils.is_empty() {
            let content = format!("using {}", &self.utensils.join(", "));
            let utensils_label = gtk::Label::new(Some(&content));
            utensils_label.set_xalign(0.0);
            utensils_label.set_wrap(true);
            utensils_label.add_css_class("utensils");
            container.append(&utensils_label);
        }

        container
    }
}

struct Recipe {
    steps: Vec<Step>,
    requirements: Vec<(usize, usize)>,
}

#[derive(Debug, PartialEq)]
struct StepNode {
    step: Step,
    visual_bounds: gdk::Rectangle,
    widget: gtk::Box,
}

impl fmt::Display for StepNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "node {} {:?} using {:?}",
            self.step.title, self.step.ingredients, self.step.utensils
        )
    }
}

impl layout::VisuallyBounded for StepNode {
    fn visual_left(&self) -> i32 {
        self.visual_bounds.x()
    }

    fn set_visual_left(&mut self, value: i32) {
        self.visual_bounds.set_x(value);
    }

    fn visual_top(&self) -> i32 {
        self.visual_bounds.y()
    }

    fn set_visual_top(&mut self, value: i32) {
        self.visual_bounds.set_y(value);
    }

    fn visual_width(&self) -> i32 {
        self.visual_bounds.width()
    }

    fn set_visual_width(&mut self, value: i32) {
        self.visual_bounds.set_width(value);
    }

    fn visual_height(&self) -> i32 {
        self.visual_bounds.height()
    }

    fn set_visual_height(&mut self, value: i32) {
        self.visual_bounds.set_height(value);
    }

    fn intersects(&self, other: &Self) -> bool {
        self.visual_bounds.intersect(&other.visual_bounds).is_some()
    }
}

impl layout::Timed for StepNode {
    fn time_in_minutes(&self) -> f64 {
        self.step.time_s as f64 / 60.0
    }

    fn time_in_units(&self) -> i32 {
        time_to_units(self.step.time_s)
    }
}

fn time_to_units(time_s: usize) -> i32 {
    time_s as i32 * TIME_SCALE / 60
}

fn on_draw_tree(
    _widget: &gtk::DrawingArea,
    cr: &cairo::Context,
    _width: i32,
    _height: i32,
    nodes: &[StepNode],
    edges: &[(usize, usize)],
) {
    cr.save().expect("invalid cairo state");

    // TODO: draw gray background
    // TODO: draw full width lines in regular intervals for time
    // TODO: draw time scale
    // TODO: draw time cursor, if interactive
    // TODO: draw white trail

    cr.set_source_color(&gdk::RGBA::new(0.26, 0.53, 0.96, 1.0));

    for node in nodes.iter() {
        let x1 = node.visual_bounds.x() as f64;
        let y1 = node.visual_bounds.y() as f64;
        let x2 = x1 + node.visual_bounds.width() as f64;
        let y2 = y1 + node.visual_bounds.height() as f64;
        cr.move_to(x2, y1);
        cr.line_to(x2, y2);
        // TODO: draw exact time indicator
    }

    for (from, to) in edges.iter().copied() {
        let from_x1 = nodes[from].visual_bounds.x() as f64;
        let from_y1 = nodes[from].visual_bounds.y() as f64;
        let from_x2 = from_x1 + nodes[from].visual_bounds.width() as f64;
        let from_y2 = from_y1 + nodes[from].visual_bounds.height() as f64;
        let to_x1 = nodes[to].visual_bounds.x() as f64;
        let to_y1 = nodes[to].visual_bounds.y() as f64;
        let to_x2 = to_x1 + nodes[to].visual_bounds.width() as f64;
        let to_y2 = to_y1 + nodes[to].visual_bounds.height() as f64;
        // TODO: connect curve to top right
        cr.move_to(from_x2, from_y1);
        cr.line_to(from_x2, from_y2);

        cr.curve_to(
            from_x2,
            // (from_y2 + to_y2) / 2.0,
            from_y2 + 100.0,
            to_x2,
            // (from_y2 + to_y2) / 2.0,
            to_y2 - 50.0,
            to_x2,
            to_y2,
        );
        // TODO: change color if critical and interactive
    }

    cr.stroke().expect("invalid cairo state");

    cr.restore().expect("invalid cairo state");
}

fn build_ui(application: &gtk::Application) {
    let root = gtk::Fixed::new();
    root.set_margin_start(SPACING);
    root.set_margin_end(SPACING);
    root.set_margin_top(SPACING);
    root.set_margin_bottom(SPACING);

    let drawing_area = gtk::DrawingArea::new();
    drawing_area.set_content_width(500);
    drawing_area.set_content_height(500);
    root.put(&drawing_area, 0.0, 0.0);

    SAMPLE_RECIPE.with(|recipe: &Recipe| {
        let mut nodes = recipe
            .steps
            .iter()
            .map(|step| {
                let widget = step.create_wigdet();
                let width = widget.width_request();
                let (_minimum, natural, _, _) = widget.measure(gtk::Orientation::Vertical, width);
                StepNode {
                    step: step.clone(),
                    visual_bounds: gdk::Rectangle::new(0, 0, width, natural.max(time_to_units(step.time_s))),
                    widget,
                }
            })
            .collect::<Vec<_>>();

        layout::layout(&mut nodes, &recipe.requirements, SPACING);

        let bounds = nodes
            .iter()
            .map(|node| node.visual_bounds)
            .reduce(|acc, node| acc.union(&node));

        println!("bounds: {:?}", bounds);

        if let Some(bounds) = bounds {
            drawing_area.set_content_width(bounds.width() + 2 * SPACING);
            drawing_area.set_content_height(bounds.height() + 2 * SPACING);

            for node in nodes.iter_mut() {
                node.visual_bounds
                    .set_x(node.visual_bounds.x() - bounds.x() + SPACING);
                node.visual_bounds
                    .set_y(node.visual_bounds.y() - bounds.y() + SPACING);
                root.put(
                    &node.widget,
                    node.visual_bounds.x() as f64,
                    node.visual_bounds.y() as f64,
                );
            }

            let requirements = recipe.requirements.clone();

            drawing_area
                .set_draw_func(move |a, b, c, d| on_draw_tree(a, b, c, d, &nodes, &requirements));
        }
    });

    let window = gtk::ApplicationWindow::builder()
        .application(application)
        .title("Recipe Visualizer")
        .default_width(350)
        .default_height(200)
        .child(&root)
        .build();

    application.connect_activate(move |_| {
        window.present();
    });
}

fn main() {
    let app = gtk::Application::builder().application_id(APP_ID).build();

    app.connect_startup(|app| {
        let provider = gtk::CssProvider::new();
        provider.load_from_string(include_str!("style.css"));
        gtk::style_context_add_provider_for_display(
            &gdk::Display::default().expect("Could not connect to a display."),
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        build_ui(app);
    });

    app.run();
}

thread_local!(static SAMPLE_RECIPE: Recipe = Recipe {
    steps: vec![
        // 0
        Step {
            time_s: 5 * 60,
            title: "Heat".into(),
            ingredients: vec!["oil".into()],
            utensils: vec!["frying pan".into()],
            ..Default::default()
        },
        // 1
        Step {
            time_s: 20 * 60,
            title: "Cook".into(),
            ingredients: vec!["rice".into(), "water".into()],
            utensils: vec!["pot".into()],
            ..Default::default()
        },
        // 2
        Step {
            time_s: 10 * 60,
            title: "Cut".into(),
            note: Some("Into small pieces".into()),
            ingredients: vec!["vegtables".into()],
            ..Default::default()
        },
        // 3
        Step {
            time_s: 10 * 60,
            title: "Steam".into(),
            utensils: vec!["steamer or steaming pot".into()],
            ..Default::default()
        },
        // 4
        Step {
            time_s: 5 * 60,
            title: "Cut".into(),
            note: Some("Into cubes".into()),
            ingredients: vec!["avocado".into()],
            ..Default::default()
        },
        // 5
        Step {
            time_s: 5 * 60,
            title: "Cut".into(),
            note: Some("Into strips".into()),
            ingredients: vec!["chicken".into()],
            ..Default::default()
        },
        // 6
        Step {
            time_s: 60,
            title: "Season".into(),
            note: Some("Generously, preferably the day before".into()),
            ingredients: vec!["roast chicken seasoning".into()],
            ..Default::default()
        },
        // 7
        Step {
            time_s: 10 * 60,
            title: "Fry".into(),
            note: Some("Until golden brown".into()),
            ..Default::default()
        },
        // 8
        Step {
            time_s: 60,
            title: "Add".into(),
            utensils: vec!["bowl".into()],
            ..Default::default()
        },
        // 9
        Step {
            time_s: 60,
            title: "Season".into(),
            ingredients: vec!["roasted vegetable seasoning".into()],
            ..Default::default()
        },
        // 10
        Step {
            time_s: 60,
            title: "Mix".into(),
            note: Some("Gently, to combine the ingredients".into()),
            ..Default::default()
        },
        // 11
        Step {
            time_s: 60,
            title: "Squeze".into(),
            note: Some("Optionally, for an extra burst of flavor".into()),
            ingredients: vec!["lime".into()],
            ..Default::default()
        },
    ],
    requirements: vec![
        (0, 7),
        (1, 8),
        (2, 3),
        (3, 8),
        (4, 10),
        (5, 6),
        (6, 7),
        (7, 8),
        (8, 9),
        (9, 10),
        (10, 11),
    ],
});
