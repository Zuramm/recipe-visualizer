use core::{fmt, cmp::PartialEq};

use petgraph::{algo::bellman_ford, prelude::*, visit::NodeIndexable};

pub trait VisuallyBounded: fmt::Debug {
    fn visual_left(&self) -> i32;
    fn set_visual_left(&mut self, value: i32);

    fn visual_top(&self) -> i32;
    fn set_visual_top(&mut self, value: i32);

    fn visual_width(&self) -> i32;
    fn set_visual_width(&mut self, value: i32);

    fn visual_height(&self) -> i32;
    fn set_visual_height(&mut self, value: i32);

    fn visual_right(&self) -> i32 {
        self.visual_left() + self.visual_width()
    }

    fn visual_bottom(&self) -> i32 {
        self.visual_top() + self.visual_height()
    }

    fn intersects(&self, other: &Self) -> bool;
}

pub trait Timed: fmt::Debug {
    fn time_in_minutes(&self) -> f64;

    fn time_in_units(&self) -> i32;
}

fn create_graph<'a, NodeWeight>(
    nodes: &'a mut [NodeWeight],
    edges: &[(usize, usize)],
) -> DiGraph<&'a mut NodeWeight, f64>
where
    NodeWeight: VisuallyBounded + Timed,
{
    let mut graph = DiGraph::<&mut NodeWeight, f64>::new();

    // initialize arrays and calculate dimensions
    for (i, node) in nodes.iter_mut().enumerate() {
        let id = graph.add_node(node);
        assert_eq!(i, id.index());
    }

    for &(from, to) in edges.iter() {
        let from = graph.from_index(from);
        let to = graph.from_index(to);
        graph.add_edge(
            from,
            to,
            graph
                .node_weight(from)
                .map_or(-1.0, |node| node.time_in_minutes() * -1.0),
        );
    }

    graph
}

pub fn layout<NodeWeight>(nodes: &mut [NodeWeight], edges: &[(usize, usize)], spacing: i32)
where
    NodeWeight: fmt::Display + VisuallyBounded + Timed + PartialEq,
{
    let mut graph = create_graph(nodes, edges);

    // assume graph is acyclic
    // and connected
    // and has one leaf (every path ends in the same node)
    let roots = graph.node_indices().filter(|&id| {
        graph
            .neighbors_directed(id, petgraph::Direction::Incoming)
            .next()
            .is_none()
    });

    let longest_path = roots
        .map(|id| bellman_ford(&graph, id))
        .filter_map(|res| res.ok())
        .min_by(|a, b| {
            match (
                a.distances.iter().copied().min_by(f64::total_cmp),
                b.distances.iter().copied().min_by(f64::total_cmp),
            ) {
                (Some(min_a), Some(min_b)) => min_a.total_cmp(&min_b),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => std::cmp::Ordering::Equal,
            }
        });

    let root_idx_path = longest_path.and_then(|path| {
        let end = path
            .distances
            .iter()
            .copied()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.total_cmp(b))
            .map(|(end, _)| end);

        end.map(NodeIndex::new)
            .map(|index| layout_path(&mut graph, path, index, spacing))
    });

    root_idx_path.map(|mut root_idx_path| {
        println!("root path:");
        println!(
            "{:?}",
            root_idx_path
                .iter()
                .map(|idx| idx.index())
                .collect::<Vec<_>>()
        );
        println!();

        root_idx_path.reverse();
        let mut pontential_branches = root_idx_path;

        while let Some(idx) = pontential_branches.pop() {
            loop {
                println!("node {}:", idx.index());
                let roots = graph.node_indices().filter(|&id| {
                    graph
                        .neighbors_directed(id, petgraph::Direction::Incoming)
                        .next()
                        .is_none()
                        && graph
                            .neighbors_directed(id, petgraph::Direction::Outgoing)
                            .next()
                            .is_some()
                });
                let longest_path = roots
                    .map(|id| bellman_ford(&graph, id))
                    .filter_map(|res| res.ok())
                    .min_by(|a, b| a.distances[idx.index()].total_cmp(&b.distances[idx.index()]));
                if longest_path.as_ref().and_then(|paths| paths.predecessors[idx.index()]).is_none() {
                    break;
                }
                let mut idx_path = longest_path
                    .map(|longest_path| layout_path(&mut graph, longest_path, idx, spacing))
                    .unwrap_or(Vec::new());

                println!(
                    "  path: {:?}",
                    idx_path
                        .iter()
                        .map(|idx| idx.index())
                        .collect::<Vec<_>>()
                );

                // last node is the current one which already is covered
                let _ = idx_path.pop();
                idx_path.reverse();
                pontential_branches.append(&mut idx_path);

                println!();
            }
        }
    });
}

fn layout_path<NodeWeight>(
    graph: &mut Graph<&mut NodeWeight, f64>,
    paths: bellman_ford::Paths<NodeIndex, f64>,
    idx: NodeIndex,
    spacing: i32,
) -> Vec<NodeIndex>
where
    NodeWeight: fmt::Display + VisuallyBounded + Timed + PartialEq,
{
    let mut idx_path = Vec::<NodeIndex>::new();

    let mut current = Some(idx);
    while let Some(to) = current {
        idx_path.push(to);

        let from = paths.predecessors[to.index()];

        match (
            from.and_then(|from| graph.node_weight(from)),
            graph.node_weight(to),
        ) {
            (Some(from), Some(to)) => {
                let from_time_dist = from.time_in_units();
                let (pos_x, height) = if from_time_dist < from.visual_height() {
                    (to.visual_width() + spacing, None)
                } else {
                    (
                        to.visual_left(),
                        Some(from.visual_top() - to.visual_top() - spacing),
                    )
                };
                let pos_y = to.visual_top() - from_time_dist;
                Some((pos_x, pos_y, height))
            }
            _ => None,
        }
        .and_then(|pos| {
            from.and_then(|from| graph.node_weight_mut(from))
                .map(|from_weight| {
                    from_weight.set_visual_left(pos.0);
                    from_weight.set_visual_top(pos.1);
                })
        });

        from.and_then(|from| graph.find_edge(from, to))
            .map(|edge| graph.remove_edge(edge));

        current = from;
    }

    println!("  {:?}", idx_path);
    while let Some(offset) = idx_path.iter()
        .skip(1)
        .copied()
        .filter_map(|idx| graph.node_weight(idx))
        .flat_map(|path_weight| graph.node_weights().filter(move |graph_weight| &path_weight != graph_weight).map(move |graph_weight| (path_weight, graph_weight)))
        .find(|(path_weight, graph_weight)| path_weight.intersects(graph_weight))
        .map(|(path_weight, graph_weight)| graph_weight.visual_right() - path_weight.visual_left() + spacing) {
        for &idx in idx_path.iter().skip(1) {
            if let Some(weight) = graph.node_weight_mut(idx) {
                weight.set_visual_left(weight.visual_left() + offset);
            }
        }
    }

    idx_path.reverse();

    idx_path
}
