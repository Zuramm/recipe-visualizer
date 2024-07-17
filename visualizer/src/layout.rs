use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt::Debug,
    ops::{Add, AddAssign, Sub, SubAssign},
};

use petgraph::{
    algo::{
        bellman_ford::{self, bellman_ford, Paths},
        is_cyclic_directed,
    },
    prelude::*,
    visit::{depth_first_search, Control, DfsEvent, NodeIndexable},
};
use thiserror::Error;

pub trait VisuallySized<Coordinates> {
    fn get_width(&self) -> Coordinates;
    fn get_height(&self) -> Coordinates;
}

pub trait Timed {
    fn get_duration(&self) -> f64 {
        1.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rect<Coordinates> {
    pub x: Coordinates,
    pub y: Coordinates,
    pub width: Coordinates,
    pub height: Coordinates,
}

impl<Coordinates> Rect<Coordinates> {
    pub fn new(x: Coordinates, y: Coordinates, width: Coordinates, height: Coordinates) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
}

impl<Coordinates: Copy> Rect<Coordinates> {
    pub fn top(&self) -> Coordinates {
        self.y
    }

    pub fn left(&self) -> Coordinates {
        self.x
    }
}

impl<Coordinates: Copy + Add<Output = Coordinates>> Rect<Coordinates> {
    pub fn bottom(&self) -> Coordinates {
        self.y + self.height
    }

    pub fn right(&self) -> Coordinates {
        self.x + self.width
    }

    pub fn extend_right_bottom(&self, right: Coordinates, bottom: Coordinates) -> Self {
        Self {
            x: self.x,
            y: self.y,
            width: self.width + right,
            height: self.height + bottom,
        }
    }
}

impl<Coordinates: Copy + Sub<Output = Coordinates>> Rect<Coordinates> {
    pub fn from_sides(
        left: Coordinates,
        top: Coordinates,
        right: Coordinates,
        bottom: Coordinates,
    ) -> Self {
        Self {
            x: left,
            y: top,
            width: right - left,
            height: bottom - top,
        }
    }
}

fn min<T>(a: T, b: T) -> Option<T>
where
    T: PartialOrd,
{
    a.partial_cmp(&b).map(|ord| match ord {
        Ordering::Greater => b,
        _ => a,
    })
}

fn max<T>(a: T, b: T) -> Option<T>
where
    T: PartialOrd,
{
    a.partial_cmp(&b).map(|ord| match ord {
        Ordering::Less => b,
        _ => a,
    })
}

impl<Coordinates: Default + PartialOrd> Rect<Coordinates> {
    pub fn is_valid(&self) -> bool {
        let neutral = Coordinates::default();
        self.width
            .partial_cmp(&neutral)
            .map(|ord| ord.is_ge())
            .unwrap_or(false)
            && self
                .height
                .partial_cmp(&neutral)
                .map(|ord| ord.is_ge())
                .unwrap_or(false)
    }
}

impl<
        Coordinates: Default + Copy + Add<Output = Coordinates> + Sub<Output = Coordinates> + PartialOrd,
    > Rect<Coordinates>
{
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let left = max(self.left(), other.left())?;
        let top = max(self.top(), other.top())?;
        let right = min(self.right(), other.right())?;
        let bottom = min(self.bottom(), other.bottom())?;
        if left < right && top < bottom {
            Some(Self::from_sides(left, top, right, bottom))
        } else {
            None
        }
    }

    pub fn union(&self, other: &Self) -> Option<Self> {
        let left = min(self.left(), other.left())?;
        let top = min(self.top(), other.top())?;
        let right = max(self.right(), other.right())?;
        let bottom = max(self.bottom(), other.bottom())?;
        Some(Self::from_sides(left, top, right, bottom))
    }

    pub fn bounded(rects: &[Self]) -> Option<Self> {
        let mut iter = rects.iter();
        iter.next().map(|first| {
            iter.fold(first.clone(), |bounds, rect| match bounds.union(rect) {
                Some(bounds) => bounds,
                None => bounds.clone(),
            })
        })
    }
}

/// Creates a graph from nodes and edges. The duration of the target node is used for the weight of
/// each edge.
fn create_graph<'a, NodeWeight, Coordinates>(
    nodes: &'a [NodeWeight],
    edges: &[(usize, usize)],
) -> DiGraph<&'a NodeWeight, f64>
where
    NodeWeight: VisuallySized<Coordinates> + Timed,
    f64: From<Coordinates>,
{
    let mut graph = DiGraph::<&NodeWeight, f64>::new();

    // initialize arrays and calculate dimensions
    for (i, node) in nodes.iter().enumerate() {
        let id = graph.add_node(node);
        assert_eq!(i, id.index());
    }

    for &(from, to) in edges.iter() {
        let weight = nodes[to].get_duration();
        let from = graph.from_index(from);
        let to = graph.from_index(to);
        graph.add_edge(from, to, weight);
    }

    graph
}

fn get_leafs_in_predecessors(predecessors: &[Option<NodeIndex>]) -> Vec<NodeIndex> {
    let mut is_leaf = vec![true; predecessors.len()];
    for pred in predecessors.iter() {
        if let Some(pred) = pred {
            is_leaf[pred.index()] = false;
        }
    }
    is_leaf
        .into_iter()
        .enumerate()
        .filter_map(|(i, is_leaf)| {
            if is_leaf {
                Some(NodeIndex::new(i))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
}

fn get_path_from_node_in_predecessors(
    predecessors: &[Option<NodeIndex>],
    node: NodeIndex,
) -> Vec<NodeIndex> {
    let mut path = vec![node];
    while let Some(pred) = predecessors[path.last().expect("paths to not be empty").index()] {
        path.push(pred);
    }
    path.reverse();
    path
}

fn extract_paths_longest_first(path: &bellman_ford::Paths<NodeIndex, f64>) -> Vec<Vec<NodeIndex>> {
    let mut leafs = get_leafs_in_predecessors(&path.predecessors);
    leafs.sort_by(|a, b| f64::total_cmp(&path.distances[a.index()], &path.distances[b.index()]));
    leafs.reverse();

    leafs
        .into_iter()
        .map(|node| get_path_from_node_in_predecessors(&path.predecessors, node))
        .collect::<Vec<_>>()
}

fn get_and_remove_longest_path(
    paths: &mut Paths<NodeIndex<u32>, f64>,
) -> Option<Vec<NodeIndex<u32>>> {
    paths
        .distances
        .iter()
        .zip(paths.predecessors.iter())
        .enumerate()
        .map(|(i, (dist, pred))| (i, dist, pred))
        .filter(|(_, dist, pred)| dist.is_finite() && pred.is_some())
        // NOTE: To prefer nodes, which have been defined first, when the distances are equal we do
        // min_by and reverse the arguments. This effectivly sorts from farthest to closest.
        .min_by(|a, b| b.1.partial_cmp(a.1).unwrap())
        .map(|(leaf, _, _)| leaf)
        .map(|leaf| {
            let mut res = vec![NodeIndex::new(leaf)];
            while let Some(node) = paths.predecessors[res.last().unwrap().index()] {
                res.push(node);
            }

            for node in res.iter() {
                let predecessor_count = paths
                    .predecessors
                    .iter()
                    .filter(|p| match p {
                        Some(p) => p == node,
                        _ => false,
                    })
                    .count();
                if predecessor_count <= 1 {
                    paths.predecessors[node.index()] = None;
                }
            }

            res.reverse();
            res
        })
}

pub fn path_get_not_laid_out_segments<
    NodeWeight: VisuallySized<Coordinates>,
    Coordinates: Default,
>(
    graph: &DiGraph<&NodeWeight, f64>,
    laid_out: &[Option<Rect<Coordinates>>],
    path: &[NodeIndex<u32>],
) -> Vec<Vec<(NodeIndex<u32>, Rect<Coordinates>)>> {
    let mut segments = path.iter().fold(vec![vec![]], |mut arr, &i| {
        if laid_out[i.index()].is_some() {
            if !arr
                .last()
                .expect("`arr` should be initialized with one element")
                .is_empty()
            {
                arr.push(vec![]);
            }
        } else {
            arr.last_mut()
                .expect("`arr` should be initialized with one element")
                .push((
                    i,
                    Rect {
                        x: Coordinates::default(),
                        y: Coordinates::default(),
                        width: graph[i].get_width(),
                        height: graph[i].get_height(),
                    },
                ));
        }
        arr
    });
    if segments
        .last()
        .expect("`segments` should be initialized with at least one element")
        .is_empty()
    {
        segments
            .last_mut()
            .expect("`segments` should be initialized with at least one element")
            .pop();
    }

    segments
}

fn push_node_children<NodeWeight, Coordinates: Copy + Add<Output = Coordinates> + AddAssign>(
    graph: &DiGraph<&NodeWeight, f64>,
    laid_out: &mut [Option<Rect<Coordinates>>],
    i: NodeIndex,
    amount: Coordinates,
) {
    depth_first_search(&graph, Some(i), |event| {
        if let DfsEvent::Discover(node, _) = event {
            if let Some(rect) = laid_out[node.index()].as_mut() {
                rect.y += amount;
            }
        }
        Control::<()>::Continue
    });
}

#[derive(Error, Debug)]
pub enum LayoutError {
    #[error("graph contains a cycle")]
    GraphIsCyclic,
    #[error("graph is disconnected")]
    GraphIsDisconnected,
    #[error("graph has multiple roots ({roots:?})]")]
    GraphHasMultipleRoots { roots: Vec<usize> },
}

pub fn layout<NodeWeight, Coordinates>(
    nodes: &[NodeWeight],
    edges: &[(usize, usize)],
    spacing_x: Coordinates,
    spacing_y: Coordinates,
) -> Result<Vec<Rect<Coordinates>>, LayoutError>
where
    NodeWeight: VisuallySized<Coordinates> + Timed,
    Coordinates: Add<Output = Coordinates>
        + AddAssign
        + Sub<Output = Coordinates>
        + SubAssign
        + PartialOrd
        + Default
        + Copy
        + Debug,
    f64: From<Coordinates>,
{
    if nodes.is_empty() {
        return Ok(Vec::new());
    }

    if nodes.len() == 1 {
        return Ok(vec![Rect {
            x: Coordinates::default(),
            y: Coordinates::default(),
            width: nodes[0].get_width(),
            height: nodes[0].get_height(),
        }]);
    }

    let graph = create_graph(nodes, edges);

    // Ensure that the graph has no cycles
    if is_cyclic_directed(&graph) {
        return Err(LayoutError::GraphIsCyclic);
    }

    // Get the single root of the graph and return an error if there are multitle.
    let root = graph
        .node_indices()
        .filter(|&id| {
            graph
                .neighbors_directed(id, petgraph::Direction::Incoming)
                .next()
                .is_none()
        })
        .collect::<Vec<_>>();
    if root.len() > 1 {
        return Err(LayoutError::GraphHasMultipleRoots {
            roots: root.into_iter().map(NodeIndex::index).collect(),
        });
    }
    let root = root[0];

    // Run the bellman-ford algorithm on the graph and update the distances such that each distance
    // is furthest reachable from the current node. This allows for a greedy aproach when layouting
    // the graph.
    let paths_from_root = bellman_ford(&graph, root).expect("graph should not be cyclic");
    let furthest_distances = {
        let mut paths = paths_from_root.clone();
        let mut to_visit = paths
            .distances
            .iter()
            .copied()
            .enumerate()
            .collect::<Vec<_>>();
        to_visit.sort_by(|a, b| f64::total_cmp(&b.1, &a.1));

        for (i, dist) in to_visit {
            let mut i = i;
            while let Some(j) = paths.predecessors[i] {
                i = j.index();
                paths.distances[i] = paths.distances[i].max(dist);
            }
        }

        paths.distances
    };

    // Prepare the layout and initialze with the root at Coordinates (0, 0).
    let mut laid_out: Vec<Option<Rect<Coordinates>>> = vec![None; nodes.len()];
    laid_out[root.index()] = Some(Rect {
        x: Coordinates::default(),
        y: Coordinates::default(),
        width: graph[root].get_width(),
        height: graph[root].get_height(),
    });

    // In a depth first search, to layout each node while checking for overlaps and pushing nodes to
    // resolve the oevrlap.
    let mut just_laid_out = VecDeque::from([root]);

    while let Some(next) = just_laid_out.pop_front() {
        // Get the successors for the current node and sort them using their assigned distance,
        // which contains the furthest distance. In other words nodes along the longest path are on
        // the left and nodes olong the shortest path are on the right.
        let mut successors = graph
            .edges_directed(next, petgraph::Direction::Outgoing)
            .map(|i| i.target())
            .filter(|i| laid_out[i.index()].is_none())
            .collect::<Vec<_>>();
        // NOTE: Sort needs to be stable, so that if the distances are equal the first defined is
        // preferred.
        // NOTE: It is sorted ascending and then reversed, because total_cmp returns b argument if
        // a and b are equal.
        successors.sort_by(|a, b| {
            f64::total_cmp(
                &furthest_distances[a.index()],
                &furthest_distances[b.index()],
            )
        });
        successors.reverse();

        // Layout the beneath the current node and and next to each other.
        let mut rect = laid_out[next.index()]
            .as_ref()
            .expect("only laid out nodes are in the queue");
        let mut x = rect.x;
        for i in successors.iter().copied() {
            // Assign naive position.
            let srect = Rect {
                x,
                y: rect.bottom() + spacing_y,
                width: graph[i].get_width(),
                height: graph[i].get_height(),
            };
            // Commit the layout and prepare variables for the next iteration.
            x += srect.width + spacing_x;
            laid_out[i.index()] = Some(srect);
            just_laid_out.push_back(i);
            rect = laid_out[next.index()]
                .as_ref()
                .expect("only laid out nodes are in the queue");
        }
    }

    // Remove the Options from the layout and verify all nodes are laid out.
    let mut layout = laid_out.into_iter().flatten().collect::<Vec<_>>();
    assert!(layout.len() == nodes.len(), "all nodes are laid out");

    let mut paths = extract_paths_longest_first(&paths_from_root);

    let main_path = paths.remove(0);

    for path in paths {
        for i in path {
            // Push all laid out children of the current node up the resolve the overlap. Repeat
            // until the is no overlap.
            while let Some((j, _overlap)) = layout
                .iter()
                .enumerate()
                .filter(|(j, _)| i.index() != *j)
                .flat_map(|(j, r)| {
                    layout[i.index()]
                        .intersection(&r.extend_right_bottom(Coordinates::default(), spacing_y))
                        .map(|overlap| (j, overlap))
                })
                .max_by(|a, b| a.1.height.partial_cmp(&b.1.height).unwrap())
            {
                let mut shift = i;
                while !main_path.contains(&shift) {
                    shift = paths_from_root.predecessors[shift.index()]
                        .expect("this path to be connected with `main_path`");
                }
                let amount = layout[i.index()].y - (layout[j].bottom() + spacing_y);
                println!("{}", shift.index());
                depth_first_search(&graph, Some(shift), |event| {
                    if let DfsEvent::Discover(node, _) = event {
                        layout[node.index()].y -= amount;
                    }
                    Control::<()>::Continue
                });
            }
        }
    }

    // Invert the y axis, such that the root is at the bottom and no node has negative coordinates.
    if let Some(bounds) = Rect::bounded(&layout) {
        for rect in layout.iter_mut() {
            rect.y = bounds.height - rect.bottom();
        }
    }

    Ok(layout)
}

pub fn layout_proportional<NodeWeight, Coordinates>(
    nodes: &[NodeWeight],
    edges: &[(usize, usize)],
    spacing: Coordinates,
) -> Result<Vec<Rect<Coordinates>>, LayoutError>
where
    NodeWeight: VisuallySized<Coordinates> + Timed,
    Coordinates: Add<Output = Coordinates>
        + AddAssign
        + Sub<Output = Coordinates>
        + SubAssign
        + PartialOrd
        + Default
        + Copy
        + Debug,
    f64: From<Coordinates>,
{
    if nodes.is_empty() {
        return Ok(Vec::new());
    }

    let graph = create_graph(nodes, edges);

    if is_cyclic_directed(&graph) {
        return Err(LayoutError::GraphIsCyclic);
    }
    // and connected
    let root = graph
        .node_indices()
        .filter(|&id| {
            graph
                .neighbors_directed(id, petgraph::Direction::Incoming)
                .next()
                .is_none()
        })
        .collect::<Vec<_>>();
    if root.len() > 1 {
        return Err(LayoutError::GraphHasMultipleRoots {
            roots: root.into_iter().map(NodeIndex::index).collect(),
        });
    }
    let root = root[0];

    let mut paths_from_root = bellman_ford(&graph, root).expect("graph should not be cyclic");

    let mut laid_out: Vec<Option<Rect<Coordinates>>> = vec![None; nodes.len()];
    laid_out[root.index()] = Some(Rect {
        x: Coordinates::default(),
        y: Coordinates::default(),
        width: graph[root].get_width(),
        height: graph[root].get_height(),
    });

    while let Some(path) = get_and_remove_longest_path(&mut paths_from_root) {
        // get all subsequences of the path, in which no node is laid out
        let mut segments = path_get_not_laid_out_segments(&graph, &laid_out, &path);

        // start with naive layout
        let mut x = Coordinates::default();
        let mut y = Coordinates::default();
        for i in path.iter().copied() {
            // NOTE: Every node in the path is either laid out or in a segment.
            //       Thereby changing the y in both cases will work for every node in the path.
            if let Some(rect) = laid_out[i.index()].clone() {
                if y > rect.top() {
                    let diff = y - rect.top();
                    push_node_children(&graph, &mut laid_out, i, diff);
                }

                x = rect.left();
                y = rect.top();

                y += nodes[i.index()].get_height() + spacing;
            }

            if let Some((_, sr)) = segments.iter_mut().flatten().find(|(si, _)| *si == i) {
                sr.x = x;
                sr.y = y;

                y += nodes[i.index()].get_height() + spacing;
            }
        }

        // resolve collisions
        for mut segment in segments.into_iter() {
            loop {
                let overlap = segment
                    .iter()
                    .flat_map(|(si, sr)| {
                        laid_out
                            .iter()
                            .enumerate()
                            .filter(move |(i, _)| *i != si.index())
                            .flat_map(|(_, rect)| rect)
                            .filter_map(move |rect| rect.intersection(sr))
                    })
                    .max_by(|a, b| a.height.partial_cmp(&b.height).unwrap_or(Ordering::Less));

                if let Some(overlap) = overlap {
                    for (_, sr) in segment.iter_mut() {
                        sr.x += overlap.width + spacing;
                    }
                } else {
                    break;
                }
            }

            for (si, sr) in segment.into_iter() {
                laid_out[si.index()] = Some(sr);
            }
        }
    }

    let mut res = laid_out.into_iter().flatten().collect::<Vec<_>>();
    assert!(res.len() == nodes.len(), "all nodes are laid out");

    if let Some(bounds) = Rect::bounded(&res) {
        for rect in res.iter_mut() {
            rect.y = bounds.height - rect.bottom();
        }
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy)]
    struct Size(u32, u32);

    impl VisuallySized<u32> for Size {
        fn get_width(&self) -> u32 {
            self.0
        }

        fn get_height(&self) -> u32 {
            self.1
        }
    }

    impl Timed for Size {
        fn get_duration(&self) -> f64 {
            self.1 as f64
        }
    }

    impl Size {
        fn positioned(self, x: u32, y: u32) -> Rect<u32> {
            Rect {
                x,
                y,
                width: self.0,
                height: self.1,
            }
        }
    }

    const NODE: Size = Size(40, 30);
    const SPACING: u32 = 10;

    fn position(positions: Vec<(u32, u32)>) -> Vec<Rect<u32>> {
        positions
            .into_iter()
            .map(|(x, y)| NODE.positioned(x * (NODE.0 + SPACING), y * (NODE.1 + SPACING)))
            .collect()
    }

    macro_rules! assert_graph {
        ($a:expr; is above $b:expr; in $result:expr) => {
            assert!(
                $result[$a].bottom() + SPACING == $result[$b].top(),
                "node {} ({:?}) is above {} ({:?})",
                $a,
                $result[$a],
                $b,
                $result[$b]
            )
        };
        ($($n:expr),+; are in column $col:expr; in $result:expr) => {
            {$(
                assert_eq!(
                    $result[$n].x, $col,
                    "node {} ({:?}) is in column {}",
                    $n, $result[$n], $col
                )
            );+}
        };
    }

    #[test]
    fn zero_nodes() -> Result<(), LayoutError> {
        let nodes: Vec<Size> = vec![];
        let edges = vec![];

        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        assert_eq!(result.len(), 0);
        Ok(())
    }

    #[test]
    fn one_node() -> Result<(), LayoutError> {
        // 0
        let nodes = vec![NODE];
        let edges = vec![];

        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 1);
        Ok(())
    }

    #[test]
    fn two_nodes() -> Result<(), LayoutError> {
        // 1
        // |
        // 0
        let nodes = vec![NODE; 2];
        let edges = vec![(0, 1)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 2);
        assert_graph!(1; is above 0; in result);
        assert_graph!(0, 1; are in column 0; in result);
        Ok(())
    }

    #[test]
    fn simple_branch() -> Result<(), LayoutError> {
        // 1 2
        // |/
        // 0
        let nodes = vec![NODE; 3];
        let edges = vec![(0, 1), (0, 2)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 3);
        assert_graph!(1; is above 0; in result);
        assert_graph!(2; is above 0; in result);
        assert_graph!(0, 1; are in column 0; in result);
        assert_graph!(2; are in column 50; in result);
        Ok(())
    }

    #[test]
    fn simple_branch_with_one_arm_longer() -> Result<(), LayoutError> {
        // 2
        // |
        // 1 3
        // |/
        // 0
        let nodes = vec![NODE; 4];
        let edges = vec![(0, 1), (1, 2), (0, 3)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 4);
        assert_graph!(1; is above 0; in result);
        assert_graph!(2; is above 1; in result);
        assert_graph!(3; is above 0; in result);
        assert_graph!(0, 1, 2; are in column 0; in result);
        assert_graph!(3; are in column 50; in result);
        Ok(())
    }

    #[test]
    fn unequal_children() -> Result<(), LayoutError> {
        // 1 2
        // |/
        // 0
        let nodes = vec![NODE, NODE, Size(40, 20)];
        let edges = vec![(0, 1), (0, 2)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 3);
        assert_graph!(1; is above 0; in result);
        assert_graph!(2; is above 0; in result);
        assert_graph!(0, 1; are in column 0; in result);
        assert_graph!(2; are in column 50; in result);
        Ok(())
    }

    // When placing nodes an tightly as possible, nodes 3 and 4 overlap. This overlap should be
    // resolved by pushing nodes 1, 2, and 3 upwards.
    #[test]
    fn vertical_overlap() -> Result<(), LayoutError> {
        // 2 3
        // |/
        // 1 4
        // |/
        // 0
        let nodes = vec![NODE, Size(40, 20), NODE, NODE, NODE];
        let edges = vec![(0, 1), (1, 2), (1, 3), (0, 4)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("result: {result:?}");
        assert_eq!(result.len(), 5);
        assert_graph!(2; is above 1; in result);
        assert_graph!(3; is above 1; in result);
        assert_graph!(4; is above 0; in result);
        assert_graph!(0, 1, 2; are in column 0; in result);
        assert_graph!(4, 3; are in column 50; in result);
        Ok(())
    }

    #[test]
    fn massive_child() -> Result<(), LayoutError> {
        //   2
        //   |
        // 1 3
        // |/
        // 0
        let nodes = [NODE, Size(40, 200), NODE, NODE];
        let edges = [(0, 3), (3, 2), (0, 1)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("{result:#?}");
        assert_eq!(result.len(), 4);
        assert_graph!(3; is above 0; in result);
        assert_graph!(2; is above 3; in result);
        assert_graph!(1; is above 0; in result);
        assert_graph!(0, 1; are in column 0; in result);
        assert_graph!(3, 2; are in column 50; in result);
        Ok(())
    }

    #[test]
    fn toscana1() -> Result<(), LayoutError> {
        // 7
        // |
        // 6
        // |
        // 5 9
        // | |
        // 4 8
        // |/
        // 3 10
        // |/
        // 2 11
        // |/
        // 1 12
        // |/
        // 0
        let nodes = [
            Size(40, 60),
            Size(40, 2100),
            Size(40, 60),
            Size(40, 60),
            Size(40, 60),
            Size(40, 600),
            Size(40, 60),
            Size(40, 300),
            Size(40, 60),
            Size(40, 60),
            Size(40, 300),
            Size(40, 600),
            Size(40, 600),
        ];
        let edges = [
            (0, 1),
            (1, 2),
            (2, 3),
            (3, 4),
            (4, 5),
            (5, 6),
            (6, 7),
            (3, 8),
            (8, 9),
            (2, 10),
            (1, 11),
            (0, 12),
        ];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("result: {result:?}");
        // assert_eq!(result.len(), 5);
        // assert_graph!(1; is above 0; in result);
        // assert_graph!(2; is above 0; in result);
        assert_graph!(0, 1, 2, 3,4,5,6,7; are in column 0; in result);
        assert_graph!(12, 11, 10, 8, 9; are in column 50; in result);
        Ok(())
    }

    #[test]
    fn toscana2() -> Result<(), LayoutError> {
        // 4
        // |
        // 3
        // |
        // 2 5
        // |/
        // 1 6
        // |/
        // 0
        let nodes = [
            Size(40, 60),  // 0
            Size(40, 60),  // 1
            Size(40, 60),  // 2
            Size(40, 60),  // 3
            Size(40, 600), // 4
            Size(40, 300), // 5
            Size(40, 600), // 6
        ];
        let edges = [(0, 1), (1, 2), (2, 3), (3, 4), (1, 5), (0, 6)];
        let result = layout(&nodes, &edges, SPACING, SPACING)?;
        println!("result: {result:?}");
        // assert_eq!(result.len(), 5);
        // assert_graph!(1; is above 0; in result);
        // assert_graph!(2; is above 0; in result);
        assert_graph!(0, 1, 2, 3, 4; are in column 0; in result);
        assert_graph!(5, 6; are in column 50; in result);
        Ok(())
    }
}
