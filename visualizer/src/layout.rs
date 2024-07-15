use core::fmt::Debug;
use std::{
    cmp::Ordering,
    ops::{Add, AddAssign, Sub, SubAssign},
};

use petgraph::{
    algo::{
        bellman_ford::{bellman_ford, Paths},
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
        if left <= right && top <= bottom {
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
        let weight = nodes[from].get_duration();
        let from = graph.from_index(from);
        let to = graph.from_index(to);
        graph.add_edge(from, to, weight);
    }

    graph
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

        let mut x = Coordinates::default();
        let mut y = Coordinates::default();
        for i in path.iter().copied() {
            // NOTE: Every node in the path is either laid out or in a segment.
            //       Thereby changing the y in both cases will work for every node in the path.
            if let Some(rect) = laid_out[i.index()].clone() {
                if y > rect.top() {
                    let diff = y - rect.top();
                    depth_first_search(&graph, Some(i), |event| {
                        if let DfsEvent::Discover(node, _) = event {
                            if let Some(rect) = laid_out[node.index()].as_mut() {
                                rect.y += diff;
                            }
                        }
                        Control::<()>::Continue
                    });
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
                    .max_by(|a, b| a.width.partial_cmp(&b.width).unwrap_or(Ordering::Less));

                if let Some(overlap) = overlap {
                    for (_si, sr) in segment.iter_mut() {
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
    use pretty_assertions::assert_eq;

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

    impl Timed for Size {}

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

    // TODO: Test for properies of the result not the exact result, so it will be easier to
    // understand what failed. For example test that nodes are behind eachother on the same level
    // or column, etc.

    #[test]
    fn zero_nodes() {
        let nodes: Vec<Size> = vec![];
        let edges = vec![];
        let result = match layout(&nodes, &edges, SPACING) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error);
                return;
            }
        };
        assert_eq!(result, position(vec![]),);
    }

    #[test]
    fn one_node() {
        // 0
        let nodes = vec![NODE; 1];
        let edges = vec![];
        let result = match layout(&nodes, &edges, SPACING) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error);
                return;
            }
        };
        assert_eq!(result, position(vec![(0, 0)]),);
    }

    #[test]
    fn two_nodes() {
        // 1
        // |
        // 0
        let nodes = vec![NODE; 2];
        let edges = vec![(0, 1)];
        let result = match layout(&nodes, &edges, SPACING) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error);
                return;
            }
        };
        assert_eq!(result, position(vec![(0, 1), (0, 0)]),);
    }

    #[test]
    fn three_nodes() {
        // 1 2
        // |/
        // 0
        let nodes = vec![NODE; 3];
        let edges = vec![(0, 1), (0, 2)];
        let result = match layout(&nodes, &edges, SPACING) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error);
                return;
            }
        };
        assert_eq!(result, position(vec![(0, 1), (0, 0), (1, 0)]),);
    }

    #[test]
    fn four_nodes() {
        // 2
        // |
        // 1 3
        // |/
        // 0
        let nodes = vec![NODE; 4];
        let edges = vec![(0, 1), (1, 2), (0, 3)];
        let result = match layout(&nodes, &edges, SPACING) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error);
                return;
            }
        };
        assert_eq!(result, position(vec![(0, 2), (0, 1), (0, 0), (1, 1)]),);
    }
}
