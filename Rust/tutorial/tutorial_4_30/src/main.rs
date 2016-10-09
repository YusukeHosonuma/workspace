// 4.30. 関連型

use std::fmt;

trait Graph {
    type N;
    type E;

    fn has_edge(&self, &Self::N, &Self::N) -> bool;
    fn edges(&self, &Self::N) -> Vec<Self::E>;
}

struct Node;
struct Edge;
struct MyGraph;

impl Graph for MyGraph {
    type N = Node;
    type E = Edge;

    fn has_edge(&self, n1: &Node, n2: &Node) -> bool {
        true
    }

    fn edges(&self, n: &Node) -> Vec<Edge> {
        Vec::new()
    }
}

fn distance<G: Graph>(graph: &G, start: &G::N, end: &G::N) -> u32 { 1 }

fn main() {
    let graph = MyGraph;
    let node = Node;
    assert_eq!(true, graph.has_edge(&node, &node));
    graph.edges(&node);

    let graph = MyGraph;
    let obj = Box::new(graph) as Box<Graph<N=Node, E=Edge>>;
}
