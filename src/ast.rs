#[derive(Debug)]
pub struct Program {
    nodes: Vec<SyntaxNode>,
}

impl Program {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn add_node(&mut self, node: SyntaxNode) {
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> &[SyntaxNode] {
        &self.nodes
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block {
    nodes: Vec<SyntaxNode>
}

impl Block {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn add_node(&mut self, node: SyntaxNode) {
        self.nodes.push(node);
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SyntaxNode {
    IncrementPointer { times: usize },
    DecrementPointer { times: usize },
    IncrementByte { times: usize },
    DecrementByte { times: usize },
    WriteByte {times: usize },
    ReadByte { times: usize },
    Loop(Block),
}