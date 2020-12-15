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

    pub fn accept<V: Visitor>(&self, visitor: &mut V) {
        for node in &self.nodes {
            node.accept(visitor);
        }
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

    pub fn accept<V: Visitor>(&self, visitor: &mut V) {
        for node in &self.nodes {
            node.accept(visitor);
        }
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

pub trait Visitor {
    fn visit_increment_pointer(&mut self, times: usize);
    fn visit_decrement_pointer(&mut self, times: usize);
    fn visit_increment_byte(&mut self, times: usize);
    fn visit_decrement_byte(&mut self, times: usize);
    fn visit_write_byte(&mut self, times: usize);
    fn visit_read_byte(&mut self, times: usize);
    fn visit_loop(&mut self, block: &Block);
}

impl SyntaxNode {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) {
        use SyntaxNode::*;
        match self {
            IncrementPointer { times } => visitor.visit_increment_pointer(*times),
            DecrementPointer { times } => visitor.visit_decrement_pointer(*times),
            IncrementByte { times } => visitor.visit_increment_byte(*times),
            DecrementByte { times } => visitor.visit_decrement_byte(*times),
            WriteByte { times } => visitor.visit_write_byte(*times),
            ReadByte { times } => visitor.visit_read_byte(*times),
            Loop(block) => visitor.visit_loop(&block),
        }
    }
}