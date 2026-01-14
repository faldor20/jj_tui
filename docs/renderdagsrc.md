column.rs
```rs 
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Column<N> {
    Empty,
    Blocked,
    Reserved(N),
    Ancestor(N),
    Parent(N),
}

impl<N> Column<N>
where
    N: Clone,
{
    pub(crate) fn matches(&self, n: &N) -> bool
    where
        N: Eq,
    {
        match self {
            Column::Empty | Column::Blocked => false,
            Column::Reserved(o) => n == o,
            Column::Ancestor(o) => n == o,
            Column::Parent(o) => n == o,
        }
    }

    fn variant(&self) -> usize {
        match self {
            Column::Empty => 0,
            Column::Blocked => 1,
            Column::Reserved(_) => 2,
            Column::Ancestor(_) => 3,
            Column::Parent(_) => 4,
        }
    }

    pub(crate) fn merge(&mut self, other: &Column<N>) {
        if other.variant() > self.variant() {
            *self = other.clone();
        }
    }

    fn reset(&mut self) {
        match self {
            Column::Blocked => *self = Column::Empty,
            _ => {}
        }
    }
}

pub(crate) trait ColumnsExt<N> {
    fn find(&self, node: &N) -> Option<usize>;
    fn find_empty(&self, index: usize) -> Option<usize>;
    fn first_empty(&self) -> Option<usize>;
    fn new_empty(&mut self) -> usize;
    fn reset(&mut self);
}

impl<N> ColumnsExt<N> for Vec<Column<N>>
where
    N: Clone + Eq,
{
    fn find(&self, node: &N) -> Option<usize> {
        for (index, column) in self.iter().enumerate() {
            if column.matches(node) {
                return Some(index);
            }
        }
        None
    }

    fn find_empty(&self, index: usize) -> Option<usize> {
        if self.get(index) == Some(&Column::Empty) {
            return Some(index);
        }
        self.first_empty()
    }

    fn first_empty(&self) -> Option<usize> {
        for (i, column) in self.iter().enumerate() {
            if *column == Column::Empty {
                return Some(i);
            }
        }
        None
    }

    fn new_empty(&mut self) -> usize {
        self.push(Column::Empty);
        self.len() - 1
    }

    fn reset(&mut self) {
        for column in self.iter_mut() {
            column.reset();
        }
        while let Some(Column::Empty) = self.last() {
            self.pop();
        }
    }
}
```

box_drawing.rs
```rs

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::marker::PhantomData;

use super::output::OutputRendererOptions;
use super::render::Ancestor;
use super::render::GraphRow;
use super::render::LinkLine;
use super::render::NodeLine;
use super::render::PadLine;
use super::render::Renderer;
use crate::pad::pad_lines;

mod glyph {
    pub(super) const SPACE: usize = 0;
    pub(super) const HORIZONTAL: usize = 1;
    pub(super) const PARENT: usize = 2;
    pub(super) const ANCESTOR: usize = 3;
    pub(super) const MERGE_LEFT: usize = 4;
    pub(super) const MERGE_RIGHT: usize = 5;
    pub(super) const MERGE_BOTH: usize = 6;
    pub(super) const FORK_LEFT: usize = 7;
    pub(super) const FORK_RIGHT: usize = 8;
    pub(super) const FORK_BOTH: usize = 9;
    pub(super) const JOIN_LEFT: usize = 10;
    pub(super) const JOIN_RIGHT: usize = 11;
    pub(super) const JOIN_BOTH: usize = 12;
    pub(super) const TERMINATION: usize = 13;
    pub(super) const COUNT: usize = 14;
}

const SQUARE_GLYPHS: [&str; glyph::COUNT] = [
    "  ", "──", "│ ", "· ", "┘ ", "└─", "┴─", "┐ ", "┌─", "┬─", "┤ ", "├─", "┼─", "~ ",
];

const CURVED_GLYPHS: [&str; glyph::COUNT] = [
    "  ", "──", "│ ", "╷ ", "╯ ", "╰─", "┴─", "╮ ", "╭─", "┬─", "┤ ", "├─", "┼─", "~ ",
];

const DEC_GLYPHS: [&str; glyph::COUNT] = [
    "  ",
    "\x1B(0qq\x1B(B",
    "\x1B(0x \x1B(B",
    "\x1B(0~ \x1B(B",
    "\x1B(0j \x1B(B",
    "\x1B(0mq\x1B(B",
    "\x1B(0vq\x1B(B",
    "\x1B(0k \x1B(B",
    "\x1B(0lq\x1B(B",
    "\x1B(0wq\x1B(B",
    "\x1B(0u \x1B(B",
    "\x1B(0tq\x1B(B",
    "\x1B(0nq\x1B(B",
    "~ ",
];

impl PadLine {
    fn to_glyph(&self) -> usize {
        match *self {
            PadLine::Parent => glyph::PARENT,
            PadLine::Ancestor => glyph::ANCESTOR,
            PadLine::Blank => glyph::SPACE,
        }
    }
}

pub struct BoxDrawingRenderer<N, R>
where
    R: Renderer<N, Output = GraphRow<N>> + Sized,
{
    inner: R,
    options: OutputRendererOptions,
    extra_pad_line: Option<String>,
    glyphs: &'static [&'static str; glyph::COUNT],
    _phantom: PhantomData<N>,
}

impl<N, R> BoxDrawingRenderer<N, R>
where
    R: Renderer<N, Output = GraphRow<N>> + Sized,
{
    pub(crate) fn new(inner: R, options: OutputRendererOptions) -> Self {
        BoxDrawingRenderer {
            inner,
            options,
            extra_pad_line: None,
            glyphs: &CURVED_GLYPHS,
            _phantom: PhantomData,
        }
    }

    pub fn with_square_glyphs(mut self) -> Self {
        self.glyphs = &SQUARE_GLYPHS;
        self
    }

    pub fn with_dec_graphics_glyphs(mut self) -> Self {
        self.glyphs = &DEC_GLYPHS;
        self
    }
}

impl<N, R> Renderer<N> for BoxDrawingRenderer<N, R>
where
    N: Clone + Eq,
    R: Renderer<N, Output = GraphRow<N>> + Sized,
{
    type Output = String;

    fn width(&self, node: Option<&N>, parents: Option<&Vec<Ancestor<N>>>) -> u64 {
        self.inner
            .width(node, parents)
            .saturating_mul(2)
            .saturating_add(1)
    }

    fn reserve(&mut self, node: N) {
        self.inner.reserve(node);
    }

    fn next_row(
        &mut self,
        node: N,
        parents: Vec<Ancestor<N>>,
        glyph: String,
        message: String,
    ) -> String {
        let glyphs = self.glyphs;
        let line = self.inner.next_row(node, parents, glyph, message);
        let mut out = String::new();
        let mut message_lines = pad_lines(line.message.lines(), self.options.min_row_height);
        let mut need_extra_pad_line = false;

        // Render the previous extra pad line
        if let Some(extra_pad_line) = self.extra_pad_line.take() {
            out.push_str(extra_pad_line.trim_end());
            out.push('\n');
        }

        // Render the nodeline
        let mut node_line = String::new();
        for entry in line.node_line.iter() {
            match entry {
                NodeLine::Node => {
                    node_line.push_str(&line.glyph);
                    node_line.push(' ');
                }
                NodeLine::Parent => node_line.push_str(glyphs[glyph::PARENT]),
                NodeLine::Ancestor => node_line.push_str(glyphs[glyph::ANCESTOR]),
                NodeLine::Blank => node_line.push_str(glyphs[glyph::SPACE]),
            }
        }
        if let Some(msg) = message_lines.next() {
            node_line.push(' ');
            node_line.push_str(msg);
        }
        out.push_str(node_line.trim_end());
        out.push('\n');

        // Render the link line
        #[allow(clippy::if_same_then_else)]
        if let Some(link_row) = line.link_line {
            let mut link_line = String::new();
            for cur in link_row.iter() {
                if cur.intersects(LinkLine::HORIZONTAL) {
                    if cur.intersects(LinkLine::CHILD) {
                        link_line.push_str(glyphs[glyph::JOIN_BOTH]);
                    } else if cur.intersects(LinkLine::ANY_FORK)
                        && cur.intersects(LinkLine::ANY_MERGE)
                    {
                        link_line.push_str(glyphs[glyph::JOIN_BOTH]);
                    } else if cur.intersects(LinkLine::ANY_FORK)
                        && cur.intersects(LinkLine::VERT_PARENT)
                        && !line.merge
                    {
                        link_line.push_str(glyphs[glyph::JOIN_BOTH]);
                    } else if cur.intersects(LinkLine::ANY_FORK) {
                        link_line.push_str(glyphs[glyph::FORK_BOTH]);
                    } else if cur.intersects(LinkLine::ANY_MERGE) {
                        link_line.push_str(glyphs[glyph::MERGE_BOTH]);
                    } else {
                        link_line.push_str(glyphs[glyph::HORIZONTAL]);
                    }
                } else if cur.intersects(LinkLine::VERT_PARENT) && !line.merge {
                    let left = cur.intersects(LinkLine::LEFT_MERGE | LinkLine::LEFT_FORK);
                    let right = cur.intersects(LinkLine::RIGHT_MERGE | LinkLine::RIGHT_FORK);
                    match (left, right) {
                        (true, true) => link_line.push_str(glyphs[glyph::JOIN_BOTH]),
                        (true, false) => link_line.push_str(glyphs[glyph::JOIN_LEFT]),
                        (false, true) => link_line.push_str(glyphs[glyph::JOIN_RIGHT]),
                        (false, false) => link_line.push_str(glyphs[glyph::PARENT]),
                    }
                } else if cur.intersects(LinkLine::VERT_PARENT | LinkLine::VERT_ANCESTOR)
                    && !cur.intersects(LinkLine::LEFT_FORK | LinkLine::RIGHT_FORK)
                {
                    let left = cur.intersects(LinkLine::LEFT_MERGE);
                    let right = cur.intersects(LinkLine::RIGHT_MERGE);
                    match (left, right) {
                        (true, true) => link_line.push_str(glyphs[glyph::JOIN_BOTH]),
                        (true, false) => link_line.push_str(glyphs[glyph::JOIN_LEFT]),
                        (false, true) => link_line.push_str(glyphs[glyph::JOIN_RIGHT]),
                        (false, false) => {
                            if cur.intersects(LinkLine::VERT_ANCESTOR) {
                                link_line.push_str(glyphs[glyph::ANCESTOR]);
                            } else {
                                link_line.push_str(glyphs[glyph::PARENT]);
                            }
                        }
                    }
                } else if cur.intersects(LinkLine::LEFT_FORK)
                    && cur.intersects(LinkLine::LEFT_MERGE | LinkLine::CHILD)
                {
                    link_line.push_str(glyphs[glyph::JOIN_LEFT]);
                } else if cur.intersects(LinkLine::RIGHT_FORK)
                    && cur.intersects(LinkLine::RIGHT_MERGE | LinkLine::CHILD)
                {
                    link_line.push_str(glyphs[glyph::JOIN_RIGHT]);
                } else if cur.intersects(LinkLine::LEFT_MERGE)
                    && cur.intersects(LinkLine::RIGHT_MERGE)
                {
                    link_line.push_str(glyphs[glyph::MERGE_BOTH]);
                } else if cur.intersects(LinkLine::LEFT_FORK)
                    && cur.intersects(LinkLine::RIGHT_FORK)
                {
                    link_line.push_str(glyphs[glyph::FORK_BOTH]);
                } else if cur.intersects(LinkLine::LEFT_FORK) {
                    link_line.push_str(glyphs[glyph::FORK_LEFT]);
                } else if cur.intersects(LinkLine::LEFT_MERGE) {
                    link_line.push_str(glyphs[glyph::MERGE_LEFT]);
                } else if cur.intersects(LinkLine::RIGHT_FORK) {
                    link_line.push_str(glyphs[glyph::FORK_RIGHT]);
                } else if cur.intersects(LinkLine::RIGHT_MERGE) {
                    link_line.push_str(glyphs[glyph::MERGE_RIGHT]);
                } else {
                    link_line.push_str(glyphs[glyph::SPACE]);
                }
            }
            if let Some(msg) = message_lines.next() {
                link_line.push(' ');
                link_line.push_str(msg);
            }
            out.push_str(link_line.trim_end());
            out.push('\n');
        }

        // Render the term line
        if let Some(term_row) = line.term_line {
            let term_strs = [glyphs[glyph::PARENT], glyphs[glyph::TERMINATION]];
            for term_str in term_strs.iter() {
                let mut term_line = String::new();
                for (i, term) in term_row.iter().enumerate() {
                    if *term {
                        term_line.push_str(term_str);
                    } else {
                        term_line.push_str(glyphs[line.pad_lines[i].to_glyph()]);
                    }
                }
                if let Some(msg) = message_lines.next() {
                    term_line.push(' ');
                    term_line.push_str(msg);
                }
                out.push_str(term_line.trim_end());
                out.push('\n');
            }
            need_extra_pad_line = true;
        }

        let mut base_pad_line = String::new();
        for entry in line.pad_lines.iter() {
            base_pad_line.push_str(glyphs[entry.to_glyph()]);
        }

        // Render any pad lines
        for msg in message_lines {
            let mut pad_line = base_pad_line.clone();
            pad_line.push(' ');
            pad_line.push_str(msg);
            out.push_str(pad_line.trim_end());
            out.push('\n');
            need_extra_pad_line = false;
        }

        if need_extra_pad_line {
            self.extra_pad_line = Some(base_pad_line);
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_fixtures;
    use super::super::test_fixtures::TestFixture;
    use super::super::test_utils::render_string;
    use super::super::test_utils::render_string_with_order;
    use crate::GraphRowRenderer;

    fn render(fixture: &TestFixture) -> String {
        let mut renderer = GraphRowRenderer::new().output().build_box_drawing();
        render_string(fixture, &mut renderer)
    }

    #[test]
    fn basic() {
        assert_eq!(
            render(&test_fixtures::BASIC),
            r#"
            o  C
            │
            o  B
            │
            o  A"#
        );
    }

    #[test]
    fn branches_and_merges() {
        assert_eq!(
            render(&test_fixtures::BRANCHES_AND_MERGES),
            r#"
            o  W
            │
            o    V
            ├─╮
            │ o    U
            │ ├─╮
            │ │ o  T
            │ │ │
            │ o │  S
            │   │
            o   │  R
            │   │
            o   │  Q
            ├─╮ │
            │ o │    P
            │ ├───╮
            │ │ │ o  O
            │ │ │ │
            │ │ │ o    N
            │ │ │ ├─╮
            │ o │ │ │  M
            │ │ │ │ │
            │ o │ │ │  L
            │ │ │ │ │
            o │ │ │ │  K
            ├───────╯
            o │ │ │  J
            │ │ │ │
            o │ │ │  I
            ├─╯ │ │
            o   │ │  H
            │   │ │
            o   │ │  G
            ├─────╮
            │   │ o  F
            │   ├─╯
            │   o  E
            │   │
            o   │  D
            │   │
            o   │  C
            ├───╯
            o  B
            │
            o  A"#
        );
    }

    #[test]
    fn octopus_branch_and_merge() {
        assert_eq!(
            render(&test_fixtures::OCTOPUS_BRANCH_AND_MERGE),
            r#"
            o      J
            ├─┬─╮
            │ │ o  I
            │ │ │
            │ o │      H
            ╭─┼─┬─┬─╮
            │ │ │ │ o  G
            │ │ │ │ │
            │ │ │ o │  E
            │ │ │ ├─╯
            │ │ o │  D
            │ │ ├─╮
            │ o │ │  C
            │ ├───╯
            o │ │  F
            ├─╯ │
            o   │  B
            ├───╯
            o  A"#
        );
    }

    #[test]
    fn reserved_column() {
        assert_eq!(
            render(&test_fixtures::RESERVED_COLUMN),
            r#"
              o  Z
              │
              o  Y
              │
              o  X
            ╭─╯
            │ o  W
            ├─╯
            o  G
            │
            o    F
            ├─╮
            │ o  E
            │ │
            │ o  D
            │
            o  C
            │
            o  B
            │
            o  A"#
        );
    }

    #[test]
    fn ancestors() {
        assert_eq!(
            render(&test_fixtures::ANCESTORS),
            r#"
              o  Z
              │
              o  Y
            ╭─╯
            o  F
            ╷
            ╷ o  X
            ╭─╯
            │ o  W
            ├─╯
            o  E
            ╷
            o    D
            ├─╮
            │ o  C
            │ ╷
            o ╷  B
            ├─╯
            o  A"#
        );
    }

    #[test]
    fn split_parents() {
        assert_eq!(
            render(&test_fixtures::SPLIT_PARENTS),
            r#"
                  o  E
            ╭─┬─┬─┤
            ╷ o │ ╷  D
            ╭─┴─╮ ╷
            │   o ╷  C
            │   ├─╯
            o   │  B
            ├───╯
            o  A"#
        );
    }

    #[test]
    fn terminations() {
        assert_eq!(
            render(&test_fixtures::TERMINATIONS),
            r#"
              o  K
              │
              │ o  J
              ├─╯
              o    I
            ╭─┼─╮
            │ │ │
            │ ~ │
            │   │
            │   o  H
            │   │
            o   │  E
            ├───╯
            o  D
            │
            ~
            
            o  C
            │
            o  B
            │
            ~"#
        );
    }

    #[test]
    fn long_messages() {
        assert_eq!(
            render(&test_fixtures::LONG_MESSAGES),
            r#"
            o      F
            ├─┬─╮  very long message 1
            │ │ │  very long message 2
            │ │ ~  very long message 3
            │ │
            │ │    very long message 4
            │ │    very long message 5
            │ │    very long message 6
            │ │
            │ o  E
            │ │
            │ o  D
            │ │
            o │  C
            ├─╯  long message 1
            │    long message 2
            │    long message 3
            │
            o  B
            │
            o  A
            │  long message 1
            ~  long message 2
               long message 3"#
        );
    }

    #[test]
    fn different_orders() {
        let order = |order: &str| {
            let order = order.matches(|_: char| true).collect::<Vec<_>>();
            let mut renderer = GraphRowRenderer::new().output().build_box_drawing();
            render_string_with_order(&test_fixtures::ORDERS1, &mut renderer, Some(&order))
        };

        assert_eq!(
            order("KJIHGFEDCBZA"),
            r#"
            o    K
            ├─╮
            │ o    J
            │ ├─╮
            │ │ o    I
            │ │ ├─╮
            │ │ │ o    H
            │ │ │ ├─╮
            │ │ │ │ o    G
            │ │ │ │ ├─╮
            o │ │ │ │ │  F
            │ │ │ │ │ │
            │ o │ │ │ │  E
            ├─╯ │ │ │ │
            │   o │ │ │  D
            ├───╯ │ │ │
            │     o │ │  C
            ├─────╯ │ │
            │       o │  B
            ├───────╯ │
            │         o  Z
            │
            o  A"#
        );

        assert_eq!(
            order("KJIHGZBCDEFA"),
            r#"
            o    K
            ├─╮
            │ o    J
            │ ├─╮
            │ │ o    I
            │ │ ├─╮
            │ │ │ o    H
            │ │ │ ├─╮
            │ │ │ │ o    G
            │ │ │ │ ├─╮
            │ │ │ │ │ o  Z
            │ │ │ │ │
            │ │ │ │ o  B
            │ │ │ │ │
            │ │ │ o │  C
            │ │ │ ├─╯
            │ │ o │  D
            │ │ ├─╯
            │ o │  E
            │ ├─╯
            o │  F
            ├─╯
            o  A"#
        );

        // Keeping the p1 branch the longest path (KFEDCBA) is a reasonable
        // optimization for a cleaner graph (less columns, more text space).
        assert_eq!(
            render(&test_fixtures::ORDERS2),
            r#"
            o    K
            ├─╮
            │ o  J
            │ │
            o │    F
            ├───╮
            │ │ o  I
            │ ├─╯
            o │    E
            ├───╮
            │ │ o  H
            │ ├─╯
            o │    D
            ├───╮
            │ │ o  G
            │ ├─╯
            o │    C
            ├───╮
            │ │ o  Z
            │ │
            o │  B
            ├─╯
            o  A"#
        );

        // Try to use the ORDERS2 order. However, the parent ordering in the
        // graph is different, which makes the rendering different.
        //
        // Note: it's KJFIEHDGCZBA in the ORDERS2 graph. To map it to ORDERS1,
        // follow:
        //
        // ORDERS1: KFJEIDHCGBZA
        // ORDERS2: KJFIEHDGCBZA
        //
        // And we get KFJEIDHCGZBA.
        assert_eq!(
            order("KFJEIDHCGZBA"),
            r#"
            o    K
            ├─╮
            o │  F
            │ │
            │ o    J
            │ ├─╮
            │ o │  E
            ├─╯ │
            │   o  I
            │ ╭─┤
            │ │ o  D
            ├───╯
            │ o    H
            │ ├─╮
            │ o │  C
            ├─╯ │
            │   o  G
            │ ╭─┤
            │ o │  Z
            │   │
            │   o  B
            ├───╯
            o  A"#
        );
    }
}
```

renderer.rs
```rs

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::ops::Range;

use bitflags::bitflags;
#[cfg(feature = "serialize")]
use serde::Serialize;

use super::column::Column;
use super::column::ColumnsExt;
use super::output::OutputRendererBuilder;

pub trait Renderer<N> {
    type Output;

    // Returns the width of the graph line, possibly including another node.
    fn width(&self, new_node: Option<&N>, new_parents: Option<&Vec<Ancestor<N>>>) -> u64;

    // Reserve a column for the given node.
    fn reserve(&mut self, node: N);

    // Render the next row.
    fn next_row(
        &mut self,
        node: N,
        parents: Vec<Ancestor<N>>,
        glyph: String,
        message: String,
    ) -> Self::Output;
}

/// Renderer for a DAG.
///
/// Converts a sequence of DAG node descriptions into rendered graph rows.
pub struct GraphRowRenderer<N> {
    columns: Vec<Column<N>>,
}

/// Ancestor type indication for an ancestor or parent node.
pub enum Ancestor<N> {
    /// The node is an eventual ancestor.
    Ancestor(N),

    /// The node is an immediate parent.
    Parent(N),

    /// The node is an anonymous ancestor.
    Anonymous,
}

impl<N> Ancestor<N> {
    fn to_column(&self) -> Column<N>
    where
        N: Clone,
    {
        match self {
            Ancestor::Ancestor(n) => Column::Ancestor(n.clone()),
            Ancestor::Parent(n) => Column::Parent(n.clone()),
            Ancestor::Anonymous => Column::Blocked,
        }
    }

    fn id(&self) -> Option<&N> {
        match self {
            Ancestor::Ancestor(n) => Some(n),
            Ancestor::Parent(n) => Some(n),
            Ancestor::Anonymous => None,
        }
    }

    fn is_direct(&self) -> bool {
        match self {
            Ancestor::Ancestor(_) => false,
            Ancestor::Parent(_) => true,
            Ancestor::Anonymous => true,
        }
    }

    fn to_link_line(&self, direct: LinkLine, indirect: LinkLine) -> LinkLine {
        if self.is_direct() { direct } else { indirect }
    }
}

struct AncestorColumnBounds {
    target: usize,
    min_ancestor: usize,
    min_parent: usize,
    max_parent: usize,
    max_ancestor: usize,
}

impl AncestorColumnBounds {
    fn new<N>(columns: &BTreeMap<usize, &Ancestor<N>>, target: usize) -> Option<Self> {
        if columns.is_empty() {
            return None;
        }
        let min_ancestor = columns
            .iter()
            .next()
            .map_or(target, |(index, _)| *index)
            .min(target);
        let max_ancestor = columns
            .iter()
            .next_back()
            .map_or(target, |(index, _)| *index)
            .max(target);
        let min_parent = columns
            .iter()
            .find(|(_, ancestor)| ancestor.is_direct())
            .map_or(target, |(index, _)| *index)
            .min(target);
        let max_parent = columns
            .iter()
            .rev()
            .find(|(_, ancestor)| ancestor.is_direct())
            .map_or(target, |(index, _)| *index)
            .max(target);
        Some(Self {
            target,
            min_ancestor,
            min_parent,
            max_parent,
            max_ancestor,
        })
    }

    fn range(&self) -> Range<usize> {
        if self.min_ancestor < self.max_ancestor {
            self.min_ancestor + 1..self.max_ancestor
        } else {
            Default::default()
        }
    }

    fn horizontal_line(&self, index: usize) -> LinkLine {
        if index == self.target {
            LinkLine::empty()
        } else if index > self.min_parent && index < self.max_parent {
            LinkLine::HORIZ_PARENT
        } else if index > self.min_ancestor && index < self.max_ancestor {
            LinkLine::HORIZ_ANCESTOR
        } else {
            LinkLine::empty()
        }
    }
}

impl<N> Column<N> {
    fn to_node_line(&self) -> NodeLine {
        match self {
            Column::Ancestor(_) => NodeLine::Ancestor,
            Column::Parent(_) => NodeLine::Parent,
            _ => NodeLine::Blank,
        }
    }

    fn to_link_line(&self) -> LinkLine {
        match self {
            Column::Ancestor(_) => LinkLine::VERT_ANCESTOR,
            Column::Parent(_) => LinkLine::VERT_PARENT,
            _ => LinkLine::empty(),
        }
    }

    fn to_pad_line(&self) -> PadLine {
        match self {
            Column::Ancestor(_) => PadLine::Ancestor,
            Column::Parent(_) => PadLine::Parent,
            _ => PadLine::Blank,
        }
    }
}

/// A column in the node row.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum NodeLine {
    /// Blank.
    Blank,

    /// Vertical line indicating an ancestor.
    Ancestor,

    /// Vertical line indicating a parent.
    Parent,

    /// The node for this row.
    Node,
}

/// A column in a padding row.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum PadLine {
    /// Blank.
    Blank,

    /// Vertical line indicating an ancestor.
    Ancestor,

    /// Vertical line indicating a parent.
    Parent,
}

bitflags! {
    /// A column in a linking row.
    #[cfg_attr(feature = "serialize", derive(Serialize))]
    #[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
    pub struct LinkLine: u16 {
        /// This cell contains a horizontal line that connects to a parent.
        const HORIZ_PARENT = 0b0_0000_0000_0001;

        /// This cell contains a horizontal line that connects to an ancestor.
        const HORIZ_ANCESTOR = 0b0_0000_0000_0010;

        /// The descendent of this cell is connected to the parent.
        const VERT_PARENT = 0b0_0000_0000_0100;

        /// The descendent of this cell is connected to an ancestor.
        const VERT_ANCESTOR = 0b0_0000_0000_1000;

        /// The parent of this cell is linked in this link row and the child
        /// is to the left.
        const LEFT_FORK_PARENT = 0b0_0000_0001_0000;

        /// The ancestor of this cell is linked in this link row and the child
        /// is to the left.
        const LEFT_FORK_ANCESTOR = 0b0_0000_0010_0000;

        /// The parent of this cell is linked in this link row and the child
        /// is to the right.
        const RIGHT_FORK_PARENT = 0b0_0000_0100_0000;

        /// The ancestor of this cell is linked in this link row and the child
        /// is to the right.
        const RIGHT_FORK_ANCESTOR = 0b0_0000_1000_0000;

        /// The child of this cell is linked to parents on the left.
        const LEFT_MERGE_PARENT = 0b0_0001_0000_0000;

        /// The child of this cell is linked to ancestors on the left.
        const LEFT_MERGE_ANCESTOR = 0b0_0010_0000_0000;

        /// The child of this cell is linked to parents on the right.
        const RIGHT_MERGE_PARENT = 0b0_0100_0000_0000;

        /// The child of this cell is linked to ancestors on the right.
        const RIGHT_MERGE_ANCESTOR = 0b0_1000_0000_0000;

        /// The target node of this link line is the child of this column.
        /// This disambiguates between the node that is connected in this link
        /// line, and other nodes that are also connected vertically.
        const CHILD = 0b1_0000_0000_0000;

        const HORIZONTAL = Self::HORIZ_PARENT.bits() | Self::HORIZ_ANCESTOR.bits();
        const VERTICAL = Self::VERT_PARENT.bits() | Self::VERT_ANCESTOR.bits();
        const LEFT_FORK = Self::LEFT_FORK_PARENT.bits() | Self::LEFT_FORK_ANCESTOR.bits();
        const RIGHT_FORK = Self::RIGHT_FORK_PARENT.bits() | Self::RIGHT_FORK_ANCESTOR.bits();
        const LEFT_MERGE = Self::LEFT_MERGE_PARENT.bits() | Self::LEFT_MERGE_ANCESTOR.bits();
        const RIGHT_MERGE = Self::RIGHT_MERGE_PARENT.bits() | Self::RIGHT_MERGE_ANCESTOR.bits();
        const ANY_MERGE = Self::LEFT_MERGE.bits() | Self::RIGHT_MERGE.bits();
        const ANY_FORK = Self::LEFT_FORK.bits() | Self::RIGHT_FORK.bits();
        const ANY_FORK_OR_MERGE = Self::ANY_MERGE.bits() | Self::ANY_FORK.bits();
    }
}

/// An output graph row.
#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct GraphRow<N> {
    /// The name of the node for this row.
    pub node: N,

    /// The glyph for this node.
    pub glyph: String,

    /// The message for this row.
    pub message: String,

    /// True if this row is for a merge commit.
    pub merge: bool,

    /// The node columns for this row.
    pub node_line: Vec<NodeLine>,

    /// The link columns for this row, if a link row is necessary.
    pub link_line: Option<Vec<LinkLine>>,

    /// The location of any terminators, if necessary.  Other columns should be
    /// filled in with pad lines.
    pub term_line: Option<Vec<bool>>,

    /// The pad columns for this row.
    pub pad_lines: Vec<PadLine>,
}

impl<N> GraphRowRenderer<N>
where
    N: Clone + Eq + std::fmt::Debug,
{
    /// Create a new renderer.
    pub fn new() -> Self {
        GraphRowRenderer {
            columns: Vec::new(),
        }
    }

    /// Build an output renderer from this renderer.
    pub fn output(self) -> OutputRendererBuilder<N, Self> {
        OutputRendererBuilder::new(self)
    }
}

impl<N> Renderer<N> for GraphRowRenderer<N>
where
    N: Clone + Eq + std::fmt::Debug,
{
    type Output = GraphRow<N>;

    fn width(&self, node: Option<&N>, parents: Option<&Vec<Ancestor<N>>>) -> u64 {
        let mut width = self.columns.len();
        let mut empty_columns = self
            .columns
            .iter()
            .filter(|&column| column == &Column::Empty)
            .count();
        if let Some(node) = node {
            // If the node is not already allocated, and there is no
            // space for the node, then adding the new node would create
            // a new column.
            if self.columns.find(node).is_none() {
                if empty_columns == 0 {
                    width += 1;
                } else {
                    empty_columns = empty_columns.saturating_sub(1);
                }
            }
        }
        if let Some(parents) = parents {
            // Non-allocated parents will also need a new column (except
            // for one, which can take the place of the node, and any that could be allocated to
            // empty columns).
            let unallocated_parents = parents
                .iter()
                .filter(|parent| {
                    parent
                        .id()
                        .is_none_or(|parent| self.columns.find(parent).is_none())
                })
                .count()
                .saturating_sub(empty_columns);
            width += unallocated_parents.saturating_sub(1);
        }
        width as u64
    }

    fn reserve(&mut self, node: N) {
        if self.columns.find(&node).is_none() {
            if let Some(index) = self.columns.first_empty() {
                self.columns[index] = Column::Reserved(node);
            } else {
                self.columns.push(Column::Reserved(node));
            }
        }
    }

    fn next_row(
        &mut self,
        node: N,
        parents: Vec<Ancestor<N>>,
        glyph: String,
        message: String,
    ) -> GraphRow<N> {
        // Find a column for this node.
        let column = self.columns.find(&node).unwrap_or_else(|| {
            self.columns
                .first_empty()
                .unwrap_or_else(|| self.columns.new_empty())
        });
        self.columns[column] = Column::Empty;

        // This row is for a merge if there are multiple parents.
        let merge = parents.len() > 1;

        // Build the initial node line.
        let mut node_line: Vec<_> = self.columns.iter().map(|c| c.to_node_line()).collect();
        node_line[column] = NodeLine::Node;

        // Build the initial link line.
        let mut link_line: Vec<_> = self.columns.iter().map(|c| c.to_link_line()).collect();
        let mut need_link_line = false;

        // Build the initial term line.
        let mut term_line: Vec<_> = self.columns.iter().map(|_| false).collect();
        let mut need_term_line = false;

        // Build the initial pad line.
        let mut pad_lines: Vec<_> = self.columns.iter().map(|c| c.to_pad_line()).collect();

        // Assign each parent to a column.
        let mut parent_columns = BTreeMap::new();
        for p in parents.iter() {
            // Check if the parent already has a column.
            if let Some(parent_id) = p.id() {
                if let Some(index) = self.columns.find(parent_id) {
                    self.columns[index].merge(&p.to_column());
                    parent_columns.insert(index, p);
                    continue;
                }
            }
            // Assign the parent to an empty column, preferring the column
            // the current node is going in, to maintain linearity.
            if let Some(index) = self.columns.find_empty(column) {
                self.columns[index].merge(&p.to_column());
                parent_columns.insert(index, p);
                continue;
            }
            // There are no empty columns left.  Make a new column.
            parent_columns.insert(self.columns.len(), p);
            node_line.push(NodeLine::Blank);
            pad_lines.push(PadLine::Blank);
            link_line.push(LinkLine::default());
            term_line.push(false);
            self.columns.push(p.to_column());
        }

        // Mark parent columns with anonymous parents as terminating.
        for (i, p) in parent_columns.iter() {
            if p.id().is_none() {
                term_line[*i] = true;
                need_term_line = true;
            }
        }

        // Check if we can move the parent to the current column.
        if parents.len() == 1 {
            if let Some((&parent_column, _)) = parent_columns.iter().next() {
                if parent_column > column {
                    // This node has a single parent which was already
                    // assigned to a column to the right of this one.
                    // Move the parent to this column.
                    self.columns.swap(column, parent_column);
                    let parent = parent_columns
                        .remove(&parent_column)
                        .expect("parent should exist");
                    parent_columns.insert(column, parent);

                    // Generate a line from this column to the old
                    // parent column.   We need to continue with the style
                    // that was being used for the parent column.
                    let was_direct = link_line[parent_column].contains(LinkLine::VERT_PARENT);
                    link_line[column] |= if was_direct {
                        LinkLine::RIGHT_FORK_PARENT
                    } else {
                        LinkLine::RIGHT_FORK_ANCESTOR
                    };
                    #[allow(clippy::needless_range_loop)]
                    for i in column + 1..parent_column {
                        link_line[i] |= if was_direct {
                            LinkLine::HORIZ_PARENT
                        } else {
                            LinkLine::HORIZ_ANCESTOR
                        };
                    }
                    link_line[parent_column] = if was_direct {
                        LinkLine::LEFT_MERGE_PARENT
                    } else {
                        LinkLine::LEFT_MERGE_ANCESTOR
                    };
                    need_link_line = true;
                    // The pad line for the old parent column is now blank.
                    pad_lines[parent_column] = PadLine::Blank;
                }
            }
        }

        // Connect the node column to all the parent columns.
        if let Some(bounds) = AncestorColumnBounds::new(&parent_columns, column) {
            // If the parents extend beyond the columns adjacent to the node, draw a horizontal
            // ancestor line between the two outermost ancestors.
            for i in bounds.range() {
                link_line[i] |= bounds.horizontal_line(i);
                need_link_line = true;
            }

            // If there is a parent or ancestor to the right of the node
            // column, the node merges from the right.
            if bounds.max_parent > column {
                link_line[column] |= LinkLine::RIGHT_MERGE_PARENT;
                need_link_line = true;
            } else if bounds.max_ancestor > column {
                link_line[column] |= LinkLine::RIGHT_MERGE_ANCESTOR;
                need_link_line = true;
            }

            // If there is a parent or ancestor to the left of the node column, the node merges from the left.
            if bounds.min_parent < column {
                link_line[column] |= LinkLine::LEFT_MERGE_PARENT;
                need_link_line = true;
            } else if bounds.min_ancestor < column {
                link_line[column] |= LinkLine::LEFT_MERGE_ANCESTOR;
                need_link_line = true;
            }

            // Each parent or ancestor forks towards the node column.
            #[allow(clippy::comparison_chain)]
            for (&i, p) in parent_columns.iter() {
                pad_lines[i] = self.columns[i].to_pad_line();
                if i < column {
                    link_line[i] |=
                        p.to_link_line(LinkLine::RIGHT_FORK_PARENT, LinkLine::RIGHT_FORK_ANCESTOR);
                } else if i == column {
                    link_line[i] |= LinkLine::CHILD
                        | p.to_link_line(LinkLine::VERT_PARENT, LinkLine::VERT_ANCESTOR);
                } else {
                    link_line[i] |=
                        p.to_link_line(LinkLine::LEFT_FORK_PARENT, LinkLine::LEFT_FORK_ANCESTOR);
                }
            }
        }

        // Now that we have assigned all the columns, reset their state.
        self.columns.reset();

        // Filter out the link line or term line if they are not needed.
        let link_line = Some(link_line).filter(|_| need_link_line);
        let term_line = Some(term_line).filter(|_| need_term_line);

        GraphRow {
            node,
            glyph,
            message,
            merge,
            node_line,
            link_line,
            term_line,
            pad_lines,
        }
    }
}
```
