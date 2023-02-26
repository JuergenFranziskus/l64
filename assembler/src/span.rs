


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    length: usize,
}
impl Span {
    pub fn from_logos(s: std::ops::Range<usize>) -> Span {
        let length = s.end - s.start;

        Span {
            start: s.start,
            length,
        }
    }

    pub fn merge(self: Span, other: Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end().max(other.end());
        let length = end - start;

        Span {
            start,
            length,
        }
    }

    pub fn end(self) -> usize {
        self.start + self.length
    }
}

