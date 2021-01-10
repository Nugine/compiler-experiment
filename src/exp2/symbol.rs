use super::DynResult;

use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(char);

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <char as fmt::Debug>::fmt(&self.0, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <char as fmt::Display>::fmt(&self.0, f)
    }
}

impl Symbol {
    pub const EMPTY: Symbol = Symbol('@');
    pub const EOF: Symbol = Symbol('$');

    pub fn new(ch: char) -> DynResult<Self> {
        if !ch.is_ascii_graphic() {
            bail!("unexpected non-ascii-graphic char: {}", ch);
        }
        match ch {
            'A'..='Z' | 'a'..='z' | '@' | '$' | '+' | '-' | '*' | '(' | ')' => Ok(Self(ch)),
            _ => bail!("unexpected char: {}", ch),
        }
    }
    pub fn is_terminal(&self) -> bool {
        self.0 != '@' && (!self.0.is_ascii_uppercase())
    }
    pub fn is_non_terminal(&self) -> bool {
        self.0.is_ascii_uppercase()
    }
    pub fn is_empty(&self) -> bool {
        *self == Self::EMPTY
    }
    pub fn is_eof(&self) -> bool {
        *self == Self::EOF
    }
}
