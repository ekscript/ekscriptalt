/// Has Token definitions

/// Basically an enum of all Possible Token Types
pub const TokenType = enum {
  KeyWordAsync

};

pub const Token = struct {
    tok_type: TokenType,

    /// line number
    line: i32,

    /// Index of the start of the token in the array
    start: i32,

    /// end of the token in the string stream
    end: i32,

};

