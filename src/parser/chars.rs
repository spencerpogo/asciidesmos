use nom::character::{is_digit, is_alphabetic};

#[inline]
pub fn is_underscore(chr: u8) -> bool {
    chr == 0x5f
}

// No digits in leading char
#[inline]
pub fn is_ident_leading_char(chr: char) -> bool {
    !chr.is_ascii() || is_alphabetic(chr as u8) || is_underscore(chr as u8)
}

#[inline]
pub fn is_ident_char(chr: char) -> bool {
    is_ident_leading_char(chr) || chr.is_digit(10)
}

#[inline]
pub fn is_space(chr: u8) -> bool {
    chr == 0x20 || chr == 0x9 // tab
}

#[inline]
pub fn is_space_char(chr: char) -> bool {
    chr.is_ascii() && is_space(chr as u8)
}

#[inline]
pub fn is_newline(chr: u8) -> bool {
    chr == 0xa
}

#[inline]
pub fn is_space_newline(chr: u8) -> bool {
    is_space(chr) || is_newline(chr)
}

#[inline]
pub fn is_space_newline_char(chr: char) -> bool {
    chr.is_ascii() && is_space_newline(chr as u8)
}

#[inline]
pub fn is_comma(chr: u8) -> bool {
    chr == 0x2c
}

#[inline]
pub fn is_arg_sep(chr: char) -> bool {
    chr.is_ascii() && is_comma(chr as u8) || is_space_newline(chr as u8)
}

#[inline]
pub fn is_close_paren(chr: u8) -> bool {
    chr == 0x29
}

#[inline]
pub fn is_arg_char(chr: char) -> bool {
    !chr.is_ascii()
        || (!is_comma(chr as u8) && !is_space_newline(chr as u8) && !is_close_paren(chr as u8))
}

#[cfg(test)]
mod test {
    use super::*;

    // Only testing the complicated ones
    #[test]
    fn test_is_arg_sep() {
        assert_eq!(is_arg_sep('1'), false);
        assert_eq!(is_arg_sep('a'), false);
        // umlaut
        assert_eq!(is_arg_sep('\u{00fc}'), false);
        assert_eq!(is_arg_sep(','), true);
        assert_eq!(is_arg_sep(' '), true);
        assert_eq!(is_arg_sep('\n'), true);
    }

    #[test]
    fn test_is_arg_char() {
        assert_eq!(is_arg_char('1'), true);
        assert_eq!(is_arg_char('a'), true);
        // umlaut
        assert_eq!(is_arg_char('\u{00fc}'), true);
        assert_eq!(is_arg_char(' '), false);
        assert_eq!(is_arg_char('\n'), false);
        assert_eq!(is_arg_char(')'), false);
    }
}
