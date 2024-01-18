macro_rules! assert_or {
    ($cond:expr, $err:expr) => {
        if !$cond {
            Err($err)
        } else {
            Ok(())
        }
    };
}
pub(crate) use assert_or;
