// Lifted from: https://docs.rs/assert_hex/latest/src/assert_hex/lib.rs.html
// and modified minimally to support assert_eq_bits

// TODO: Is it possible for a Macro to look at a numerical value from code
// and see whether the coder wrote it in a particular base? (i.e. 0b1111, 0xF, 15)
// It would be neat if an error diff automatically showed the most human-meaningful
// comparision (and probably base 10, too).

#[macro_export]
macro_rules! assert_eq_hex {
    ($left:expr, $right:expr $(,)?) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion `left == right` failed
  left: {:#x?}
 right: {:#x?}"#, &*left_val, &*right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion `left == right` failed: {}
  left: {:#x?}
 right: {:#x?}"#, format_args!($($arg)+), &*left_val, &*right_val)
                }
            }
        }
    });
}

#[macro_export]
macro_rules! assert_eq_bits {
    ($left:expr, $right:expr $(,)?) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion `left == right` failed
  left: {:#b}
 right: {:#b}"#, &*left_val, &*right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion `left == right` failed: {}
  left: {:#b?}
 right: {:#b?}"#, format_args!($($arg)+), &*left_val, &*right_val)
                }
            }
        }
    });
}
