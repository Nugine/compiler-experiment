macro_rules! bail {
    ($($t:tt)+) => {
        return Err(format!($($t)+).into())
    };
}

pub type DynResult<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync + 'static>>;

pub mod grammar;
pub mod symbol;
