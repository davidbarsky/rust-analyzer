//! Validated request parameter newtypes for MCP tools.

use std::fmt;

use schemars::JsonSchema;
use serde::de::Error as _;
use serde::{Deserialize, Deserializer};

macro_rules! numeric_param {
    ($name:ident, $inner:ty) => {
        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, JsonSchema,
        )]
        #[serde(transparent)]
        pub struct $name($inner);

        impl $name {
            pub const fn new(value: $inner) -> Self {
                Self(value)
            }

            pub const fn get(self) -> $inner {
                self.0
            }
        }

        impl From<$name> for $inner {
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

macro_rules! non_empty_string_param {
    ($name:ident, $message:literal) => {
        #[derive(Debug, Clone, PartialEq, Eq, JsonSchema)]
        #[serde(transparent)]
        pub struct $name(String);

        impl $name {
            pub fn parse(value: impl Into<String>) -> Result<Self, String> {
                let value = value.into();
                let value = value.trim().to_owned();
                if value.is_empty() {
                    return Err($message.into());
                }
                Ok(Self(value))
            }

            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                self.as_str()
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let value = String::deserialize(deserializer)?;
                Self::parse(value).map_err(D::Error::custom)
            }
        }

        impl TryFrom<String> for $name {
            type Error = String;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::parse(value)
            }
        }
    };
}

numeric_param!(Line, u32);
numeric_param!(Column, u32);
numeric_param!(SymbolLimit, u32);
numeric_param!(PreviewLimit, u32);
numeric_param!(PreviewOffset, u32);
numeric_param!(ReachabilityDepth, u32);
numeric_param!(ArgumentIndex, usize);

impl PreviewLimit {
    pub fn as_usize(self) -> usize {
        match usize::try_from(self.0) {
            Ok(value) => value,
            Err(_) => usize::MAX,
        }
    }
}

impl PreviewOffset {
    pub fn as_usize(self) -> usize {
        match usize::try_from(self.0) {
            Ok(value) => value,
            Err(_) => usize::MAX,
        }
    }
}

non_empty_string_param!(PatternText, "pattern must not be empty");
non_empty_string_param!(SsrRuleText, "rule must not be empty");
non_empty_string_param!(SymbolQueryText, "query must not be empty");
non_empty_string_param!(RenameTargetText, "newName must not be empty");
non_empty_string_param!(PathPatternText, "path pattern must not be empty");
non_empty_string_param!(DispatchFilterText, "dispatch filter must not be empty");
non_empty_string_param!(FunctionFilterText, "function filter must not be empty");
non_empty_string_param!(ImplFilterText, "impl filter must not be empty");

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(transparent)]
pub struct PlaceholderText(String);

impl PlaceholderText {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for PlaceholderText {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for PlaceholderText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub const fn default_symbol_limit() -> SymbolLimit {
    SymbolLimit::new(50)
}

pub const fn default_preview_limit() -> PreviewLimit {
    PreviewLimit::new(20)
}

pub const fn default_preview_offset() -> PreviewOffset {
    PreviewOffset::new(0)
}

pub const fn default_reachability_depth() -> ReachabilityDepth {
    ReachabilityDepth::new(1)
}

pub fn default_placeholder() -> PlaceholderText {
    PlaceholderText::new("todo!()")
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn non_empty_string_types_trim_and_reject_empty() {
        let parsed: PatternText = serde_json::from_value(json!("  foo($x)  ")).unwrap();
        assert_eq!(parsed.as_str(), "foo($x)");

        let err = serde_json::from_value::<PatternText>(json!("   ")).unwrap_err();
        assert!(err.to_string().contains("pattern must not be empty"));
    }

    #[test]
    fn numeric_types_deserialize_transparently() {
        let line: Line = serde_json::from_value(json!(12)).unwrap();
        assert_eq!(line.get(), 12);
    }

    #[test]
    fn placeholder_preserves_input() {
        let parsed: PlaceholderText = serde_json::from_value(json!("  ")).unwrap();
        assert_eq!(parsed.as_str(), "  ");
    }
}
