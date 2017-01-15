use std::collections::HashSet;
use std::ops::Deref;
use ast::{Type, SumTypeCaseIndexOf, MatchExpression, Pattern, Literal};
use compileerror::*;

fn check_any_match(m: &MatchExpression) -> CompileResult<bool>
{
    let mut any_match_seen = false;
    for (idx, c) in m.cases.iter().enumerate() {
        if let Pattern::Any(ref span) = c.pattern {
            if idx != m.cases.len() - 1 {
                return err(span, ErrorCode::UnreachablePatternMatch, "A pattern match with _ must always be the last one in a match statement");
            } else {
                any_match_seen = true;
            }
        }
    }
    Ok(any_match_seen)
}

fn check_array_match_is_exhaustive(m: &MatchExpression, any_match_seen: bool) -> CompileResult<()>
{
    let mut empty_array_seen = false;
    let mut head_tail_seen = false;

    for c in &m.cases {
        match c.pattern {
            Pattern::EmptyArray(_) => {
                if empty_array_seen {
                    return err(&c.span, ErrorCode::DuplicatePatternMatch, "Duplicate pattern match, pattern match for [] already exists");
                } else {
                    empty_array_seen = true;
                }
            },
            Pattern::Array(_) => {
                if head_tail_seen {
                    return err(&c.span, ErrorCode::DuplicatePatternMatch, "Duplicate pattern match, pattern match already exists");
                } else {
                    head_tail_seen = true;
                }
            },
            _ => (),
        }
    }

    if any_match_seen || (empty_array_seen && head_tail_seen) {
        Ok(())
    } else {
        err(&m.span, ErrorCode::IncompletePatternMatch, "Incomplete pattern match")
    }
}

fn check_sum_match_is_exhaustive<ST: SumTypeCaseIndexOf>(m: &MatchExpression, st: &ST, any_match_seen: bool) -> CompileResult<()>
{
    let mut indexes = HashSet::new();

    let add_to_indices = |idx: Option<usize>, name: &str, indexes: &mut HashSet<usize>| {
        let idx = idx.expect("Internal Compiler Error: cannot determine index of sum type case");
        if indexes.contains(&idx) {
            err(&m.span, ErrorCode::DuplicatePatternMatch, format!("Duplicate pattern match for {}", name))
        } else {
            indexes.insert(idx);
            Ok(())
        }
    };

    for c in &m.cases {
        match c.pattern
        {
            Pattern::Name(ref nr) => {
                add_to_indices(st.index_of(&nr.name), &nr.name, &mut indexes)?;
            },
            Pattern::Struct(ref s) => {
                add_to_indices(st.index_of(&s.name), &s.name, &mut indexes)?;
            },
            _ => (),
        }
    }

    if !any_match_seen && indexes.len() != st.num_cases() {
        return err(&m.span, ErrorCode::IncompletePatternMatch, "Incomplete pattern match, not all cases are handled");
    }
    Ok(())
}

fn check_bool_match_is_exhaustive(m: &MatchExpression) -> CompileResult<()>
{
    let mut true_seen = false;
    let mut false_seen = false;
    for c in &m.cases {
        if let Pattern::Literal(Literal::Bool(_, v)) = c.pattern
        {
            if v {
                if true_seen {
                    return err(&c.span, ErrorCode::DuplicatePatternMatch, "Duplicate pattern match, pattern match for true already exists");
                } else {
                    true_seen = true;
                }
            } else if false_seen {
                return err(&c.span, ErrorCode::DuplicatePatternMatch, "Duplicate pattern match, pattern match for false already exists");
            } else {
                false_seen = true;
            }
        }
    }

    if !true_seen || !false_seen {
        err(&m.span, ErrorCode::IncompletePatternMatch, "Incomplete pattern match, not all boolean values are matched against")
    } else {
        Ok(())
    }
}

fn check_optional_match_is_exhaustive(m: &MatchExpression) -> CompileResult<()>
{
    let mut optional_seen = false;
    let mut nil_seen = false;
    for c in &m.cases {
        match c.pattern
        {
            Pattern::Optional(_) => optional_seen = true,
            Pattern::Nil(_) => nil_seen = true,
            Pattern::Any(_) => return Ok(()),
            _ => (),
        }
    }

    if !optional_seen || !nil_seen {
        err(&m.span, ErrorCode::IncompletePatternMatch, "Incomplete pattern match, not all possible optionals are matched again")
    } else {
        Ok(())
    }
}

pub fn check_match_is_exhaustive(m: &MatchExpression, target_type: &Type) -> CompileResult<()>
{
    let any_match_seen = check_any_match(m)?;

    match *target_type
    {
        Type::Array(_) | Type::Slice(_) => {
            check_array_match_is_exhaustive(m, any_match_seen)
        },

        Type::Sum(ref st) => {
            check_sum_match_is_exhaustive(m, st.deref(), any_match_seen)
        },

        Type::Enum(ref et) => {
            check_sum_match_is_exhaustive(m, et.deref(), any_match_seen)
        },

        Type::Struct(_) => {
            if !m.cases.is_empty() {
                err(&m.span, ErrorCode::DuplicatePatternMatch, "Duplicate pattern match, structs can only have one pattern match")
            } else {
                Ok(())
            }
        },

        Type::Bool => {
            check_bool_match_is_exhaustive(m)
        },

        Type::Optional(_) => {
            check_optional_match_is_exhaustive(m)
        },

        _ => {
            if !any_match_seen {
                err(&m.span, ErrorCode::IncompletePatternMatch, format!("Incomplete pattern match for type {}", target_type))
            } else {
                Ok(())
            }
        },
    }
}
