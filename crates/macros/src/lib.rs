//! # match_keywords
//!
//! A proc macro for generating trie-based keyword matchers with case-insensitive matching.
//!
//! ## Usage
//!
//! ```rust,ignore
//! let result = match_keywords!(
//!     "Add" => Keyword::Add,
//!     "Brh" => Keyword::Brh,
//!     "Ret" => Keyword::Ret,
//!     "Rsh" => Keyword::Rsh,
//! );
//!
//! match result {
//!     Some((len, keyword)) => {
//!         self.pos += len;
//!         Ok(Some(TokenSpan::new(Token::Keyword(keyword), Span::new(start, self.pos))))
//!     }
//!     None => {
//!         // handle non-keyword case
//!     }
//! }
//! ```

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::collections::BTreeMap;
use syn::{
    Expr, LitStr, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

/// A single keyword mapping: "keyword" => expression
struct KeywordArm {
    keyword: String,
    expr: Expr,
}

impl Parse for KeywordArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit: LitStr = input.parse()?;
        input.parse::<Token![=>]>()?;
        let expr: Expr = input.parse()?;
        Ok(KeywordArm {
            keyword: lit.value(),
            expr,
        })
    }
}

/// The full macro input: multiple keyword arms separated by commas
struct MatchKeywordsInput {
    arms: Punctuated<KeywordArm, Token![,]>,
}

impl Parse for MatchKeywordsInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arms = Punctuated::parse_terminated(input)?;
        Ok(MatchKeywordsInput { arms })
    }
}

/// Trie node for building the matching logic
#[derive(Default, Debug)]
struct TrieNode {
    /// Children indexed by lowercase character
    children: BTreeMap<char, TrieNode>,
    /// If this node represents a complete keyword, store the expression index
    terminal: Option<usize>,
}

impl TrieNode {
    fn insert(&mut self, word: &str, expr_idx: usize) {
        let mut node = self;
        for ch in word.chars() {
            // Store as lowercase for case-insensitive matching
            node = node.children.entry(ch.to_ascii_lowercase()).or_default();
        }
        node.terminal = Some(expr_idx);
    }
}

/// Check if a keyword needs a word boundary check (ends with alphanumeric or underscore)
fn needs_word_boundary_check(keyword: &str) -> bool {
    keyword
        .chars()
        .last()
        .map(|c| c.is_ascii_alphanumeric() || c == '_')
        .unwrap_or(false)
}

/// Generate a word boundary guard for a terminal match
fn generate_terminal_with_boundary_check(
    keyword_len: usize,
    expr: &Expr,
    keyword: &str,
) -> TokenStream2 {
    if needs_word_boundary_check(keyword) {
        // Generate boundary check: ensure next char is not alphanumeric or underscore
        quote! {
            match self.peek(#keyword_len) {
                Some(c) if c.is_ascii_alphanumeric() || c == b'_' => None,
                _ => Some((#keyword_len, #expr))
            }
        }
    } else {
        // No boundary check needed for operators like "!=", ">=", etc.
        quote! { Some((#keyword_len, #expr)) }
    }
}

/// Generate the trie matching code recursively.
/// Returns code that evaluates to `Option<(usize, keyword_expr)>` where usize is keyword length.
///
/// The matching uses longest-match semantics: if "Ret" and "Return" are both keywords,
/// "Return" will match "Return" (not "Ret" followed by "urn").
fn generate_trie_match(
    node: &TrieNode,
    depth: usize,
    expressions: &[&Expr],
    keywords: &[&str],
) -> TokenStream2 {
    // Base case: leaf node with no children
    if node.children.is_empty() {
        if let Some(idx) = node.terminal {
            let expr = expressions[idx];
            let keyword_len = keywords[idx].len();
            return generate_terminal_with_boundary_check(keyword_len, expr, keywords[idx]);
        }
        return quote! { None };
    }

    // Build match arms for each child character
    let match_arms: Vec<TokenStream2> = node
        .children
        .iter()
        .map(|(&lower_char, child)| {
            let child_code = generate_trie_match(child, depth + 1, expressions, keywords);
            let upper_char = lower_char.to_ascii_uppercase();
            let lower_byte = lower_char as u8;
            let upper_byte = upper_char as u8;

            // If this node is also terminal, we need to try the longer match first,
            // then fall back to this terminal if the longer match fails
            let arm_body = if node.terminal.is_some() {
                let idx = node.terminal.unwrap();
                let fallback_expr = expressions[idx];
                let fallback_len = keywords[idx].len();
                let fallback_with_check = generate_terminal_with_boundary_check(
                    fallback_len,
                    fallback_expr,
                    keywords[idx],
                );
                quote! {
                    match #child_code {
                        Some(result) => Some(result),
                        None => #fallback_with_check
                    }
                }
            } else {
                child_code
            };

            if lower_char.is_ascii_alphabetic() && lower_char != upper_char {
                quote! {
                    Some(#lower_byte) | Some(#upper_byte) => #arm_body
                }
            } else {
                quote! {
                    Some(#lower_byte) => #arm_body
                }
            }
        })
        .collect();

    // Default arm: handle terminal or non-match
    let default_arm = if let Some(idx) = node.terminal {
        let expr = expressions[idx];
        let keyword_len = keywords[idx].len();
        let terminal_with_check =
            generate_terminal_with_boundary_check(keyword_len, expr, keywords[idx]);
        quote! { _ => #terminal_with_check }
    } else {
        quote! { _ => None }
    };

    quote! {
        match self.peek(#depth) {
            #(#match_arms,)*
            #default_arm
        }
    }
}

/// A standalone proc macro that generates a trie-based keyword matcher.
///
/// # Syntax
///
/// ```rust,ignore
/// let result = match_keywords!(
///     "Keyword1" => expr1,
///     "Keyword2" => expr2,
///     // ...
/// );
/// ```
///
/// # Return Value
///
/// Returns `Option<(usize, T)>` where:
/// - `usize` is the length of the matched keyword
/// - `T` is the type of the expressions on the right-hand side of `=>`
/// - Returns `None` if no keyword matches
///
/// # Word Boundary Checking
///
/// For keywords ending in alphanumeric characters, the macro automatically
/// ensures they don't match when followed by more alphanumeric characters.
/// For example, "c" won't match in "count" - it will return None so the
/// lexer can treat "count" as an identifier.
///
/// # Requirements
///
/// The surrounding context must have:
/// - `self.peek(n: usize) -> Option<u8>` - peek at byte at offset n from current position
#[proc_macro]
pub fn match_keywords(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MatchKeywordsInput);

    if input.arms.is_empty() {
        return quote! { None }.into();
    }

    // Build the trie
    let mut root = TrieNode::default();
    let mut expressions: Vec<&Expr> = Vec::new();
    let mut keywords: Vec<&str> = Vec::new();

    for (idx, arm) in input.arms.iter().enumerate() {
        root.insert(&arm.keyword, idx);
        expressions.push(&arm.expr);
        keywords.push(&arm.keyword);
    }

    // Generate the trie matching code starting at depth 0
    let trie_code = generate_trie_match(&root, 0, &expressions, &keywords);

    let output = quote! {
        #trie_code
    };

    output.into()
}
