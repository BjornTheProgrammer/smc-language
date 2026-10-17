use std::collections::HashMap;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use smc_assembler::assembler::backends;
use smc_assembler::assembler::{AssembledInstruction, Assembler, AssemblerError, LabelMap};
use smc_assembler::lexer::token::Span;
use smc_assembler::lexer::{Lexer, LexerError};
use smc_assembler::parser::{
    DefineMap, DefineSpanMap, LabelSpanMap, Parser, ParserError, ReferenceMap,
};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Definitions {
    defines: DefineMap,
    labels: LabelMap,
    define_spans: DefineSpanMap,
    label_spans: LabelSpanMap,
    define_references: ReferenceMap,
    label_references: ReferenceMap,
    instructions: Vec<AssembledInstruction>,
    /// Total program size in bytes
    size: usize,
    bytes_per_address: usize,
    source_text: String,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    /// Each open document's latest assembly, by URI
    documents: RwLock<HashMap<Url, Definitions>>,
    /// Whether the client can be asked to request inlay hints again
    inlay_hint_refresh: AtomicBool,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let refresh = params
            .capabilities
            .workspace
            .and_then(|workspace| workspace.inlay_hint)
            .and_then(|inlay_hint| inlay_hint.refresh_support)
            .unwrap_or(false);
        self.inlay_hint_refresh.store(refresh, Ordering::Relaxed);

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::INFO, "Shutdown initiated")
            .await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, &params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(params.text_document.uri, &params.content_changes[0].text)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.write().await.remove(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let Some(definitions) = documents.get(&uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(position, &definitions.source_text);

        // Check if cursor is on a define reference -> go to define definition
        for (name, ref_spans) in &definitions.define_references {
            for ref_span in ref_spans {
                if offset >= ref_span.start() && offset <= ref_span.end() {
                    if let Some(def_span) = definitions.define_spans.get(name) {
                        let range = span_to_range(def_span, &definitions.source_text);
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                            uri, range,
                        ))));
                    }
                }
            }
        }

        // Check if cursor is on a label reference -> go to label definition
        for (name, ref_spans) in &definitions.label_references {
            for ref_span in ref_spans {
                if offset >= ref_span.start() && offset <= ref_span.end() {
                    if let Some(def_span) = definitions.label_spans.get(name) {
                        let range = span_to_range(def_span, &definitions.source_text);
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                            uri, range,
                        ))));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let documents = self.documents.read().await;
        let Some(definitions) = documents.get(&uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(position, &definitions.source_text);

        // Find which symbol the cursor is on
        let symbol_name = find_symbol_at_offset(offset, definitions);
        let symbol_name = match symbol_name {
            Some(name) => name,
            None => return Ok(None),
        };

        let mut locations = Vec::new();

        // Check if it's a define
        if definitions.define_spans.contains_key(&symbol_name) {
            if include_declaration {
                if let Some(def_span) = definitions.define_spans.get(&symbol_name) {
                    let range = span_to_range(def_span, &definitions.source_text);
                    locations.push(Location::new(uri.clone(), range));
                }
            }
            if let Some(ref_spans) = definitions.define_references.get(&symbol_name) {
                for ref_span in ref_spans {
                    let range = span_to_range(ref_span, &definitions.source_text);
                    locations.push(Location::new(uri.clone(), range));
                }
            }
        }

        // Check if it's a label
        if definitions.label_spans.contains_key(&symbol_name) {
            if include_declaration {
                if let Some(def_span) = definitions.label_spans.get(&symbol_name) {
                    let range = span_to_range(def_span, &definitions.source_text);
                    locations.push(Location::new(uri.clone(), range));
                }
            }
            if let Some(ref_spans) = definitions.label_references.get(&symbol_name) {
                for ref_span in ref_spans {
                    let range = span_to_range(ref_span, &definitions.source_text);
                    locations.push(Location::new(uri.clone(), range));
                }
            }
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let Some(definitions) = documents.get(&uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(position, &definitions.source_text);

        // Find the symbol at this position
        if let Some(name) = find_symbol_at_offset(offset, definitions) {
            // Check if it's a define
            if let Some(value) = definitions.defines.get(&name) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "**define** `{}`\n\nValue: `{}` (dec) | `0b{:b}` (bin) | `0x{:X}` (hex)",
                            name, value, *value as i64, *value as i64
                        ),
                    }),
                    range: None,
                }));
            }

            // Check if it's a label
            if let Some(address) = definitions.labels.get(&name) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "**label** `.{}`\n\nAddress: `{}` (dec) | `0b{:b}` (bin) | `0x{:X}` (hex)",
                            name, address, address, address
                        ),
                    }),
                    range: None,
                }));
            }
        }

        // Otherwise show the machine code of the instruction under the cursor
        let instruction = definitions.instructions.iter().find(|instruction| {
            offset >= instruction.span.start() && offset <= instruction.span.end()
        });
        if let Some(instruction) = instruction {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: instruction_hover(instruction, definitions),
                }),
                range: Some(span_to_range(&instruction.span, &definitions.source_text)),
            }));
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let documents = self.documents.read().await;
        let Some(definitions) = documents.get(&params.text_document_position.text_document.uri)
        else {
            return Ok(Some(CompletionResponse::Array(vec![])));
        };

        let mut items = Vec::new();

        // Add defines to completion
        for (name, value) in &definitions.defines {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some(format!("= {}", value)),
                insert_text: Some(name.clone()),
                ..Default::default()
            });
        }

        // Add labels to completion
        for (name, address) in &definitions.labels {
            items.push(CompletionItem {
                label: format!(".{}", name),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(format!("@ {}", address)),
                insert_text: Some(format!(".{}", name)),
                ..Default::default()
            });
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let documents = self.documents.read().await;
        let Some(definitions) = documents.get(&params.text_document.uri) else {
            return Ok(None);
        };

        // The program's size, after its last instruction
        let Some(last) = definitions.instructions.last() else {
            return Ok(None);
        };
        let position = offset_to_position(last.span.end(), &definitions.source_text);
        if position < params.range.start || position > params.range.end {
            return Ok(None);
        }

        Ok(Some(vec![InlayHint {
            position,
            label: InlayHintLabel::String(size_text(definitions)),
            kind: None,
            text_edits: None,
            tooltip: Some(InlayHintTooltip::String("Program size".to_string())),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        }]))
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: &str) {
        // The path works for any URI scheme, including unsaved documents
        let backend = Path::new(uri.path())
            .extension()
            .and_then(|ext| ext.to_str())
            .and_then(backends::find_by_extension);

        let Some(backend) = backend else {
            return;
        };

        let tokens: Vec<_> = Lexer::new(text).into_iter().collect();
        let parsed = Parser::new(tokens).parse();
        let assembler_result = Assembler::new(backend, parsed).assemble();

        // Store definitions (even if there are errors, we still want navigation to work)
        self.documents.write().await.insert(
            uri.clone(),
            Definitions {
                defines: assembler_result.defines,
                labels: assembler_result.labels,
                define_spans: assembler_result.define_spans,
                label_spans: assembler_result.label_spans,
                define_references: assembler_result.define_references,
                label_references: assembler_result.label_references,
                instructions: assembler_result.instructions,
                size: assembler_result.size,
                bytes_per_address: assembler_result.bytes_per_address,
                source_text: text.to_string(),
            },
        );

        let diagnostics: Vec<_> = assembler_result
            .result
            .err()
            .unwrap_or_default()
            .into_iter()
            .map(|err| Diagnostic {
                range: span_to_range(extract_span_from_error(&err), text),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("smc-assembler".to_string()),
                message: err.to_string(),
                ..Default::default()
            })
            .collect();

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;

        // The program's size may have changed
        if self.inlay_hint_refresh.load(Ordering::Relaxed) {
            let _ = self.client.inlay_hint_refresh().await;
        }
    }
}

/// An instruction as written, then each word it assembled to with its address
fn instruction_hover(instruction: &AssembledInstruction, definitions: &Definitions) -> String {
    let source = &definitions.source_text[instruction.span.start()..instruction.span.end()];
    let total = format!("Program size: {}", size_text(definitions));
    if instruction.bytes.is_empty() {
        return format!("**instruction** `{source}`\n\nAssembles to nothing\n\n{total}");
    }

    let words: Vec<String> = instruction
        .bytes
        .chunks(definitions.bytes_per_address)
        .enumerate()
        .map(|(i, word)| {
            let binary: String = word.iter().map(|byte| format!("{byte:08b}")).collect();
            let hex: String = word.iter().map(|byte| format!("{byte:02X}")).collect();
            format!("0x{:02X}  {binary}  {hex}", instruction.address + i)
        })
        .collect();

    format!(
        "**instruction** `{source}`\n\n```text\n{}\n```\n\n{total}",
        words.join("\n")
    )
}

/// The program's size in bytes, and in words for targets with wider addresses
fn size_text(definitions: &Definitions) -> String {
    let plural = |count: usize, unit: &str| match count {
        1 => format!("1 {unit}"),
        _ => format!("{count} {unit}s"),
    };

    let bytes = plural(definitions.size, "byte");
    match definitions.bytes_per_address {
        1 => bytes,
        n => format!("{bytes} ({})", plural(definitions.size / n, "word")),
    }
}

/// Extract the span from an assembler error
fn extract_span_from_error(err: &AssemblerError) -> &Span {
    match err {
        AssemblerError::DefineNotFound(span, _) => span,
        AssemblerError::LabelNotFound(span, _) => span,
        AssemblerError::ParserError(parser_error) => match parser_error {
            ParserError::SyntaxError(lexer_error) => match lexer_error {
                LexerError::InvalidNumber(span, _) => span,
                LexerError::UnexpectedCharacter(span, _) => span,
                LexerError::ExpectedCharacter(span, _) => span,
                LexerError::InvalidOffset(span, _) => span,
            },
            ParserError::DuplicateDefine(span, _) => span,
            ParserError::DuplicateLabel(span, _) => span,
            ParserError::ExpectedButReceived(span, _, _) => span,
            ParserError::UnexpectedEof(span) => span,
        },
        AssemblerError::UnsupportedOperation(span, _) => span,
        AssemblerError::InvalidRegister(span, _) => span,
        AssemblerError::AddressOutOfRange(span, _) => span,
        AssemblerError::OffsetOutOfRange(span, _) => span,
        AssemblerError::InvalidCondition(span, _) => span,
        AssemblerError::ImmediateOutOfRange(span, _) => span,
        AssemblerError::InvalidOperands(span, _) => span,
    }
}

/// Convert a Span to an LSP Range
fn span_to_range(span: &Span, source: &str) -> Range {
    let start = offset_to_position(span.start(), source);
    let end = offset_to_position(span.end(), source);
    Range::new(start, end)
}

/// Convert an LSP Position to a byte offset in the source text. Positions count columns in
/// UTF-16 units, and a column past the end of a line means the end of that line.
fn position_to_offset(position: Position, source: &str) -> usize {
    let mut line_start = 0;
    for _ in 0..position.line {
        match source[line_start..].find('\n') {
            Some(i) => line_start += i + 1,
            None => return source.len(),
        }
    }

    let rest = &source[line_start..];
    let line = &rest[..rest.find('\n').unwrap_or(rest.len())];
    let line = line.strip_suffix('\r').unwrap_or(line);

    let mut units = 0;
    for (i, ch) in line.char_indices() {
        if units >= position.character as usize {
            return line_start + i;
        }
        units += ch.len_utf16();
    }
    line_start + line.len()
}

/// Convert a byte offset to an LSP Position, counting columns in UTF-16 units
fn offset_to_position(offset: usize, source: &str) -> Position {
    let mut offset = offset.min(source.len());
    while !source.is_char_boundary(offset) {
        offset -= 1;
    }

    let before = &source[..offset];
    let line = before.matches('\n').count();
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let column: usize = before[line_start..].chars().map(char::len_utf16).sum();
    Position::new(line as u32, column as u32)
}

/// Find the symbol name at a given byte offset
fn find_symbol_at_offset(offset: usize, definitions: &Definitions) -> Option<String> {
    // Check define definitions
    for (name, span) in &definitions.define_spans {
        if offset >= span.start() && offset <= span.end() {
            return Some(name.clone());
        }
    }

    // Check label definitions
    for (name, span) in &definitions.label_spans {
        if offset >= span.start() && offset <= span.end() {
            return Some(name.clone());
        }
    }

    // Check define references
    for (name, spans) in &definitions.define_references {
        for span in spans {
            if offset >= span.start() && offset <= span.end() {
                return Some(name.clone());
            }
        }
    }

    // Check label references
    for (name, spans) in &definitions.label_references {
        for span in spans {
            if offset >= span.start() && offset <= span.end() {
                return Some(name.clone());
            }
        }
    }

    None
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: RwLock::new(HashMap::new()),
        inlay_hint_refresh: AtomicBool::new(false),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converts_lf_positions() {
        let source = "ldi r1 1\njmp .loop\n";
        assert_eq!(position_to_offset(Position::new(1, 4), source), 13);
        assert_eq!(offset_to_position(13, source), Position::new(1, 4));
    }

    #[test]
    fn converts_crlf_positions() {
        let source = "ldi r1 1\r\njmp .loop\r\nhlt";
        // `.loop` starts at byte 14, after "ldi r1 1\r\njmp "
        assert_eq!(position_to_offset(Position::new(1, 4), source), 14);
        assert_eq!(offset_to_position(14, source), Position::new(1, 4));
        assert_eq!(position_to_offset(Position::new(2, 0), source), 21);
        // Columns past the end of a line stop before its \r\n
        assert_eq!(position_to_offset(Position::new(0, 99), source), 8);
    }

    #[test]
    fn counts_columns_in_utf16() {
        // 'é' is 2 bytes and 1 UTF-16 unit, '😀' is 4 bytes and 2 UTF-16 units
        let source = "# é😀 .x";
        let label = source.find(".x").unwrap();
        assert_eq!(offset_to_position(label, source), Position::new(0, 6));
        assert_eq!(position_to_offset(Position::new(0, 6), source), label);
    }
}
