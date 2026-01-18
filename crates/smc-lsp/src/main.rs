use smc_assembler::assembler::backends::Backend as SmcBackend;
use smc_assembler::assembler::{Assembler, AssemblerError, LabelMap};
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
    source_text: String,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    definitions: RwLock<Option<Definitions>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        work_done_progress_options: Default::default(),
                    },
                )),
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
        self.on_change(TextDocumentChange {
            uri: params.text_document.uri.to_string(),
            text: &params.text_document.text,
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentChange {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri.to_string(),
        })
        .await;
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

        let definitions = self.definitions.read().await;
        let definitions = match definitions.as_ref() {
            Some(d) => d,
            None => return Ok(None),
        };

        let offset = match position_to_offset(position, &definitions.source_text) {
            Some(o) => o,
            None => return Ok(None),
        };

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

        let definitions = self.definitions.read().await;
        let definitions = match definitions.as_ref() {
            Some(d) => d,
            None => return Ok(None),
        };

        let offset = match position_to_offset(position, &definitions.source_text) {
            Some(o) => o,
            None => return Ok(None),
        };

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
        let position = params.text_document_position_params.position;

        let definitions = self.definitions.read().await;
        let definitions = match definitions.as_ref() {
            Some(d) => d,
            None => return Ok(None),
        };

        let offset = match position_to_offset(position, &definitions.source_text) {
            Some(o) => o,
            None => return Ok(None),
        };

        // Find the symbol at this position
        if let Some(name) = find_symbol_at_offset(offset, definitions) {
            // Check if it's a define
            if let Some(value) = definitions.defines.get(&name) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**define** `{}`\n\nValue: `{}`", name, value),
                    }),
                    range: None,
                }));
            }

            // Check if it's a label
            if let Some(address) = definitions.labels.get(&name) {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**label** `.{}`\n\nAddress: `{}`", name, address),
                    }),
                    range: None,
                }));
            }
        }

        Ok(None)
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        let definitions = self.definitions.read().await;
        let definitions = match definitions.as_ref() {
            Some(d) => d,
            None => return Ok(Some(CompletionResponse::Array(vec![]))),
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
}

impl Backend {
    async fn on_change(&self, item: TextDocumentChange<'_>) {
        let backend = (|| {
            let url = Url::parse(&item.uri).ok()?;
            let path = url.to_file_path().ok()?;
            let ext = path.extension()?;

            if ext.eq_ignore_ascii_case("tasm") {
                Some(SmcBackend::TauAnalyzersNone)
            } else if ext.eq_ignore_ascii_case("smc") {
                Some(SmcBackend::BatPU2)
            } else {
                None
            }
        })();

        let backend = match backend {
            Some(backend) => backend,
            None => return,
        };

        let tokens: Vec<_> = Lexer::new(&item.text).into_iter().collect();
        let parsed = Parser::new(tokens).parse();
        let assembler_result = Assembler::new(backend, parsed).assemble();

        // Extract the result before moving assembler_result
        // We need to destructure or take ownership of the result
        let (
            result,
            defines,
            labels,
            define_spans,
            label_spans,
            define_references,
            label_references,
        ) = (
            assembler_result.result,
            assembler_result.defines,
            assembler_result.labels,
            assembler_result.define_spans,
            assembler_result.label_spans,
            assembler_result.define_references,
            assembler_result.label_references,
        );

        // Store definitions (even if there are errors, we still want navigation to work)
        {
            let mut definitions = self.definitions.write().await;
            *definitions = Some(Definitions {
                defines,
                labels,
                define_spans,
                label_spans,
                define_references,
                label_references,
                source_text: item.text.to_string(),
            });
        }

        let uri =
            Url::parse(&item.uri).unwrap_or_else(|_| Url::from_directory_path(&item.uri).unwrap());

        let errors = match result {
            Ok(_) => {
                // Clear diagnostics on success
                self.client.publish_diagnostics(uri, vec![], None).await;
                return;
            }
            Err(errs) => errs,
        };

        let diagnostics: Vec<_> = errors
            .into_iter()
            .map(|err| {
                let span = extract_span_from_error(&err);
                let range = span_to_range(span, item.text);

                Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("smc-assembler".to_string()),
                    message: err.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
            .collect();

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

struct TextDocumentChange<'a> {
    uri: String,
    text: &'a str,
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
                LexerError::UnknownCondition(span, _) => span,
                LexerError::InvalidOffset(span, _) => span,
                LexerError::InvalidIsaCode(span, _) => span,
                LexerError::InvalidRegisterNumber(span, _) => span,
            },
            ParserError::DuplicateDefine(span, _) => span,
            ParserError::DuplicateLabel(span, _) => span,
            ParserError::ExpectedButReceived(span, _, _) => span,
            ParserError::UnexpectedEof(span) => span,
            ParserError::InvalidSkip(span, _) => span,
        },
        AssemblerError::UnsupportedOperation(span, _) => span,
        AssemblerError::InvalidRegister(span, _) => span,
        AssemblerError::AddressOutOfRange(span, _) => span,
        AssemblerError::OffsetOutOfRange(span, _) => span,
        AssemblerError::InvalidCondition(span, _) => span,
        AssemblerError::ImmediateOutOfRange(span, _) => span,
    }
}

/// Convert a Span to an LSP Range
fn span_to_range(span: &Span, source: &str) -> Range {
    let start = offset_to_position(span.start(), source);
    let end = offset_to_position(span.end(), source);
    Range::new(start, end)
}

/// Convert an LSP Position to a byte offset in the source text
fn position_to_offset(position: Position, source: &str) -> Option<usize> {
    let mut offset = 0;
    for (line_num, line) in source.lines().enumerate() {
        if line_num == position.line as usize {
            let char_offset = position.character as usize;
            return Some(offset + char_offset.min(line.len()));
        }
        offset += line.len() + 1; // +1 for newline
    }
    Some(source.len())
}

/// Convert a byte offset to an LSP Position
fn offset_to_position(offset: usize, source: &str) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position::new(line, col)
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
        definitions: RwLock::new(None),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
