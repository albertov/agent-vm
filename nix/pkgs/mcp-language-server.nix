{
  inputs,
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "mcp-language-server";
  version = "git-${src.rev}";

  src = inputs.mcp-language-server;

  vendorHash = "sha256-WcYKtM8r9xALx68VvgRabMPq8XnubhTj6NAdtmaPa+g=";

  subPackages = [ "." ];

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  doCheck = false;

  meta = with lib; {
    description = "Model Context Protocol server that integrates Language Server Protocol capabilities";
    longDescription = ''
      A Model Context Protocol (MCP) server that runs a language server and provides tools for communicating with it.
      Language servers excel at tasks that LLMs often struggle with, such as precisely understanding types,
      understanding relationships, and providing accurate symbol references. This project brings those
      tools to LLMs by integrating LSP capabilities into the MCP ecosystem.

      Provides tools for:
      - read_definition: Retrieve complete source code definitions
      - find_references: Locate all symbol usages
      - get_diagnostics: Get warnings and errors for files
      - hover: Display type hints and documentation
      - rename_symbol: Rename symbols across projects
      - apply_text_edit: Make programmatic code edits

      Supports various language servers including:
      - gopls (Go)
      - pyright (Python)  
      - typescript-language-server (TypeScript)
      - rust-analyzer (Rust)
      - And many more LSP-compatible servers
    '';
    homepage = "https://github.com/isaacphi/mcp-language-server";
    license = licenses.mit; # Verify the actual license from the repository
    mainProgram = "mcp-language-server";
  };
}
