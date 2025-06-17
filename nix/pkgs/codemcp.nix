{ lib
, python3
, fetchFromGitHub
, git
,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "codemcp";
  version = "0.6.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "ezyang";
    repo = "codemcp";
    # Use the 'prod' branch as recommended in the docs
    rev = "prod";
    hash = "sha256-Ja5cojmAu3tP/rzJDaK2YVuOEVQ9AV3IpWRp/LdpUdg="; # Update this hash
  };

  build-system = with python3.pkgs; [
    hatchling
  ];

  dependencies = with python3.pkgs; [
    pathspec
    agno
    anthropic
    anyio
    click
    editorconfig
    fastapi
    google-genai
    mcp
    pyyaml
    ruff
    starlette
    toml
    tomli
    uvicorn
  ];

  # Runtime dependencies - only git is needed externally
  makeWrapperArgs = [
    "--prefix PATH : ${lib.makeBinPath [ git ]}"
  ];

  # Skip tests since they may require additional setup
  doCheck = false;

  pythonImportsCheck = [
    "codemcp"
  ];

  meta = with lib; {
    description = "MCP server for file operations - coding assistant for Claude Desktop";
    longDescription = ''
      codemcp is a coding assistant MCP (Model Context Protocol) server for Claude Desktop.
      It allows Claude to directly edit files, run tests, and perform other coding tasks
      on your local codebase through a safe, version-controlled interface.
    '';
    homepage = "https://github.com/ezyang/codemcp";
    license = licenses.mit;
    maintainers = with maintainers; [ ]; # Add your name here
    platforms = platforms.unix;
    mainProgram = "codemcp";
  };
}
