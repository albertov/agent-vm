{
  lib,
  python3,
  inputs,
}:
python3.pkgs.buildPythonApplication rec {
  pname = "mcp-proxy";
  version = "git-${src.rev}";
  format = "pyproject";
  build-system = with python3.pkgs; [ setuptools ];

  src = inputs.mcp-proxy;

  dependencies = with python3.pkgs; [
    uvicorn
    mcp
  ];

  doCheck = false;

  meta = with lib; {
    description = "A MCP server which proxies requests to a remote MCP server over SSE transport";
    homepage = "https://github.com/sparfenyuk/mcp-proxy";
    changelog = "https://github.com/sparfenyuk/mcp-proxy/releases";
    license = licenses.mit;
    maintainers = with maintainers; [ ]; # Add your maintainer handle here
    mainProgram = "mcp-proxy";
  };
}
