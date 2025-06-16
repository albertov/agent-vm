{ lib
, python3
, fetchPypi
}:

python3.pkgs.buildPythonPackage rec {
  pname = "mcp";
  version = "1.9.2";
  format = "pyproject";
  build-system = with python3.pkgs; [
    hatchling
    uv-dynamic-versioning
  ];
  dependencies = with python3.pkgs; [
    python-multipart
    anyio
    httpx-sse
    httpx
    pydantic-settings
    pydantic
    sse-starlette
    starlette
    uvicorn
    typer
  ];
  src = fetchPypi {
    inherit pname version;
    hash = "sha256-PHZRwFPWNf0jWZChLoRQn+MngM01mlu+81LiDU2WPAU=";
  };

}
