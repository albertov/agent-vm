{
  lib,
  python3,
  fetchPypi,
}:

python3.pkgs.buildPythonPackage rec {
  pname = "agno";
  version = "1.5.8";
  pyproject = true;

  build-system = with python3.pkgs; [
    setuptools
    hatchling
  ];

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-qYYDybwPrcemnkcYQj6gBiwqkKQhg59/AWGSqTU22Ak="; # Update this hash
  };

  dependencies = with python3.pkgs; [
    # Core dependencies
    docstring-parser
    gitpython
    httpx
    pydantic
    pydantic-settings
    python-dotenv
    python-multipart
    pyyaml
    rich
    tomli
    typer
    typing-extensions
  ];

  nativeCheckInputs = with python3.pkgs; [
    pytest
    pytest-asyncio
  ];

  # Skip tests for now as they may require additional setup
  doCheck = false;

  pythonImportsCheck = [
    "agno"
  ];

  meta = with lib; {
    description = "A lightweight library for building Multi-Agent Systems with memory, knowledge and reasoning";
    longDescription = ''
      Agno is a full-stack framework for building Multi-Agent Systems with memory, knowledge and reasoning.
      It provides a unified interface to 23+ model providers, is highly performant with agents that
      instantiate in ~3μs, and supports reasoning as a first-class citizen. Agno agents are natively
      multi-modal and can work with text, image, audio, and video inputs/outputs.
    '';
    homepage = "https://pypi.org/project/agno/";
    license = licenses.mpl20; # Mozilla Public License Version 2.0
    maintainers = with maintainers; [ ]; # Add your name here
    platforms = platforms.all;
  };
}
