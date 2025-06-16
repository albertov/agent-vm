{ lib
, stdenv
, inputs
, buildNpmPackage
, makeWrapper
, chromium
, chromedriver
, nodejs
}:

buildNpmPackage rec {
  pname = "mcp-selenium";
  version = "git-${src.rev}";

  src = inputs.mcp-selenium;

  npmDepsHash = "sha256-XeM1cqv5PR9JD2h5yEkMvpw8XWQgYRIpkTWXca4g7RY=";

  # Don't run tests during build (there are no tests defined)
  dontNpmBuild = true;

  nativeBuildInputs = [
    makeWrapper
    nodejs
  ];

  buildInputs = [
    chromium
    chromedriver
  ];

  # Set up the runtime environment
  postInstall = ''
    makeWrapper ${nodejs}/bin/node $out/bin/mcp-selenium \
      --add-flags "$out/lib/node_modules/@angiejones/mcp-selenium/src/lib/server.js" \
      --set CHROME_BIN ${chromium}/bin/chromium \
      --set CHROME_PATH ${chromium}/lib/chromium \
      --prefix PATH : ${lib.makeBinPath [ chromium chromedriver ]} \
      --set PUPPETEER_SKIP_CHROMIUM_DOWNLOAD true
  '';

  # Runtime dependencies for selenium
  runtimeDependencies = [
    chromium
    chromedriver
  ];

  meta = with lib; {
    description = "A Model Context Protocol (MCP) server implementation for Selenium WebDriver";
    longDescription = ''
      MCP Selenium provides browser automation capabilities through the Model Context Protocol,
      enabling AI agents and other MCP clients to control web browsers using Selenium WebDriver.
      
      Features include:
      - Start browser sessions with customizable options
      - Navigate to URLs and interact with web elements
      - Support for Chrome and Firefox browsers
      - Headless mode support
      - Screenshot capture
      - File upload functionality
      - Mouse and keyboard interactions
    '';
    homepage = "https://github.com/angiejones/mcp-selenium";
    license = licenses.mit;
    maintainers = [ ]; # Add your name here if you become a maintainer
    platforms = platforms.linux ++ platforms.darwin;
    mainProgram = "mcp-selenium";
  };
}
