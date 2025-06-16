# codemcp - AI Pair Programming with Claude Desktop

**codemcp** is a Model Context Protocol (MCP) tool that transforms Claude Desktop into a powerful pair programming assistant. It enables Claude to directly edit files, run commands, and work with your codebase without the need to copy code back and forth.

## What is codemcp?

codemcp provides Claude with a comprehensive set of tools to interact with your filesystem and development environment:

- **File Operations**: Read, write, and edit files directly
- **Project Navigation**: List directories, search files with patterns (glob), and grep for content
- **Command Execution**: Run predefined commands like formatters, linters, and tests
- **Git Integration**: All changes are automatically committed with descriptive messages
- **IDE Agnostic**: Works with any editor - Claude makes changes, you review them in your preferred IDE

## Key Features

### Auto-Accept by Default
Unlike other AI coding tools, codemcp is designed to work autonomously while maintaining safety:
- **Predeclared Commands**: Only commands defined in `codemcp.toml` can be executed (no unrestricted shell access)
- **Git Version Control**: Every change is automatically committed, allowing fine-grained rollbacks
- **Subscription-Based**: Works with Claude Pro/Max subscriptions for zero marginal cost per action

### Project Configuration
Each project uses a `codemcp.toml` file to configure available commands and project-specific instructions:

```toml
project_prompt = """
Project-specific instructions for Claude
"""

[commands]
format = ["./run_format.sh"]
test = ["./run_test.sh"]
lint = ["hlint"]

[commands.test]
command = ["./run_test.sh"]
doc = "Accepts a pytest-style test selector as an argument"
```

## Installation and Configuration for Claude Desktop on Linux

### Prerequisites

1. **Install dependencies**:
   ```bash
   # Install uv (Python package manager)
   curl -LsSf https://astral.sh/uv/install.sh | sh

   # Ensure git is installed
   sudo apt update && sudo apt install git  # On Ubuntu/Debian
   # OR
   sudo dnf install git  # On Fedora/RHEL
   ```

### Configure Claude Desktop

1. **Create or edit the Claude Desktop configuration file**:
   ```bash
   mkdir -p ~/.config/anthropic/claude
   ```

2. **Edit `~/.config/anthropic/claude/claude_desktop_config.json`**:
   ```json
   {
     "mcpServers": {
       "codemcp": {
         "command": "/home/YOUR_USERNAME/.local/bin/uvx",
         "args": [
           "--from",
           "git+https://github.com/ezyang/codemcp@prod",
           "codemcp"
         ]
       }
     }
   }
   ```

   **Replace `YOUR_USERNAME` with your actual username**.

3. **Alternative: Global pip installation** (if you prefer not to use uv):
   ```bash
   pip install git+https://github.com/ezyang/codemcp@prod
   ```

   Then use this configuration:
   ```json
   {
     "mcpServers": {
       "codemcp": {
         "command": "python",
         "args": ["-m", "codemcp"]
       }
     }
   }
   ```

4. **Restart Claude Desktop** after modifying the configuration.

### Verify Installation

1. Open Claude Desktop
2. Look for a **hammer icon** in the interface - this indicates MCP tools are loaded
3. Click the hammer icon and verify "codemcp" appears in the list
4. You can also ask Claude: "What tools do you have available?" - it should mention codemcp tools

### Troubleshooting

If codemcp fails to load:

1. **Check logs**: Go to Settings > Developer > codemcp > Logs in Claude Desktop
2. **Verify paths**: Ensure the command path in your config is correct:
   ```bash
   which uvx  # Should show the path to uvx
   # OR
   which python  # If using pip installation
   ```
3. **Linux-specific**: Make sure uvx is in your global PATH (not just shell profile)

## Usage

### 1. Set up a project

Create a `codemcp.toml` file in your git repository root:

```toml
# Basic configuration
project_prompt = """
Add any project-specific instructions for Claude here.
For example: coding standards, testing requirements, etc.
"""

[commands]
format = ["./run_format.sh"]
test = ["./run_test.sh"]
```

### 2. Initialize in Claude Desktop

Create a new project in Claude Desktop and add this to the Project Instructions:

```
Initialize codemcp with /absolute/path/to/your/project
```

### 3. Start coding with Claude

Simply describe what you want Claude to do:
- "Implement a new feature that..."
- "Fix the bug in..."
- "Refactor the code to..."

Claude will:
- Read and understand your codebase
- Make the necessary changes
- Run formatting/linting automatically
- Create git commits for all changes

## Example Project Configuration

Here's a sample config

```toml
project_prompt = """
You must not do stupid things
"""

[commands.format]
command = ["run_format.sh"]
doc = "Must pass the name of the haskell file that needs formatting as an argument"

[commands.test]
command = ["run_test.sh"]
doc = "Accepts the name of a single package as an argument to run their specific tests"

[commands.changed_files]
command = ["git", "diff", "--name-only", "origin/master...HEAD"]
doc = "List the file names that have been changed in this branch"
```

## Philosophy

- **Review-driven development**: Claude does the work, you review and guide
- **Time over money**: Uses subscription-based pricing, so focus on quality over cost optimization
- **Safety through constraints**: Predeclared commands and git versioning prevent dangerous operations
- **IDE independence**: Use your favorite development environment alongside Claude

## Advanced Tips

- **Multiple projects**: You can configure different codemcp.toml files for different types of projects
- **Command documentation**: Use the `doc` field in command definitions to help Claude understand when and how to use each tool
- **Logging**: Configure logging levels in your `codemcp.toml` to debug issues
- **Remote development**: codemcp can work with remote servers when properly configured

For more information and advanced configuration options, visit the [codemcp repository](https://github.com/ezyang/codemcp).
