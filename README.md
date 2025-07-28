# operator.nvim

A Neovim plugin that provides utilities for creating custom operator functions.

## Features

- Easily create custom operator functions
- Support for different selection modes (character-wise, line-wise, block-wise)
- Utilities for handling text regions and selections

## Installation

### Using Packer

```lua
use "ClarityWay/operator.nvim"
```

### Using lazy.nvim

```lua
{ "ClarityWay/operator.nvim" }
```

## Usage

### Basic Example

Create a custom operator by wrapping your function with `operator.wrap`:

```lua
local operator = require("operator")

-- Example: Custom operator to uppercase selected text
local function uppercase_text(texts, _)
  return vim.tbl_map(string.upper, texts)
end

vim.keymap.set("n", "gU", operator.wrap(uppercase_text))
```

Now you can use `gU` followed by a motion (e.g., `gUw` to uppercase a word, `gUap` to uppercase a paragraph).

### Line-wise Operators

For line-wise operations, pass `true` as the second argument to `operator.wrap`:

```lua
vim.keymap.set("n", "gU", operator.wrap(uppercase_text, true))
```

## API

### `operator.wrap(func, linewise)`

Wraps a function to create an operator function that can be mapped.

- `func`: The function to wrap. It receives `texts` (selected text lines) and `info` (selection information).
- `linewise`: (optional) If `true`, uses line-wise selection mode.

### `operator.create_opfunc(func)`

Creates a raw operator function compatible with Neovim's `operatorfunc` option.

## License

MIT
