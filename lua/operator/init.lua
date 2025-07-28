local M = {}

local utils = require("operator.utils")

---@alias vim.o.opfunc.Type "line"|"char"|"block"

---@class (exact) operator.opfunc.Info
---`[row, col]` tuple, representing the position of the first character of the previously selected text, (0,0)-indexed
---@field start_pos [integer, integer]
---`[row, col]` tuple, where `row` and `col` are inclusive and exclusive respectively,
---representing the position of the last character of the previously selected text, (0,0)-indexed
---@field end_pos [integer, integer]
---@field type vim.o.opfunc.Type The argument of the 'operatorfunc', see |:map-operator| for more info.

---
---Generate a new 'operatorfunc'
---
---@param func fun(texts: string[], info: operator.opfunc.Info): string[]?, string? A function called by the |'operatorfunc'| with these parameters:
---Parameters:
---  - texts: A list with the selected text.
---  - info: A table about the motion with these keys:
---    * start_pos: A (0,0)-indexed `[row, col]` tuple representing the position of the first character of the
---      previously selected text.
---    * end_pos: A (0,0)-indexed `[row, col]` tuple representing the position of the last character of the previously
---      selected text.
---    * type: The argument of the 'operatorfunc', see |:map-operator| for more info.
---
---Return:
---  - replacement: An array of lines used to replace the selected region in the buffer.
---  - type: Any |getregtype()| result.
---
---Indexing is 0-based. Row indices are end-inclusive, and column indices are end-exclusive.
---@return fun(regtype: vim.o.opfunc.Type) opfunc # |'operatorfunc'|
local function create_opfunc(func)
  return function(regtype)
    local region, _ipairs = utils.region(0, "'[", "']", regtype, vim.o.selection == "inclusive")

    if not _ipairs then return end

    ---@type string[]
    local texts = vim.iter(_ipairs(region)):map(function(_, r) return r.text end):totable()

    local start_row = vim.fn.line("'[") - 1 -- 0-indexed
    local start_col = region[start_row][1] -- 0-indexed
    local end_row = vim.fn.line("']") - 1 -- 0-indexed
    local end_col = region[end_row][2] -- 0-indexed

    ---@type operator.opfunc.Info
    local info = {
      start_pos = { start_row, start_col },
      end_pos = { end_row, end_col },
      type = regtype,
    }

    local replacement, new_type = func(texts, info)

    if not replacement then return end

    new_type = new_type or ({ line = "V", char = "v", block = vim.keycode("<C-V>") })[regtype]

    if regtype == "line" then
      vim.cmd.normal { "0", bang = true }
      vim.api.nvim_buf_set_lines(0, start_row, end_row + 1, false, replacement)
      vim.cmd.normal { "_", bang = true }
    elseif regtype == "char" then
      local blockwise = (utils.is_visual_mode(false, new_type) or 255) < 32

      local rep = blockwise and { "" } or replacement
      if new_type == "V" then
        table.insert(rep, 1, "")
        table.insert(rep, "")
      end

      vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, rep)

      if blockwise then
        vim.api.nvim_put(replacement, new_type, false, false)
      elseif new_type == "V" then
        vim.cmd.normal { "w", bang = true }
      elseif #rep == 1 then
        vim.fn.cursor(start_row + 1, start_col + #rep[1])
      end
    else
      local col
      if new_type == "v" and #replacement == 1 then
        col = start_col + #replacement[1]
        new_type = vim.keycode("<C-V>")
        for _ = start_row + 1, end_row do
          vim.list_extend(replacement, replacement, 1, 1)
        end
      end

      ---@type string[]
      local new_lines = vim
        .iter(_ipairs(region))
        :map(function(_, r) return (r.extras[1] or "") .. (r.extras[2] or "") end)
        :totable()

      vim.api.nvim_buf_set_lines(0, start_row, end_row + 1, false, new_lines)
      vim.api.nvim_put(replacement, new_type, false, false)

      if col then vim.fn.cursor(start_row + 1, col) end
    end
  end
end

---
---Wrap a function as an operator function that can be mapped with the `<expr>` flag.
---
---@param func fun(texts: string[], info: operator.opfunc.Info): string[]?, string? A function called by the |'operatorfunc'| with these parameters:
---Parameters:
---  - texts: A list with the selected text.
---  - info: A table about the motion with these keys:
---    * start_pos: A (0,0)-indexed `[row, col]` tuple representing the position of the first character of the
---      previously selected text.
---    * end_pos: A (0,0)-indexed `[row, col]` tuple representing the position of the last character of the previously
---      selected text.
---    * type: The argument of the 'operatorfunc', see |:map-operator| for more info.
---
---Return:
---  - replacement: An array of lines used to replace the selected region in the buffer.
---  - type: Any |getregtype()| result.
---
---Indexing is 0-based. Row indices are end-inclusive, and column indices are end-exclusive.
---@param linewise? boolean Whether to use linewise mode (default: false)
---@return fun(): string
function M.wrap(func, linewise)
  assert(type(func) == "function" or getmetatable(func) and type(getmetatable(func).__call) == "function")

  return function()
    M.opfunc = create_opfunc(func)

    local modpath = vim.fs.normalize(debug.getinfo(1, "S").source, { _fast = true })
    local modname = modpath:gsub("^.*/lua/", ""):gsub("%.lua$", ""):gsub("/init$", ""):gsub("/", ".")
    vim.o.operatorfunc = ("v:lua.require'%s'.opfunc"):format(modname)

    return "g@" .. (linewise and "_" or "")
  end
end

return M
