vim.keymap.set(
  { "n", "v" },
  "<F5>",
  require("operator").wrap(function()
    local regname = vim.list_contains(vim.opt.clipboard:get(), "unnamedplus") and "+" or ""
    return vim.fn.getreg(regname, 1, true), vim.fn.getregtype(regname)
  end),
  { expr = true }
)
