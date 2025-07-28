local M = {}

---A `[bufnum, lnum, col, off, curswant?]` tuple returned by the |getpos()| family function,
---where `lnum`, `col` and `curswant` are 1-indexed, `off` is 0-indexed
---@alias VimPosition [integer, integer, integer, integer, integer?]

local fn = vim.fn
local api = vim.api

---
---Compare the line number of two positions `p1` and `p2`. It returns an integer less than, equal to, or greater than
---zero if the line number of `p1` is less than, equal to, or greater than that of `p2`.
---
---@param p1 string|VimPosition|vim.fn.getmousepos.ret
---@param p2 string|VimPosition|vim.fn.getmousepos.ret
---@return integer
---
---@see vim.fn.line
---@see vim.fn.getpos
---@see vim.fn.getcharpos
---@see vim.fn.getcurpos
---@see vim.fn.getcursorcharpos
---@see vim.fn.getmousepos
function M.linecmp(p1, p2)
  local line1 = type(p1) == "string" and fn.line(p1) or (p1[2] and p1[2] or p1.line)
  local line2 = type(p2) == "string" and fn.line(p2) or (p2[2] and p2[2] or p2.line)
  return line1 - line2
end

---@enum (key) VimColFuncName
local col_fn_name = {
  col = 1,
  charcol = 2,
  virtcol = 3,
}

---
---Compare the column number of two positions `p1` and `p2`. It returns an integer less than, equal to, or greater than
---zero if the column number of `p1` is less than, equal to, or greater than that of `p2`.
---
---@param p1 string|VimPosition|vim.fn.getmousepos.ret
---@param p2 string|VimPosition|vim.fn.getmousepos.ret
---@param fn_name? VimColFuncName
---@return integer
---
---@see vim.fn.col
---@see vim.fn.charcol
---@see vim.fn.virtcol
---@see vim.fn.getpos
---@see vim.fn.getcharpos
---@see vim.fn.getcurpos
---@see vim.fn.getcursorcharpos
---@see vim.fn.getmousepos
function M.colcmp(p1, p2, fn_name)
  fn_name = fn_name or "col"
  if not col_fn_name[fn_name] then error("Unknown function: " .. tostring(fn_name)) end
  local column1 = type(p1) == "string" and fn[fn_name](p1) or (p1[3] and p1[3] or p1.column)
  local column2 = type(p2) == "string" and fn[fn_name](p2) or (p2[3] and p2[3] or p2.column)
  return column1 - column2
end

---
---Compare two positions `p1` and `p2`. It returns an integer less than, equal to, or greater than zero if `p1`
---is forward than, equal to, or backward than `p2`.
---
---@param p1 string|VimPosition|vim.fn.getmousepos.ret
---@param p2 string|VimPosition|vim.fn.getmousepos.ret
---@param fn_name? VimColFuncName
---@return integer
---
---@see vim.fn.getpos
---@see vim.fn.getcharpos
---@see vim.fn.getcurpos
---@see vim.fn.getcursorcharpos
---@see vim.fn.getmousepos
function M.poscmp(p1, p2, fn_name)
  local res = M.linecmp(p1, p2)
  return res == 0 and M.colcmp(p1, p2, fn_name) or res
end

---
---Check the specified position is within the visual area.
---
---@param pos VimPosition|vim.fn.getmousepos.ret
---@param charidx? boolean
---@return boolean
---
---@see vim.fn.getpos
---@see vim.fn.getcharpos
---@see vim.fn.getcurpos
---@see vim.fn.getcursorcharpos
---@see vim.fn.getmousepos
function M.in_visual_area(pos, charidx)
  if pos.winid and pos.winid ~= fn.win_getid() then return false end

  local get_pos = (pos.winid or not charidx) and "getpos" or "getcharpos"
  local start_pos = fn[get_pos]("v") --[[@as VimPosition]]
  local end_pos = fn[get_pos](".") --[[@as VimPosition]]

  if M.poscmp(start_pos, end_pos) > 0 then
    -- Make sure the start position is before the end position
    start_pos, end_pos = end_pos, start_pos
  end

  return M.poscmp(start_pos, pos) <= 0
    and M.poscmp(pos, end_pos) <= 0
    and (pos.column or pos[3]) < fn.col { (pos.line or pos[2]), "$" }
end

---
---Check whether current mode is visual or select mode.
---If `mode` is present, treat it as the current mode.
---If `buf` is present and is not current buffer, return -1.
---
---@param check_selectmode? boolean
---@param mode? string
---@param buf? integer|string
---@return integer|false # `false` if current mode is not visual or select mode, otherwise, the ascii code of current mode is returned
function M.is_visual_mode(check_selectmode, mode, buf)
  if buf ~= nil and not vim.list_contains({ 0, fn.bufnr(), "%", fn.bufname() }, buf) then return false end
  local mode_map = { v = 118, V = 86, [vim.keycode("<C-V>")] = 22 }
  if check_selectmode then vim.tbl_extend("error", mode_map, { s = 115, S = 83, [vim.keycode("<C-S>")] = 19 }) end
  return mode_map[mode and mode:sub(1, 1) or fn.mode()] or false
end

---
---Get the position for `expr` in `buf`.
---
---@param expr string
---@param buf? integer|string
---@return VimPosition
---
---@see vim.fn.getpos
function M.getpos(expr, buf)
  local err = { 0, 0, 0, 0 }
  buf = (buf == nil or buf == 0) and fn.bufnr() or fn.bufnr(buf)

  if fn.bufexists(buf) == 0 then return err end
  if buf == fn.bufnr() then return fn.getpos(expr) end

  if expr == "$" then return { buf, api.nvim_buf_line_count(buf), 1, 0 } end

  local marks = vim.list_extend(fn.getmarklist(buf), fn.getmarklist())
  local target = (expr == "." or expr == "v") and [['"]] or expr

  for _, item in ipairs(marks) do
    if item.mark == target then return item.pos end
  end

  return err
end

---
---Get the position of the cursor in `buf`.
---
---@param buf? integer|string
---@return VimPosition
---
---@see vim.fn.getcurpos
function M.getcurpos(buf)
  if buf == nil or vim.list_contains({ 0, fn.bufnr(), "%", fn.bufname() }, buf) then return fn.getcurpos() end

  for _, item in ipairs(fn.getmarklist(buf)) do
    if item.mark == [['"]] then
      item.pos[5] = M.virtcol({ unpack(item.pos, 2) }, false, buf)
      return item.pos
    end
  end

  return { 0, 0, 0, 0, 0 }
end

---
---Get the character index position of the cursor in `buf`.
---
---@param buf? integer|string
---@return VimPosition
---
---@see vim.fn.getcursorcharpos
function M.getcursorcharpos(buf)
  if buf == nil or vim.list_contains({ 0, fn.bufnr(), "%", fn.bufname() }, buf) then return fn.getcursorcharpos() end

  local err = { 0, 0, 0, 0, 0 }

  for _, item in ipairs(fn.getmarklist(buf)) do
    if item.mark == [['"]] then
      if fn.bufloaded(buf) == 0 then fn.bufload(buf) end

      local line = fn.getbufline(buf, item.pos[2])[1]
      if not line then return err end

      item.pos[5] = M.virtcol({ unpack(item.pos, 2) }, false, buf)
      item.pos[3] = fn.charidx(line, item.pos[3] - 1) + 1
      return item.pos
    end
  end

  return err
end

---
---Get the screen column of the file position given with `expr` in `buf`.
---
---If `list` is present and non-false, then virtcol() returns a List with the first and last screen position occupied by
---the character.
---
---With the optional `buf` argument, the values are obtained for that buffer instead of the current buffer.
---
---If `mode` exists, treat it as the current mode. Only valid when 'virtualedit' is turned on.
---
---If `offset` is present and non-zero, then virtcol() returns the screen column with the `offset` characters to the
---right (when `offset` is positive) or left (when `offset` is negative) of the specified position.
---
---@param expr string|[integer, integer|"$", integer?]
---@param list? boolean
---@param buf? integer|string
---@param mode? string
---@param offset? integer
---@return integer|[integer, integer]
---
---@see vim.fn.virtcol
function M.virtcol(expr, list, buf, mode, offset)
  vim.validate { offset = { offset, "number", true } }
  local err = list and { 0, 0 } or 0

  buf = (buf == nil or buf == 0) and fn.bufnr() or fn.bufnr(buf)

  if fn.bufexists(buf) == 0 then return err end
  if fn.bufloaded(buf) == 0 then fn.bufload(buf) end

  if type(expr) == "string" then
    if buf == fn.bufnr() then
      local pos = fn.getpos(expr == "$" and "." or expr)
      if pos[2] > 0 and pos[3] > 0 then expr = expr == "$" and { pos[2], "$", pos[4] } or { unpack(pos, 2) } end
    else
      local marks = vim.list_extend(fn.getmarklist(buf), fn.getmarklist())
      local target = (expr == "." or expr == "$" or expr == "v") and [['"]] or expr

      for _, item in ipairs(marks) do
        if item.mark == target then
          expr = expr == "$" and { item.pos[2], "$", item.pos[4] } or { unpack(item.pos, 2) }
          break
        end
      end
    end

    if type(expr) == "string" then return err end
  elseif type(expr) == "table" then
    if type(expr[1]) ~= "number" then return err end
    if type(expr[2]) ~= "number" and expr[2] ~= "$" then return err end
  else
    return err
  end

  local line = fn.getbufline(buf, expr[1])[1]
  if not line then return err end
  local chars = fn.split(line, [[\zs]])

  local visual = M.is_visual_mode(true, mode, buf)

  local winid = buf == fn.bufnr() and fn.win_getid() or fn.bufwinid(buf)
  local wo_ve = winid > 0 and vim.split(vim.wo[winid].virtualedit, ",", { trimempty = true }) or {}
  local opt_ve = #wo_ve > 0 and wo_ve or vim.opt.virtualedit:get()

  local virtualedit = vim.list_contains(opt_ve, "all") or (visual or 255) < 32 and vim.list_contains(opt_ve, "block")

  local col = expr[2] == "$" and #line + 1 or expr[2]
  local off = (type(expr[3]) ~= "number" or expr[3] < 0) and 0 or expr[3]

  if col < 1 or col > #line + 1 then return err end

  local charcol = fn.charidx(line, col - 1) + 1

  if (offset or 0) ~= 0 then
    if virtualedit then
      local i = 0
      local step = offset > 0 and 1 or -1

      while i ~= offset do
        if chars[charcol] == "\t" then
          local tab_width = fn.strdisplaywidth("\t", fn.strdisplaywidth(table.concat(chars, nil, 1, charcol - 1)))

          while i ~= offset do
            i = i + step
            off = math.min(tab_width - 1, off) + step
            if off < 0 or off == tab_width then
              off = step < 0 and math.huge or 0
              charcol = charcol + step
              break
            end
          end
        elseif charcol == #chars + 1 then
          i = i + step
          off = off + step
          if off < 0 then
            off = math.huge
            charcol = charcol + step
          end
        else
          i = i + step
          off = step < 0 and math.huge or 0
          charcol = charcol + step
        end
      end
    else
      charcol = math.min(math.max(1, charcol + offset), #chars + 1)
    end
  end

  local width1 = fn.strdisplaywidth(table.concat(chars, nil, 1, charcol - 1))
  local width2 = chars[charcol] and fn.strdisplaywidth(chars[charcol], width1) or 0

  local virtcol = {}

  if virtualedit then
    if charcol == #chars + 1 then
      virtcol[1] = width1 + 1 + off
      virtcol[2] = virtcol[1]
    elseif chars[charcol] == "\t" then
      virtcol[1] = width1 + 1 + math.min(width2 - 1, off)
      virtcol[2] = virtcol[1]
    else
      virtcol[1] = width1 + 1
      virtcol[2] = width1 + width2
    end
  else
    virtcol[1] = width1 + 1
    virtcol[2] = width1 + math.max(1, width2)
  end

  return list and virtcol or virtcol[2]
end

---
---Return the byte index of the character in `buf` at buffer line `lnum` and virtual column `col`.
---
---@param buf integer|string
---@param lnum integer
---@param col integer
---@return integer
---
---@see vim.fn.virtcol2col
function M.virtcol2col(buf, lnum, col)
  if vim.list_contains({ 0, fn.bufnr(), "%", fn.bufname() }, buf) then return fn.virtcol2col(0, lnum, col) end

  if fn.bufexists(buf) == 0 then return -1 end
  if type(col) ~= "number" or col < 0 then return -1 end

  if fn.bufloaded(buf) == 0 then fn.bufload(buf) end

  local line = fn.getbufline(buf, lnum)[1]
  if not line then return -1 end

  for v = math.min(col, fn.strdisplaywidth(line)), 1, -1 do
    local id = fn.match(line, ([[\%%%uv.]]):format(v))
    if id ~= -1 then return id + 1 end
  end

  return 1
end

---@class (exact) LineRegionText
---@field [1] integer Starting column (byte offset), 0-indexed
---@field [2] integer Ending column (byte offset), 0-indexed, exclusive. Negative indices are interpreted as `length + 1 + index`: -1 refers to the index past the end.
---@field text string The text between the start and end columns of the current line
---@field line string Current line text
---@field extras [string?, string?] The texts outside the start and end columns of the current line

---@class (exact) RegionText
---@field [integer]? LineRegionText A key-value pair `(linenr,LineRegionText)`
---@field start_row? integer Starting row (byte offset), 0-indexed
---@field end_row? integer Ending row (byte offset), 0-indexed, inclusive

---
---Gets a dict of line segment ("chunk") positions and its associated texts for the region from `pos1` to `pos2`.
---
---Input and output positions are byte positions, (0,0)-indexed. "End of line" column
---position (for example, |linewise| visual selection) is returned as |v:maxcol| (big number).
---
---@param buf integer|string Buffer number or buffer name, use 0 or "%" for current buffer
---@param pos1 [integer, integer, integer?, integer?]|string Start of region as a `[line, column, offset?, curswant?]` tuple or |getpos()|-compatible string
---@param pos2 [integer, integer, integer?, integer?]|string End of region as a `[line, column, offset?, curswant?]` tuple or |getpos()|-compatible string
---@param regtype string [setreg()]-style selection type or any 'operatorfunc' argument (the |blockwise-visual| mode may include width, e.g. "block3"), see |:map-operator|
---@param inclusive? boolean Controls whether the ending column is inclusive (see also 'selection').
---@return RegionText region
---@return (fun(t: RegionText): ((fun(table: RegionText, i: integer?): integer, LineRegionText), RegionText, integer))? _ipairs # An iterator function that can be used to iterate over key-value pairs in `region` in numerical order.
---
---@see vim.region
function M.region(buf, pos1, pos2, regtype, inclusive)
  vim.validate {
    buf = { buf, { "number", "string" } },
    pos1 = { pos1, { "table", "string" } },
    pos2 = { pos2, { "table", "string" } },
    regtype = { regtype, "string" },
    inclusive = { inclusive, "boolean", true },
  }

  buf = buf == 0 and fn.bufnr() or fn.bufnr(buf)
  if fn.bufexists(buf) == 0 then return {} end
  if fn.bufloaded(buf) == 0 then fn.bufload(buf) end

  regtype = (({ char = "v", c = "v", line = "V", l = "V" })[regtype] or regtype)
    :gsub("^block(%d*)$", vim.keycode("<C-V>%1"))
    :gsub("^b(%d*)$", vim.keycode("<C-V>%1"))

  local winid = buf == fn.bufnr() and fn.win_getid() or fn.bufwinid(buf)
  local wo_ve = winid > 0 and vim.split(vim.wo[winid].virtualedit, ",", { trimempty = true }) or {}
  local opt_ve = #wo_ve > 0 and wo_ve or vim.opt.virtualedit:get()

  ---@type [{ charpos: VimPosition, col: integer, tab_width: integer? }, { charpos: VimPosition, col: integer, tab_width: integer? }]
  local edges = {} -- `col` is 0-indexed

  local buf_line_count

  for i, p in ipairs { pos1, pos2 } do
    if type(p) == "string" then
      -- When entering a mark like '0 or 'A and encountering a multi-byte character, the return value of the
      -- `getcharpos` function may be incorrect, but the return value of the `getpos` function is still correct.
      local charpos = p == "." and M.getcurpos(buf) or M.getpos(p, buf)
      local col = charpos[3] >= vim.v.maxcol and -1 or charpos[3] - 1
      local line = api.nvim_buf_get_lines(charpos[1], charpos[2] - 1, charpos[2], false)[1]
      if not line then return {} end
      charpos[3] = fn.charidx(line, math.min(charpos[3] - 1, #line)) + 1

      edges[i] = { charpos = charpos, col = col }
    elseif type(p) == "table" then
      for j = 1, 2 do
        assert(type(p[j]) == "number" and p[j] >= 0, ("Invalid 'pos%d': Expected Lua VimPosition"):format(i))
        assert(p[j + 2] == nil or type(p[j + 2]) == "number", ("Invalid 'pos%d': Expected Lua VimPosition"):format(i))
      end

      local charpos = { buf, unpack(p) } ---@type VimPosition

      -- check that region falls within current buffer
      buf_line_count = buf_line_count or api.nvim_buf_line_count(buf)
      charpos[2] = math.min(p[1] + 1, buf_line_count)

      local line = api.nvim_buf_get_lines(buf, charpos[2] - 1, charpos[2], false)[1]
      if not line then return {} end

      charpos[3] = fn.charidx(line, math.min(p[2], #line)) + 1
      if not charpos[4] or charpos[4] < 0 then charpos[4] = 0 end

      local virtualedit = vim.list_contains(opt_ve, "all")
        or vim.list_contains(opt_ve, "block") and (M.is_visual_mode(true, regtype) or 255) < 32

      local virtcol -- 0-based index
      local tab_width

      if virtualedit and charpos[4] > 0 then
        local char = fn.strcharpart(line, charpos[3] - 1, 1, true)
        if char == "\t" then
          virtcol = fn.strdisplaywidth(fn.strcharpart(line, 0, charpos[3] - 1, true))
          tab_width = fn.strdisplaywidth("\t", virtcol)
          charpos[4] = math.min(charpos[4], tab_width - 1)
        elseif char ~= "" then
          charpos[4] = 0
        end
      else
        charpos[4] = 0
      end

      if charpos[5] and charpos[5] <= 0 then
        charpos[5] = (virtcol or fn.strdisplaywidth(fn.strcharpart(line, 0, charpos[3] - 1, true))) + charpos[4]
      end

      edges[i] = {
        charpos = charpos,
        col = p[2] >= vim.v.maxcol and -1 or math.min(p[2], #line),
        tab_width = tab_width,
      }
    end

    if buf ~= (edges[i].charpos[1] == 0 and fn.bufnr() or edges[i].charpos[1]) then return {} end
  end

  if M.poscmp(edges[1].charpos, edges[2].charpos) > 0 then
    edges[1], edges[2] = edges[2], edges[1]
  end

  -- getpos() may return { 0, 0, 0, 0 }
  if edges[1].charpos[2] == 0 or edges[1].charpos[3] == 0 then return {} end

  ---@type RegionText
  local region = {
    start_row = edges[1].charpos[2] - 1,
    end_row = edges[2].charpos[2] - 1,
  }

  local lines = fn.getbufline(buf, edges[1].charpos[2], edges[2].charpos[2]) --[=[@as string[]]=]

  if regtype == "v" or regtype == "s" then -- character-wise
    ---@type [{ [integer]: string, edges: [integer, integer], tab_fix: table<integer, { value: integer, width: integer, offset: integer }>?, eol_fix: integer }, { [integer]: string, edges: [integer, integer], tab_fix: table<integer, { value: integer, width: integer, offset: integer }>?, eol_fix: integer }?]
    local chars_list = { fn.split(lines[1], [[\zs]]) }
    if #lines > 1 then table.insert(chars_list, fn.split(lines[#lines], [[\zs]])) end

    local virtualedit = vim.list_contains(opt_ve, "all")

    if inclusive then
      edges[2].col = fn.byteidx(lines[#lines], edges[2].charpos[3])
    elseif not vim.deep_equal(vim.list_slice(edges[1].charpos, 2, 4), vim.list_slice(edges[2].charpos, 2, 4)) then
      local finish = edges[2]
      local id = finish.charpos[4] > 0 and 4 or 3
      finish.charpos[id] = finish.charpos[id] - 1
    end

    for i, edge in ipairs(edges) do
      local chars = chars_list[i == 1 and 1 or #chars_list]
      local charcol, off = edge.charpos[3], edge.charpos[4]

      chars.edges = chars.edges or {}
      table.insert(chars.edges, edge.col)

      if virtualedit and chars[charcol] == "\t" then
        local tab_width = edge.tab_width
          or fn.strdisplaywidth("\t", fn.strdisplaywidth(table.concat(chars, nil, 1, charcol - 1)))

        local tab_fix = (-1) ^ i * off + i - 1
        chars.tab_fix = chars.tab_fix or {}
        if chars.tab_fix[charcol] then
          chars.tab_fix[charcol].value = chars.tab_fix[charcol].value + tab_fix
        else
          chars.tab_fix[charcol] = { value = tab_fix, width = tab_width, offset = (2 - i) * off }
        end
      elseif charcol > #chars then
        local eol_fix = (-1) ^ i * off + i - 1
        chars.eol_fix = chars.eol_fix and chars.eol_fix + eol_fix or eol_fix
      end
    end

    local vchar = virtualedit and " " or "\n"

    for i, chars in ipairs(chars_list) do
      if chars.tab_fix then
        for charcol, fix in pairs(chars.tab_fix) do
          if fix.value <= 0 then fix.value = fix.value + fix.width end
          if fix.value < fix.width then chars[charcol] = (" "):rep(fix.value) end
        end
      end
      chars.eol_fix = chars.eol_fix and (chars.eol_fix < 0 and 1 or chars.eol_fix) or 0

      -- NOTE: When `start` or `finish` equals `#chars + 1` (beyond the array bounds), attempting to access
      -- `chars[start]` or `chars[finish]` would cause out-of-range errors. To prevent exceptions in `table.concat`,
      -- ensure `finish` is set to a value less than `start` in such cases. This guarantees the function safely returns
      -- an empty string when the specified range is invalid.

      local start = i == 1 and edges[1].charpos[3] or 1
      local finish = i == #chars_list and math.min(edges[2].charpos[3], #chars) or #chars
      local id = i == #chars_list and #lines or i
      local text = table.concat(chars, nil, start, finish) .. vchar:rep(chars.eol_fix)

      local extras = {}
      if start > 1 then
        local extra = table.concat(chars, nil, 1, start - 1)
        if vim.tbl_get(chars, "tab_fix", start) then extra = extra .. (" "):rep(chars.tab_fix[start].offset) end
        extras[1] = extra
      end
      if finish < #chars then
        local extra = table.concat(chars, nil, finish + 1)
        if vim.tbl_get(chars, "tab_fix", finish) then
          local tab = chars.tab_fix[finish]
          extra = (" "):rep(tab.width - tab.offset - tab.value) .. extra
        end
        extras[2] = extra
      end

      if #chars.edges == 1 then table.insert(chars.edges, 3 - i, i - 2) end
      region[region.start_row + id - 1] = {
        chars.edges[1],
        chars.edges[2],
        text = text,
        line = lines[id],
        extras = extras,
      }
    end

    for l = region.start_row + 1, region.end_row - 1 do
      local id = l - region.start_row + 1
      region[l] = { 0, -1, text = lines[id], line = lines[id], extras = {} }
    end
  elseif (M.is_visual_mode(true, regtype) or 255) < 32 then -- block-wise
    ---@type { [integer]: string, edges: [{ charpos: VimPosition, col: integer }, { charpos: VimPosition, col: integer }], width: integer, tab_fix: table<integer, { value: integer, width: integer, offset: integer }>?, eol_fix: integer }[]
    local chars_list = vim.tbl_map(function(line) return fn.split(line, [[\zs]]) end, lines)

    local vcol_start = M.virtcol({ edges[1].charpos[2], edges[1].col + 1, edges[1].charpos[4] }, true, buf, regtype)
    local vcol_end = M.virtcol({ edges[2].charpos[2], edges[2].col + 1, edges[2].charpos[4] }, true, buf, regtype)

    local virtcols = { math.min(vcol_start[1], vcol_end[1]) } ---@type [integer, integer]

    local curswant = edges[1].charpos[5] or edges[2].charpos[5] or 0

    if curswant == vim.v.maxcol then
      virtcols[2] = 0
      for i, chars in ipairs(chars_list) do
        chars.width = fn.strdisplaywidth(lines[i])
        if chars.width > virtcols[2] then virtcols[2] = chars.width end
      end
      virtcols[2] = virtcols[2] + 1
    else
      virtcols[2] = math.max(vcol_start[2], vcol_end[2])
    end

    if not inclusive and vcol_start[2] < vcol_end[1] and curswant < vim.v.maxcol then virtcols[2] = vcol_end[1] - 1 end

    for i, line in ipairs(lines) do
      local chars = chars_list[i]
      chars.width = chars.width or fn.strdisplaywidth(line)

      local lnum = region.start_row + i

      chars.edges = {}

      for id = 1, 2 do
        local col = virtcols[id] > chars.width and #line + 1 or M.virtcol2col(buf, lnum, virtcols[id])
        local ccol = fn.charidx(line, col - 1) + 1
        local vcol = M.virtcol({ lnum, col }, true, buf, regtype)
        local off = (ccol > #chars or chars[ccol] == "\t") and virtcols[id] - vcol[1] or 0
        chars.edges[id] = {
          charpos = { edges[1].charpos[1], lnum, ccol, off },
          col = col - 1, -- 0-indexed
        }
      end

      if inclusive then
        local end_col = virtcols[2] >= chars.width and #line + 1 or M.virtcol2col(buf, lnum, virtcols[2] + 1)
        chars.edges[2].col = end_col - 1
      end

      for id, edge in ipairs(chars.edges) do
        local charcol, off = edge.charpos[3], edge.charpos[4]
        if chars[charcol] == "\t" then
          local tab_width = fn.strdisplaywidth("\t", virtcols[id] - off - 1)

          local tab_fix = (-1) ^ id * off + id - 1
          chars.tab_fix = chars.tab_fix or {}
          if chars.tab_fix[charcol] then
            chars.tab_fix[charcol].value = chars.tab_fix[charcol].value + tab_fix
          else
            chars.tab_fix[charcol] = { value = tab_fix, width = tab_width, offset = (2 - id) * off }
          end
        elseif charcol > #chars then
          local eol_fix = (-1) ^ id * off + id - 1
          chars.eol_fix = chars.eol_fix and chars.eol_fix + eol_fix or eol_fix
        end
      end
    end

    local virtualedit = vim.list_contains(opt_ve, "block") or vim.list_contains(opt_ve, "all")

    for i, chars in ipairs(chars_list) do
      if chars.tab_fix then
        for charcol, fix in pairs(chars.tab_fix) do
          if fix.value <= 0 then fix.value = fix.value + fix.width end
          if fix.value < fix.width then chars[charcol] = (" "):rep(fix.value) end
        end
      end
      chars.eol_fix = chars.eol_fix and (chars.eol_fix < 0 and 1 or chars.eol_fix) or 0
      if not virtualedit and virtcols[1] <= chars.width then chars.eol_fix = 0 end

      -- NOTE: Since the range of values for `start` and `finish` are `[1, #char + 1]`, when `start` and `finish` are
      -- `#char + 1`, `chars[start]` and `chars[finish]` do not exist, which can cause exceptions in the `table.concat`
      -- function. Therefore, it is necessary to make `finish` less than `start` so that the `table.concat` function
      -- will return a correct empty string.

      local start = chars.edges[1].charpos[3]
      local finish = math.min(chars.edges[2].charpos[3], #chars)

      local text = table.concat(chars, nil, start, finish) .. (" "):rep(chars.eol_fix)

      local extras = {}
      if start > 1 then
        local extra = table.concat(chars, nil, 1, start - 1)
        if vim.tbl_get(chars, "tab_fix", start) then extra = extra .. (" "):rep(chars.tab_fix[start].offset) end
        extras[1] = extra
      end
      if finish < #chars then
        local extra = table.concat(chars, nil, finish + 1)
        if vim.tbl_get(chars, "tab_fix", finish) then
          local tab = chars.tab_fix[finish]
          extra = (" "):rep(tab.width - tab.offset - tab.value) .. extra
        end
        extras[2] = extra
      end

      region[region.start_row + i - 1] = {
        chars.edges[1].col,
        chars.edges[2].col,
        text = text,
        line = lines[i],
        extras = extras,
      }
    end
  elseif regtype == "V" or regtype == "S" then
    for l = region.start_row, region.end_row do
      local id = l - region.start_row + 1
      region[l] = { 0, -1, text = lines[id], line = lines[id], extras = {} }
    end
  end

  return region, function(t) return ({ ipairs(t) })[1], t, t.start_row - 1 end
end

---
---Get text within a selected region (i.e. the highlighted text in visual or select mode, or the word under the cursor
---in other modes)
---
---p.s. Why is this not a built-in Vim script function?!
---
---@param last_selected? boolean Whether use the last selected region when current mode is not visual or select
---@return string
function M.get_visual_selection(last_selected)
  local pos1, pos2, mode

  if M.is_visual_mode(true) then
    pos1, pos2, mode = "v", ".", fn.mode()
  elseif last_selected then
    pos1, pos2, mode = "'<", "'>", fn.visualmode()
  else
    return fn.expand("<cword>")
  end

  local region, _ipairs = M.region(0, pos1, pos2, mode, vim.o.selection == "inclusive")

  if not _ipairs then return fn.expand("<cword>") end

  return vim.iter(_ipairs(region)):fold("", function(s, _, r) return s .. "\n" .. r.text end):sub(2)
end

return M
