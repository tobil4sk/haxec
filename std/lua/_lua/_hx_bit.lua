-- lua 5.3 has native bitwise operators
local has_native_ops, native_ops = pcall(load, [[
    return {
      bnot = function(x) return ~x end,
      band = function(x, y) return x & y end,
      bor = function(x, y) return x | y end,
      bxor = function(x, y) return x ^ y end,
      lshift = function(x, y) return x << y end,x
      rshift = function(x, y) return x >> y end,
      arshift = function(x, y) return x >> y end,
      mod = function(x, y) return x % y end
    }
]])

if has_native_ops and native_ops then
  _hx_bit_raw = native_ops
elseif _G.bit32 or pcall(require, 'bit32') then
  -- lua 5.2 and 5.3 have bit32 builtin, or it maybe be an external library on 5.1
  _hx_bit_raw = _G.bit32 or require('bit32')
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  -- bit32 operations require manual clamping
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end
  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end
  -- see https://github.com/HaxeFoundation/haxe/issues/8849
  _hx_bit.bor = function(...) return _hx_bit_clamp(_hx_bit_raw.bor(...)) end
  _hx_bit.band = function(...) return _hx_bit_clamp(_hx_bit_raw.band(...)) end
  _hx_bit.arshift = function(...) return _hx_bit_clamp(_hx_bit_raw.arshift(...)) end
  _hx_bit.lshift = function(...) return _hx_bit_clamp(_hx_bit_raw.lshift(...)) end
elseif _G.bit or pcall(require, 'bit') then
  --If we do not have bit32, fallback to 'bit', default on luajit
  _hx_bit_raw = _G.bit or require('bit')
else
  _hx_bit_raw = setmetatable({}, {__index = function()
    error("Failed to load bit or bit32")
  end})
end
_hx_bit = _hx_bit or _hx_bit_raw
