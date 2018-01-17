--===========
-- debugging
--===========
reprdepth = 0
function _tablerepr(t)
	if reprdepth > 1 then
		return "<table>"
	end

	reprdepth += 1
	local first = true
	local ret = "{"
	for k, v in pairs(t) do
		if not first then
			ret = ret .. ", "
		end
		first = false
		local rv
		if v == t then
			rv = "<...>"
		else
			rv = repr(v)
		end
		ret = ret .. repr(k) .. ": " .. rv
	end
	reprdepth -= 1
	return ret .. "}"
end

function repr(t)
	if t == true then
		return "<true>"
	elseif t == false then
		return "<false>"
	elseif t == nil then
		return "<nil>"
	elseif type(t) == "function" then
		return "<function>"
	elseif type(t) ~= "table" then
		return "" .. t
	elseif t.__repr then
		return t:__repr()
	elseif t.__name then
		return "<object/" .. t.__name .. " " .. _tablerepr(t) .. ">"
	elseif t.new then
		return "<object " .. _tablerepr(t) .. ">"
	else
		return _tablerepr(t)
	end
end

function spew(arg)
	local out = ""
	for s in all(arg) do
		out = out .. repr(s) .. " "
	end
	printh(out)
end

-->8
--=============
-- utils
--=============
function sort(args, keyfunc)
	-- insertion, good enough
	local keys = {}
	keyfunc = keyfunc or function(v) return v end
	for i = 1, #keys do
		keys[i] = keyfunc(list[i])
	end
	for i = 2, #keys do
		local j = i
		while j > 1 and keys[j - 1] > keys[j] do
			keys[j], keys[j - 1] = keys[j - 1], keys[j]
			list[j], list[j - 1] = list[j - 1], list[j]
			j -= 1
		end
	end
end

-- list filtering
-- not in-place
function filter(list, condition)
	local t = {}
	for i=1,#list do
		local b = list[i]
		if condition(b) then
			t[#t+1] = list[i]
		end
	end
	return t
end

function any(list, condition)
	for i=1,#list do
		if condition(list[i]) then
			return true
		end
	end
	return false
end
	
-- beta distribution, for small
-- positive integer a and b.
-- note that beta(3, 3) is
-- close-enough to gaussian,
-- except bounded in [0,1]
function beta(a,b, range)
	local vals = {}
	for i=1,(a+b-1) do
		vals[i] = rnd(range)
	end
	sort(vals)
	return vals[a]
end

-- classes
function class(base, proto)
	proto = proto or {}
	proto.__index = proto
	local meta = {__index = base}
	setmetatable(proto, meta)
	function meta:__call(...)
		local this = setmetatable({}, self)
		if this.init then
			this:init(...)
		end
		return this
	end
	return proto
end

-- for xy coordinates, etc
vec2 = class()
function vec2:init(x,y)
	self.x = x or 0
	self.y = y or 0
end

function vec2:copy()
	return vec2(self.x, self.y)
end

function vec2:__repr()
	return "<vec2 " .. self.x .. ", " .. self.y .. ">"
end

function vec2:__add(other)
	return vec2(
		self.x + other.x,
		self.y + other.y)
end

function vec2:__mul(n)
		return vec2(
		self.x * n,
		self.y * n)
end

function vec2:__sub(other)
	-- if performance is problem,
	-- can inline (cost: symcount)
	return self + other * -1
end

-- paired multiplication
function vec2:elemx(other)
	return vec2(
		self.x * other.x,
		self.y * other.y)
end

-- magnitude
function vec2:mag()
	return sqrt(self.x * self.x +
		self.y * self.y)
end

-- box at other, this size
function vec2:at(anchor)
	return box(
		anchor.x, anchor.y,
		self.x, self.y)
end

-- get unit vectors
function vec2:unit()
	local len = sqrt(
		self.x * self.x +
		self.y * self.y)
	return vec2(self.x / len,
		self.y / len)
end

function heading(angle)
	return vec2(cos(angle), sin(angle))
end

-- random point on unit circle
function randir()
	local p
	repeat
	p = vec2(beta(3,3),
		beta(3,3)):unit()
	until(p.x > 0 or p.y > 0)
	return p
end

-- rectangles
box = class()

function box:init(l,t,w,h)
	if w < 0 then
		l += w
		w *= -1
	end
	if h < 0 then
		t += h
		h *= -1
	end
	self.l = l
	self.t = t
	self.w = w
	self.h = h
end

function box:__repr()
	return "<box at (" ..
		self.l .. "," .. self.t .. "), (" ..
		self.w .. " by " .. self.h .. ")>"
end

function box.__index(self, key)
	if key == "r" then
		return self.l + self.w
	elseif key == "b" then
		return self.t + self.h
	else
		return box[key]
	end
end

function box:center()
	return vec2(self.l + self.w/2,
		self.t + self.h/2)
end

-- adjust box location
function box:__add(offset)
	return box(self.l + offset.x,
		self.t + offset.y,
		self.w, self.h)
end

-- check for overlaps. note
-- that it must be more
-- than the very edge.
function box:overlaps(other)
	return self.l < other.r and
		self.r > other.l and
		self.t < other.b and
		self.b > other.t
end

-- distance from self edge
-- to other edge
function box:dist(other)
	local x, y
	if self.l < other.l then
		x = other.l - self.r
	else
		x = self.l - other.r
	end
	if self.t < other.t then
		y = other.t - self.b
	else
		y = self.t - other.b
	end
	return vec2(x, y)
end

-- distance from self center
-- to other center
function box:cdist(other)
	return self:center():dist(other:center())
end

-->8
--========
-- hooks
--========
function _init()
end

function _update()
end

function _draw()
	cls()
end
-->8
