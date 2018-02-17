pico-8 cartridge // http://www.pico-8.com
version 15
__lua__
-- picohistory: spacewar
-- by david heyman

--=============
-- utils
--=============
-- debugging
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
	
-->8
--========
-- hooks
--========
function _init()
	ships = {ship(0), ship(1)}
	shots = {}
	splosions = {}
end

function _update()
	if ships[1].crashed or ships[2].crashed then
			-- let all explosions finish, then wait one second.
			if endgame == 0 then
				extcmd('reset')
			elseif #splosions == 0 then
				endgame -=1
			end
	else
		foreach(ships, ship.update)
		foreach(shots, shot.update)
	end
	foreach(splosions, splode.update)
	-- remove things
	shots = filter(shots,
		function(a) return not a.crashed end)
	splosions = filter(splosions,
		function(a) return not a.done end)
end

function _draw()
	cls()
	spr(18, 56, 56, 2, 2)
	foreach(ships, ship.draw)
	foreach(shots, shot.draw)
	foreach(splosions, splode.draw)
end
-->8
--===========
-- ships, shots, splosions
--===========
bounds = 128
starcenter = vec2(64,64)
starsize = 8 -- radius, not dia
acc = 0.1
endgame = 60 -- endgame timer

-- physics functions
-- accel due to grav, by pos
function grav(pos)
	local h = starcenter - pos
	local dist = h:mag()
	local	head = h:unit()
	return head * (5/(dist*dist))
end

-- basic physics update
-- returns whether star collision
function phys(self)
	if self.pos == nil then error() end
	self.vel += grav(self.pos)
	local npos = self.pos + self.vel
	if npos.x < 0 or
			npos.x > bounds then
		npos.x	= npos.x % bounds
	end
	if npos.y < 0 or
			npos.y > bounds then
		npos.y	= npos.y % bounds
	end
	self.pos = npos
	self.hitbox = vec2(self.hitbox.w, self.hitbox.h):at(npos)
	return starcoll(self)
end

-- star collision
function starcoll(self)
	return (self.hitbox:center() -
		starcenter):mag() <
		(3 + starsize)
end

-- specific entities
ship = class({})

-- precomputed constants
headings = {
	heading(1/8),
	heading(1/4),
	heading(3/8),
	heading(1/2),
	heading(5/8),
	heading(3/4),
	heading(7/8),
	heading(1)
}
-- convert angle to sprite number
shipsprites = {
	[0]={2,1,8,7,6,5,4,3},
	[1]={10,9,16,15,14,13,12,11}
}

function ship:init(pnum)
	self.ammo = 5
	self.fuel = 100
	self.vel = vec2(0,0)
	self.hyp = 1
	self.pnum = pnum
	self.pos = vec2(rnd(bounds-7),
		rnd(bounds-7))
	-- heading angle is a number from 1 to 8
	self.head_angle = flr(rnd(8))+1
	self.hitbox = vec2(7, 7):at(self.pos)
end

function ship:__index(key)
	if key == 'heading' then
		return headings[flr(self.head_angle)]
	else
		return ship[key]
	end
end

function ship:update()
	self.crashed = phys(self)
	if self.crashed then
		splode:init(self.pos)
	end
	local pnum = self.pnum
	if btn(0, pnum) then
		-- left
		self.head_angle = (self.head_angle - 0.8) % 8 + 1
	elseif btn(1, pnum) then
		-- right
		self.head_angle = (self.head_angle - 1.2) % 8 + 1
	elseif btn(2, pnum) then
		-- up
		self:thrust()
	elseif btnp(3, pnum) then
		-- down
		self:jump()
	elseif btnp(4, pnum) then
		-- o
		self:shoot()
	-- shot coll handled by shots
	end
end

function ship:draw()
	if not self.crashed then
		spr(shipsprites[self.pnum][flr(self.head_angle)],
			self.pos.x, self.pos.y)
	end
end

function ship:thrust()
	if self.fuel then
		self.vel = self.vel + (self.heading * acc)
		self.fuel -= 1
	end
end

function ship:shoot()
	if self.ammo > 0 then
		-- todo shot should come from nose of ship
		-- todo use a precalc table based on int direction
		local npos = self.hitbox:center() + (self.heading*10)
		shot(npos, self.vel, self.heading)
		self.ammo -= 1
	end
end

-- randomize position, but each
-- use is more likely to land
-- in the star (centered).
function ship:jump()
	self.pos = vec2(
		beta(self.hyp, self.hyp, bounds),
		beta(self.hyp, self.hyp, bounds))
	self.hyp += 1
end

shot = class()
function shot:init(pos, shooter_vel, shooter_heading)
	self.pos = pos:copy()
	self.vel = shooter_vel + shooter_heading * (acc * 10)
	self.hitbox = vec2(3,3):at(self.pos)
	shots[#shots + 1] = self
end

function shot:update()
	self.crashed = phys(self)
	if self.crashed then
		splode:init(self.pos)
	else
		for i=1,2 do
			if ships[i].hitbox:overlaps(self.hitbox) then
				ships[i].crashed = true
				splode(self.pos)
			end
		end
	end
end

function shot:draw()
	if not self.crashed then
		spr(17,
			self.pos.x, self.pos.y)
	end
end

splode = class()
effects = {20, 21, 22, 23, 24, 25, 26}
function splode:init(pos)
	self.pos = pos:copy()
	self.t = 0
	splosions[#splosions + 1] = self
end

function splode:update()
	self.t += 0.1
	if self.t > #effects then
		self.done = true
	end
end

function splode:draw()
	spr(effects[flr(self.t)+1],
		self.pos.x, self.pos.y)
end
__gfx__
000000000006000000000060000000000600000000666000000006000000000060000000000f000000000ff000000000000ff0000fffff0000ff000000000000
000000000006000000000600000000006c600000006c600000006c600000000006000000000f00000000fff0ff00000000fff0000f999f0000fff00000000ff0
007007000006000000006000666000000660000000666000000066000000666000600000000f0000ffffff00f9ff00000ff9f00000f9f00000f9ff00000ff9f0
0007700000060000000600006c66666000060000000600000006000066666c600006000000fff000ff99f000f99ffff0ff99f00000fff00000f99ff0ffff99f0
00077000006660000660000066600000000060000006000000600000000066600000660000f9f0000ff9f000f9ff0000ffffff00000f00000ffffff0000ff9f0
00700700006c60006c600000000000000000060000060000060000000000000000006c600f999f0000fff000ff0000000000fff0000f0000fff0000000000ff0
0000000000666000060000000000000000000060000600006000000000000000000006000fffff00000ff0000000000000000ff0000f0000ff00000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ff0000008880000000000aaaaaa00000000000000000000000000000808008082120021255500555000000000000000000000000000000000000000000000000
fff0000088800000000aaaaaaaaaa000000000000000000009000090090000901410014155500555050000500000000000000000000000000000000000000000
0ffffff08880000000aaaaaaaaaaaa00000000000090090000900900808008082120021255500555000000000000000000000000000000000000000000000000
00f99ff0000000000aaa44aaaaaaaaa000077000000aa00000000000000000000000000000000000000000000000000000000000000000000000000000000000
00f9ff00000000000aaa444aaaa7aaa000077000000aa00000000000000000000000000000000000000000000000000000000000000000000000000000000000
00fff00000000000aaaaaaaaaa7a7aaa000000000090090000900900808008082120021255500555000000000000000000000000000000000000000000000000
00ff000000000000aaaa7aaaaaa7aaaa000000000000000009000090090000901410014155500555050000500000000000000000000000000000000000000000
0000000000000000aaaaaaaaaaaaaaaa000000000000000000000000808008082120021255500555000000000000000000000000000000000000000000000000
0000000000000000aaaaaaaaaaaaaaaa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000aaaaa7aaaaaaaaaa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000aaaaaaaaaaaaaaaa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000007aaaaaa4aaaaaa0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000aaaaaaa44aaaaa0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000aaaaaaaaaaaa00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000aaaaaaaaaa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000aaaaaa00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
