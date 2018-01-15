pico-8 cartridge // http://www.pico-8.com
version 15
__lua__
-- picohistory: pong
-- by david heyman
-- engine mostly lifted from
-- under construction
-- by glip and eevee

--============================--
--==== begin engine code =====--
--============================--

--------------------------------
-- bad constants
animframedelay = 4

--------------------------------
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

--------------------------------
-- misc utilities

-- smallest possible value
-- 1/256/256 == 1/65536
minfrac = 0.0000152587890625

function ceil(n)
	return flr(n + 1 - minfrac)
end

-- copy values from b into a
function merge(a, b)
	for k, v in pairs(b) do
		a[k] = v
	end
	return a
end

function sort(list, keyfunc)
	-- insertion sort
	-- stable, fast enough for
	-- small lists
	local keys = {}
	keyfunc = keyfunc or function(v) return v end
	for i = 1, #list do
		keys[i] = keyfunc(list[i])
	end
	for i = 2, #list do
		local j = i
		while j > 1 and keys[j - 1] > keys[j] do
			keys[j], keys[j - 1] = keys[j - 1], keys[j]
			list[j], list[j - 1] = list[j - 1], list[j]
			j -= 1
		end
	end
end

--------------------------------
-- micro class type

function class(base, proto)
	proto = proto or {}
	proto.__index = proto
	local meta = {}
	setmetatable(proto, meta)
	if base then
		meta.__index = base
	end
	function meta:__call(...)
		local this = setmetatable({}, self)
		if this.init then
			this:init(...)
		end
		return this
	end
	return proto
end

--------------------------------
-- vec2 type (point or size)

vec2 = class()

function vec2:init(x, y)
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
	return self + other * -1
end

-- element-wise multiplication
function vec2:elemx(other)
	return vec2(
		self.x * other.x,
		self.y * other.y)
end

function vec2:at(anchor)
	return box(
		anchor.x, anchor.y,
		self.x, self.y)
end

function vec2:max()
	return max(self.x, self.y)
end

function vec2:len()
	return sqrt(self.x * self.x + self.y * self.y)
end

function vec2:normalize()
	local x = self.x / self:len()
	local y = self.y / self:len()
	return vec2(x, y)
end

function heading(angle)
	return vec2(cos(angle), sin(angle))
end

-- box type (rectangle)

box = class()

function box:init(l, t, w, h)
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
	return "<box " .. self.l .. ", " .. self.t .. " to " .. self.r .. ", " .. self.b .. ">"
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

function box:__add(offset)
	return box(self.l + offset.x, self.t + offset.y, self.w, self.h)
end

function box:overlaps(other)
	-- don't count touching edges
	-- as overlapping; the right
	-- and bottom are exclusive
	return (
		self.l < other.r and
		self.r > other.l and
		self.t < other.b and
		self.b > other.t
	)
end

-- distance between boxes;
-- lengths are always positive,
-- unless boxes overlap
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

--------------------------------
-- actor type

-- note that the base unit is the
-- tile (8px), not the pixel, as
-- that's what the map uses

actor = class{
	__name = "actor",
	enabled = true,
	-- collision box, relative to
	-- top-left corner
	shape = box(0, 0, 8, 8),
}

function actor:init(pos)
	self.pos = pos
	self:reset()
end

-- called when the map is
-- reloaded
function actor:reset()
end

-- returns collision box
function actor:coll()
	return self.shape + self.pos
end

-- called when a mobactor first
-- bumps into this one, even if
-- not blocked.  delta is how
-- fast they're colliding, which
-- may or may not be meaningful,
-- but does indicate direction
function actor:oncollide(other, delta)
end

-- called every tic
function actor:update()
end

-- called every tic, but only
-- if actor is visible; this
-- should not change state!
-- by default actors are
-- assumed to be part of the
-- map and drawn by map()
function actor:draw()
end

--------------------------------
-- actor that can move, checks collision
mobactor = class(actor, {
	__name = "mobactor",
	is_mobile = true,
	-- physics
	min_speed = 0.015625,  -- 1/64
	max_speed = 1.25,
})

function mobactor:init(pos)
	self.pos0 = self.pos0 or pos
	actor.init(self, pos)
end

function mobactor:reset()
	actor.reset(self)

	-- reset movement stuff,
	-- including original pos
	self.pos = self.pos0
	self.vel = heading(rnd(1)) + vec2(0.5, 0)
end

function mobactor:update()
	local abs, vec2 = abs, vec2

	-- speed adjustments
	if self.vel:len() > self.max_speed then
		self.vel = self.vel:normalize() * self.max_speed
	end
	local vel = self.vel

	-- ok, now deal with movement
	-- velocity is in tiles per
	-- tic, so every tic, move by
	-- the velocity
	local move = vel:copy()

	-- round to a tile when very
	-- close to one, so actors
	-- can fall down gaps easily,
	-- but only when slowing down
	if abs(vel.x) < abs(prevx) then
		local goalx = self.pos.x + move.x
		local d1 = goalx - flr(goalx + 0.5)
		-- 1/16 is half a pixel
		if abs(d1) < 1/16 then
			move.x -= d1
		end
	end

	local coll = self:coll()
	local delta = vec2(
		abs(move.x),
		abs(move.y))
	local sign = vec2(
		sgn(move.x),
		sgn(move.y))

	-- trim to map bounds
	local nearx = sign.x > 0 and "r" or "l"
	local neary = sign.y > 0 and "b" or "t"
	local boundx = abs(field_bounds[nearx] - coll[nearx])
	if delta.x > boundx then
		-- ball hit end, score increments
		local m = {r="x", l="y"}
		game.point = m[nearx]
		self:reset()
		return
	end
	local boundy = abs(field_bounds[neary] - coll[neary])
	if delta.y > boundy then
		delta.y = boundy
		vel.y = -vel.y
	end
	-- find actors we might hit
	local blockers = {}
	local moverange = coll + vec2()
	if sign.x > 0 then
		moverange.w += delta.x
	else
		moverange.l -= delta.x
	end
	if sign.y > 0 then
		moverange.h += delta.y
	else
		moverange.t -= delta.y
	end
	local function add_blocker(other)
		if not other.enabled then
			return
		end
		if other.oncollide == actor.oncollide then
			return
		end
		local otherbox = other:coll()
		if not coll:overlaps(otherbox) and moverange:overlaps(otherbox) then
			local dist = coll:dist(otherbox)
			-- sort in roughly the order we'll encounter actors
			local order = dist.x * delta.y + dist.y * delta.x
			add(blockers, {
				actor = other,
				dist = dist,
				order = order,
				coll = otherbox,
			})
		end
	end
	for actor in all(game.mobs) do
--		if actor ~= self and
--			abs(self.pos.x - actor.pos.x) + abs(self.pos.y - actor.pos.y) <= 8
--		then
			add_blocker(actor)
--		end
	end
	sort(blockers, function (blocker)
		return blocker.order
	end)
	local movedby = vec2()

	for blocker in all(blockers) do
		-- step 1: move to touch the
		-- other actor
		local dist = blocker.dist - movedby
		dist.x = max(0, dist.x)
		dist.y = max(0, dist.y)
		local ymult = delta.y * dist.x
		local xmult = delta.x * dist.y
		local step = dist:copy()
		if dist.x == 0 or dist.y == 0 then
			-- already touching; no trim
		elseif ymult > xmult then
			-- touch side first, so trim y distance
			step.y = delta.y * dist.x / delta.x
		elseif xmult > ymult then
			-- touch top/bottom first, so trim x distance
			step.x = delta.x * dist.y / delta.y
		end

		delta -= step
		movedby += step
		coll += (step:elemx(sign))

		-- step 2: if we were about
		-- to collide, do oncollide,
		-- and stop if solid.
		local newdist = coll:dist(blocker.coll)
		local touchx = newdist.x == 0
		local touchy = newdist.y == 0
		if touchx or touchy then
			if touchx and touchy then
				-- "corner" case, ho ho!
				-- allow sliding in the
				-- direction that we're
				-- moving fastest in
				if delta.x > delta.y then
					touchx = false
				else
					touchy = false
				end
			end

			local force = move:copy()
			if not touchx or delta.x == 0 then
				force.x = 0
			end
			if not touchy or delta.y == 0 then
				force.y = 0
			end
			blocker.actor:oncollide(self, force)
		end
	end
	-- leftover isn't blocked
	movedby += delta
	self.pos += (movedby:elemx(sign))
end

function mobactor:draw()
	spr(self.sprite, self.pos.x, self.pos.y)
end

--------------------------------
-- player actor type
-- main difference is that it
-- reads input every frame;
-- it also has is_player,
-- which other actors may check
-- for
playeractor = class(mobactor, {
	__name = "playeractor",
	is_player = true,
	collide_check = false,
	shape = box(0, 0, 1, 24),
	-- controls
	accel = 0.75,
})

function playeractor:reset()
	mobactor.reset(self)
	self.vel = vec2()
end

function playeractor:draw()
	-- is a player, which in this game means 5-high paddle
	for i=0,2 do
		spr(self.sprite, self.pos.x, self.pos.y + i*8)
	end
end

-- move up and down only. stop when buttons released.
function playeractor:update()
	if btn(2, self.pnum) then
		self.vel.y -= self.accel
	elseif btn(3, self.pnum) then
		self.vel.y += self.accel
	else
		self.vel.y = 0
	end
	-- simpler update if this doesn't check collision
	if self.collide_check then
		mobactor.update(self)
	else
		local abs, vec2 = abs, vec2

		-- speed adjustments
		if self.vel:len() > self.max_speed then
			self.vel = self.vel:normalize() * self.max_speed
		end
		local vel = self.vel

		-- ok, now deal with movement
		-- velocity is in tiles per
		-- tic, so every tic, move by
		-- the velocity
		local move = vel:copy()

		-- round to a tile when very
		-- close to one, so actors
		-- can fall down gaps easily,
		-- but only when slowing down
		if abs(vel.x) < abs(prevx) then
			local goalx = self.pos.x + move.x
			local d1 = goalx - flr(goalx + 0.5)
			-- 1/16 is half a pixel
			if abs(d1) < 1/16 then
				move.x -= d1
			end
		end

		local coll = self:coll()
		local delta = vec2(
			abs(move.x),
			abs(move.y))
		local sign = vec2(
			sgn(move.x),
			sgn(move.y))

		-- trim to map bounds
		local nearx = sign.x > 0 and "r" or "l"
		local neary = sign.y > 0 and "b" or "t"
		local boundx = abs(field_bounds[nearx] - coll[nearx])
		if delta.x > boundx then
			delta.x = boundx
			vel.x = 0
		end
		local boundy = abs(field_bounds[neary] - coll[neary])
		if delta.y > boundy then
			delta.y = boundy
			vel.y = 0
		end
		self.pos += (delta:elemx(sign))
	end
end
-- singleton
game = {} -- contents are game-specific

--------------------------------
-- pico-8 engine hooks

function _init()
	game:initialize()
end

function _update()
	game:update()
end

function _draw()
	game:draw()
end

--============================--
--===== end engine code ======--
--============================--

-- counter to provide pause before ball turns up
game.counter = 3
-- playable area bounds
field_bounds = box(8,46,112,80)

function game:initialize()
	self.score = vec2(0, 0)
	local p0, p1, ball = playeractor(vec2(108, 80)), playeractor(vec2(14, 80)), mobactor(vec2(60, 94))
	p0.pnum = 0
	p1.pnum = 1
	ball.vel = vec2(-ball.max_speed, 0)

	function paddlecol(paddle, other, force)
		local rel_y = abs((paddle.pos - other.pos).y)
		-- now convert to correct angle
		local a
		if rel_y < 10 then
			a = 0.125
--		elseif rel_y < 6/5 then
--			a = .9375
		elseif rel_y < 14 then
			a = 0
--		elseif rel_y < 12/5 then
--			a = 0.0625
		else
			a = .875
		end
		local h = heading(a)
		-- flip x direction
		h.x = h.x * -sgn(force.x) * 1.25
		other.vel = h
	end
	p0.oncollide = paddlecol
	p1.oncollide = paddlecol
	p0.sprite = 10
	p1.sprite = 10
	ball.sprite = 11
	self.mobs = {p0, p1, ball}
end

function game:update()
	for a in all(self.mobs) do
		a:update()
	end
	local point = self.point
	if point then
		self.score[point] = self.score[point] + 1
		if self.score[point] > 9 then
			self.score = vec2()
		end
	end
	self.point = nil
end

function game:draw()
	cls()
	map(0, 5, 0, 38, 16, 12)
	for a in all(self.mobs) do
		a:draw()
	end
	local scoreloc = self.score * 3
	map(scoreloc.x, 0, 24, 0, 3, 5)
	map(scoreloc.y, 0, 72, 0, 3, 5)
end

__gfx__
00000000777777770000007700000000770000000000000000000077000000000000000770000000770000007777777700000000000000000000000000000000
00000000777777770000007700000000770000000000000000000077000000000000000770000000770000007777777700000000000000000000000000000000
00700700000000000000007700000000000000000000000000000000000000000000000770000000770000007777777700000000000000000000000000000000
00077000000000000000007700000000000000000000000000000000000000000000000000000000770000007777777700000000000000000000000000000000
00077000000000000000007700000000000000000000000000000000000000000000000770000000770000007777777700000000000000000000000000000000
00700700000000000000007700000000000000000000000000000000000000000000000770000000770000007777777700000000000000000000000000000000
00000000000000000000007777000000000000000000007700000000777777770000000770000000770000007777777700000000000000000000000000000000
00000000000000000000007777000000000000000000007700000000777777770000000000000000770000007777777700000000000000000000000000000000
__gff__
0001010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0b0b0b000b000b0b0b0b0b0b0b000b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0b000b000b0000000b00000b0b000b0b00000b000000000b0b000b0b000b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0b000b000b000b0b0b000b0b0b0b0b0b0b0b0b0b0b00000b0b0b0b0b0b0b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0b000b000b000b000000000b00000b00000b0b000b00000b0b000b00000b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0b0b0b000b000b0b0b0b0b0b00000b0b0b0b0b0b0b00000b0b0b0b00000b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0507070707070707070707070707070300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0200000000000008090000000000000a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0601010101010101010101010101010400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
