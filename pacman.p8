pico-8 cartridge // http://www.pico-8.com
version 15
__lua__
-- picohistory: pacman
-- david heyman

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

-- round to integer
function vec2:cell()
	return vec2(flr(self.x + 0.5), flr(self.y + 0.5))

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

-->8
--========
-- hooks and globals
--========
local ents
local level = 1
local win = false
local power = 0
local cur_timer = 0
local timers
local chase_mode = true
local mode_frame = false

function _init()
	ents = {
		pacman(13.5, 22),
		blinky(11.5, 13.5),
		pinky(12.5, 13.5),
		inky(13.5, 13.5),
		clyde(14.5, 13.5)
	}
	cur_timer = 0
	timers = {210, 600, 210, 600, 150, 600, 150}
end

function _update()
	if cur_timer <= 0 then
		cur_timer = timers[1]
		del(timers, cur_timer)
		chase_mode = not chase_mode
		mode_frame = true
	elseif power <= 0 then
		cur_timer -= 1
	else
		power -= 1
	end
	if any(ents, function(t) return t:update() end) then
		-- endgame stuff
		level += 1
		-- todo
	end
	mode_frame = false
end

function _draw()
	cls()
	for e in all(ents) do
		e:draw()
	end
end
-->8
function wrap(pos)
	pos.x = pos.x % 224
end

thing = class()
function thing:init(x, y)
	self.pos = vec2(x*8, y*8)
	self._updater = cocreate(self.loop)
end

function thing:update()
	coresume(self._updater)
	return costatus(self._updater)
end

-- directions are enumerated to match the buttons
-- but with 4 for not-moving
directions = {
	0 = vec2(-1, 0),
	1 = vec2(1, 0),
	2 = vec2(0, -1),
	3 = vec2(0, 1),
	4 = vec2(0, 0)
}
directions[0] = vec2(0, 0)

function thing:move(cell, dir, flag, vel)
	local target = cell + directions[dir]
	-- check cell for walls (having flag)
	if not fget(mget(target.x, target.y), flag) then
		self.pos += (directions[dir] * vel)
	end
end

function thing:draw()
	-- need to account for centering
	spr(self.spr, self.pos.x - 8,
		self.pos.y - 8, 2, 2,
		self.flipx, self.flipy)
end

pacman = class(thing)
function pacman:draw()
	-- todo set flipx and flipy and sprite
	thing.draw(self)
end

function pacman:loop()
	local dir = 4 -- 4 indicates not moving
	local cell
	repeat
		-- check button inputs and set direction
		-- direction persists between frames
		for i=0,4 do
			if btn(i) then
				dir = i
				break
			end
		end
		-- todo match speed to level properly
		cell = self.pos:round()
		self:move(cell, dir, 0, 0.5)
		-- todo clear pellets
		-- todo set power for big pellet
		yield()
	until win
end

ghost = class(thing)
function ghost:draw()
	pal(8, self.color)
	-- todo set flipx and sprite
	thing.draw(self)
end

function ghost:loop()
	local dir
	-- states are:
	-- 0: chase
	-- 1: scatter
	-- 2: fright
	-- 3: eyes
	local state = 0
	local cell
	repeat
		cell = self.pos:round()
		state, dir = self:path(state, cell, dir)
		-- todo match speed to level and state properly
		self:move(cell, dir, 1, 0.6)
		if state < 3 then
		-- todo check for pacman
		yield()
	until win
end

function ghost:path(state, cell, dir)
	-- first check if pathing is to be reevaluated
	-- rules depend on state and global
	if mode_frame and state < 2 then
		-- reverse state and direction
		if dir == 1 or dir == 3 then
			return ((state + 1) % 2), dir - 1
		else
			return ((state + 1) % 2), dir + 1
		end
	else
		-- not doing a reversal, so only redirect on
		-- intersections
		local rule = mget(cell.x, cell.y)
		if fget(rule, 3) then
			-- flag 3 on current cell means intersection
			if state == 2 then
				-- frightened, select direction at random
				-- todo
			else
				local t
				if state == 3 then
					-- eyes, target is ghost-independent
					-- set t to cage cells
					-- or change state if there
					-- todo
				else
					-- either chase or scatter,
					-- target depends on ghost
					t = self:target(state, cell)
				end
				-- todo choose a direction based on target
			end
		end
	end
end

blinky = class(ghost)
function blinky:init(x, y)
	self.color = 8
	ghost.init(self, x, y)
end

function blinky:target()
	-- todo
end

pinky = class(ghost)
function pinky:init(x, y)
	self.color = 14
	ghost.init(self, x, y)
end

function pinky:target()
	-- todo
end

inky = class(ghost)
function inky:init(x, y)
	self.color = 12
	ghost.init(self, x, y)
end

function inky:target()
	-- todo
end

clyde = class(ghost)
function clyde:init(x, y)
	self.color = 9
	ghost.init(self, x, y)
end

function clyde:target()
	-- todo
end
__gfx__
0000000000000000000000000000000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa00000
00000000000000000000000000000000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000
0070070000000111111111111110000000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa00
00077000000011cccccccccccc1100000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaa0000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa0
0007700000011cccccccccccccc110000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaa000aaaaaaaaaaa00000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa0
007007000011cc111111111111cc1100aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000aaaaaaaaaa000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
00000000001cc11000000000011cc100aaaaaaaaaaaaaaaaaaaaaaaaaaa00000aaaaaaaaa0000000aaaaaaaa00000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
00000000001cc10000000000001cc100aaaaaaaaaaaaaaaaaaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
00777700001cc10000000000001cc100aaaaaaaaaaaaaaaaaaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
07777770001cc10000000000001cc100aaaaaaaaaaaaaaaaaaaaaaaaaaa00000aaaaaaaaa0000000aaaaaaaa00000000aaaaaaa00aaaaaaaaaaaaa0000aaaaaa
77777777001cc10000000000001cc100aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000aaaaaaaaaa000000aaaaaaa00aaaaaaaaaaaaa0000aaaaaa
77777777001cc10077777777001cc1000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaa000aaaaaaaaaaa00000aaaaa0000aaaaa00aaaaa0000aaaaa0
77777777001cc10077777777001cc1000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaa0000aaaaa0000aaaaa00aaaa000000aaaa0
77777777001cc10000000000001cc10000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaa0000aaaa0000aaa000000aaa00
07777770001cc10000000000001cc100000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaa0000aaa000000a00000000a000
00777700001cc10000000000001cc10000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000a0000a000000000000000000000
00000000001cc11000000000001cc100000008888880000000000788887000000000088888800000000000000000000000000aaaaaa000000000000000000000
00000000001cc11100000000011cc1000008888888888000000117888871100000088888888880000000000000000000000aaaaaaaaaa0000000000000000000
000000000011cc111111111111ccc100008888888888880000711788887117000088888888888800000000000000000000aaaaaaaaaaaa000000000000000000
0007700000011ccccccccccccccc110008888888888888800877778888777780088888888888888000000000000000000aaaaaaaaaaaaaa00000000000000000
00077000000011ccccccccccccc1100008778888877888800887788888877880088888888888888000007700007700000aaaaaaaaaaaaaa00000000000000000
000000000000011111111111111100000777788877778880088888888888888008888888888888800007777007777000aaaaaaaaaaaaaaaa0000000000000000
000000000000000000000000000000008117788811778888888888888888888888887788887788880007117007117000aaaaaaa00aaaaaaa0000000000000000
000000000000000000000000000000008117788811778888888888888888888888877778877778880007117007117000aaaaaaa00aaaaaaa0000000000000000
000000000000000000000000000000008877888887788888888888888888888888877778877778880007777007777000aaaaaa0000aaaaaa0000000000000000
000000000000000000000000000000008888888888888888888888888888888888871178871178880000770000770000aaaaaa0000aaaaaa0000000000000000
000000000030030000a00a00000000008888888888888888888888888888888888881188881188880000000000000000aaaaa000000aaaaa0000000000000000
000330000000000000000000000aa00088888888888888888888888888888888888888888888888800000000000000000aaaa000000aaaa00000000000000000
000330000000000000000000000aa00088888888888888888888888888888888888888888888888800000000000000000aaa00000000aaa00000000000000000
000000000030030000a00a0000000000888088888888088888808888888808888880888888880888000000000000000000a0000000000a000000000000000000
00000000000000000000000000000000880008800880008888000880088000888800088008800088000000000000000000000000000000000000000000000000
00000000000000000000000000000000800008000080000880000800008000088000080000800008000000000000000000000000000000000000000000000000
__gff__
0003030300000000000000000000000004030103000000000000000000000000040303030000000000000000000000000c08181c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0122222222222222222222222203012222222222222222222222220300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320202020203020202020202011132020202020203020202020201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320010202032001020202032011132001020202032001020203201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1310110000132011000000132011132011000000132011000013101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320212222232021222222232021232021222222232021222223201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1330202020203020203020203020203020202020203020202020301100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320010202032001032001020202020202032001032001020203201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320212222232011132021222203012222232011132021222223201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320202020203011132020202011132020202011133020202020201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2102020202032011210202030011130001020223132001020202022300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011012222230021230021222203132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011130000003200003200000011132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011130001020212120202030011132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2222222222232021230011000000000000130021232021222222222200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000003000003111000000000000133100003000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0202020202032001030011000000000000130001032001020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011130021222222222222230011132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011133100000000000000003111132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000132011130001020202020202030011132011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0122222222232021230021222203012222230021232021222222220300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320202020203020203020202011132020203020203020202020201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320010202032001020202032011132001020202032001020203201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1320212203132021222222232021232021222222232011012223201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1310202011133020203020203300003320203020203011132020101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2102032011132001032001020202020202032001032011132001022300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0122232021232011132021222203012222232011132021232021220300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120203020202011132020202011132020202011132020203020201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120010202020223210202032011132001020223210202020203201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120212222222222222222232021232021222222222222222223201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120202020202020202020203020203020202020202020202020201100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2102020202020202020202020202020202020202020202020202022300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
