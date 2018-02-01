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
	elseif type(t) == "thread" then
		return "<thread>"
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
function sort(list, keyfunc)
	-- insertion, good enough
	local keys = {}
	keyfunc = keyfunc or function(v) return v end
	for i = 1, #list do
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
	return list
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

function vec2:__eq(other)
	return self.x == other.x and self.y == other.y
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
function vec2:round()
	return vec2(flr(self.x + 0.5), flr(self.y + 0.5))
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

-->8
--========
-- hooks and globals
--========
local ents -- table
local level -- int
local win -- bool
local lose -- bool
local power -- int
local cur_timer -- int
local timers -- table[int]
local chase_mode -- bool
local mode_frame -- bool
local pac -- pacman
local ghosts -- table[ghost]
local cam = box(0, 0, 16, 16)
local pelletcount -- int
local coros -- table[coroutine]
local vel = 0.25

function _init()
	ents = {
		pacman(13.5, 23),
		blinky(13.5, 11),
		pinky(11.5, 14),
		inky(13.5, 14),
		clyde(15.5, 14)
	}
	win = false
	lose = false
	pac = ents[1]
	ghosts = {ents[2], ents[3], ents[4], ents[5]}
	cur_timer = 0
	chase_mode = true
	mode_frame = false
	timers = {210, 600, 210, 600, 150, 600, 150}
	power = 0
	pelletcount = 0
	for x=0,27 do for y=0,30 do
		if fget(mget(x,y), 2) then
			pelletcount += 1
		end
	end end
	coros = {
		endtime = cocreate(endgame_timer),
		wingame = cocreate(win_timer)
	}
end

function _update()
	if win then
		lose = false
		if not coresume(coros.wingame) then
			extcmd('reset')
		end
	elseif lose then
		if not coresume(coros.endtime) then
			extcmd('reset')
		end
	else
		-- manage various timers
		if cur_timer == 0 and #timers > 0 then
			cur_timer = timers[1]
			del(timers, cur_timer)
			chase_mode = not chase_mode
			mode_frame = true
		elseif power >  0 then
			power -= 1
		else
			cur_timer -= 1
		end
		-- update the entities
		foreach(ents, function(t) t:update() end)
		-- update the camera
		local ploc = pac.pos
		cam.l = mid(0, ploc.x - 8, 12)
		cam.t = mid(0, ploc.y - 8, 15)
		-- reset mode_frame for the next frame
		mode_frame = false
	end
end

function endgame_timer()
	-- currently just a timer before reset
	-- plus setting pac's animation
	pac.anim = 1
	for i=1,23 do
		yield()
	end
end

function win_timer()
	-- manages fades for win animation
	pal(1, 0)
	pal(7, 15)
	pal(12, 13)
	pal(10, 9)
	for i=1,10 do yield() end
	pal(12, 5)
	pal(7, 6)
	pal(10, 4)
	for i=1,10 do yield() end
	pal(10, 5)
	pal(7, 1)
	pal(12, 1)
	for i=1,10 do yield() end
	pal(10, 0)
	pal(7,0)
	pal(12, 0)
	for i=1,10 do yield() end
end

function _draw()
	cls()
	-- center the camera on the player
	-- but don't scroll past the edge
	camera(cam.l * 8, cam.t * 8)
	local x = flr(cam.l)
	local y = flr(cam.t)
	map(x, y, x * 8, y * 8, 17, 17)
	if not win and not lose then
		for e in all(ents) do
			e:draw()
		end
	else
		-- don't draw ghosts in endgame
		pac:draw()
	end
end
-->8
function wrap(pos)
	pos.x = pos.x % 224
end

-- directions are enumerated to match the buttons
-- but with 4 for not-moving
local directions = {
	[0] = vec2(-1, 0),
	[1] = vec2(1, 0),
	[2] = vec2(0, -1),
	[3] = vec2(0, 1),
	[4] = vec2(0, 0)
}
function revdir(num)
	if num == 4 then
		return 4
	elseif num % 2 == 1 then
		return num - 1
	else
		return num + 1
	end
end

-- maps sprite numbers for pellets
-- to no-pellet equivalents
-- (preserve ghost pathing info)
local pelletmap = {
	[16] = 47,
	[32] = 47,
	[62] = 49,
	[48] = 49,
	[51] = 50
}
-->8
thing = class()
function thing:init(x, y)
	self.pos = vec2(x, y)
	self.anim = 1
	self.dir = 4
end

function thing:move(flag, vel)
	local pos = self.pos
	local dir = self.dir
	if dir ~= 4 then
		local cell = pos:round()
		local target = (pos + (directions[dir] * 0.7)):round()
		-- check if we're within half a cell of hitting the wall
		if not fget(mget(target.x, target.y), flag) then
			self.pos += (directions[dir] * vel)
		end
		-- nudge towards path center
		local axis
		if dir < 2 then -- horiz movement
			axis = 'y'
		else -- vertical movement
			axis = 'x'
		end
		if pos[axis] ~= cell[axis] then
			local nudge = min(abs(pos[axis] - cell[axis]), vel)
			local sign = (pos[axis] - cell[axis]) > 0 and -1 or 1
			self.pos[axis] += sign * nudge
		end
	end
end

function thing:draw(sp, flipx, flipy)
	local center = self.pos
	-- adjust coordinates because sprites draw from top-left
	spr(sp, (center.x - 1) * 8 + 4,
		(center.y - 1) * 8 + 4, 2, 2,
		flipx, flipy)
end

pacman = class(thing)
function pacman:init(x, y)
	thing.init(self, x, y)
	self.sprss = {
		{4, 6, 8, 10, 8, 6, 4},
		{4, 12, 14, 44, 14, 12, 4},
		{4, 12, 14, 44, 14, 12, 4},
		{4}
	}
	self.sprss[0] = {4, 6, 8, 10, 8, 6, 4}
end

-->8
function pacman:draw()
	local flipx, flipy, sprs
	if not lose then
		flipx = self.dir == 0
		flipy = self.dir == 2
		sprs = self.sprss[self.dir]
	else
		flipx = false
		flipy = true
		sprs = {4, 12, 14, 44, 68, 70, 72, 74, 76}
	end
	self.anim = (self.anim + 0.3) % #sprs
	thing.draw(self, sprs[flr(self.anim)+1], flipx, flipy)
end

function pacman:update()
	-- check button inputs and set direction
	-- direction persists between frames
	for i=0,4 do
		if btn(i) then
			self.dir = i
			break
		end
	end
	local cell = self.pos:round()
	local p = mget(cell.x, cell.y)
	local pelcell = fget(p, 2)
	if not pelcell then
		-- eating causes one-frame pause
		-- energizers cause speed up
		if power > 0 then
			self:move(0, vel * 1.1)
		else
			self:move(0, vel)
		end
	end
	self.pos.x = self.pos.x % 28
	-- clear pellets
	if pelcell then
		mset(cell.x, cell.y, pelletmap[p])
		pelletcount -= 1
		if pelletcount == 0 then
			win = true
		end
		if p == 16 or p == 62 then
			-- energizer
			power = 150 -- can't find timing docs
		end
		-- a ghost counts up
		for g in all(ghosts) do
			if g.state == 4 then
				g.counter += 1
				break
			end
		end
	end
end
-->8
ghost = class(thing)
function ghost:init(x, y)
	thing.init(self, x, y)
	self.home = vec2(13.5, 14)
	self.sprs = {36, 38, 40, 40}
	self.sprs[0] = 36
	self.counter = 0
	-- states are:
	-- 0: chase
	-- 1: scatter
	-- 2: fright
	-- 3: eyes
	-- 4: in cage
	-- 5: following specific routine
	self.state = 4
	self.dir = 1
end

function ghost:draw()
	pal(8, self.color)
	local sp
	local st = self.state
	if st == 2 then
		sp = 66 -- fright
	elseif st == 3 then
		sp = 42 -- eyes
	else
		sp = self.sprs[self.dir]
	end
	thing.draw(self, sp, self.dir == 0, false)
end

function ghost:update()
	local cell = self.pos:round()
	self.state, self.dir = self:path(cell)
	if self.state == 2 then
		self:move(1, vel * 0.7)
	else
		self:move(1, vel * 0.98)
	end
	if cell == pac.pos:round() then
		-- someone's getting et
		if self.state < 2 then
			lose = true
		elseif self.state == 2 then
			self.state = 3
		end
	end
end

function ghost:path(cell)
	printh("pathing for ".. self.name)
	spew{self}
	-- todo movement problems:
	-- todo choice of direction
	-- todo special-case leaving the house
	-- debug grep is:
	-- grep -pazo "(?s)pathing for blinky.*?intersection.*?done pathing"
	-- current
	local state = self.state
	local dir = self.dir
	-- new
	local ns = state
	local nd = dir
	-- first check if pathing is to be reevaluated
	-- rules depend on state and global
	-- default is to not change
	if mode_frame and state < 2 then
		printh 'reversing'
		-- reverse state and direction
		ns = (state + 1) % 2
		nd = revdir(dir)
	elseif ((self.pos * 8) - (cell * 8)):mag() < 2 then
		-- only reevaluate direction once per cell,
		-- in the middle
		local t
		local options = find_valid_directions(cell, state, dir)
		-- not doing a reversal, so only redirect on
		-- intersections or if leaving the cage
		if state == 4 then
			printh 'caged'
			-- currently in cage.
			if self.counter >= self.plimit then
				t = self:begin_leaving()
				ns = 5
			else
				t = self.home -- stay put
			end
		elseif state == 5 then
			printh 'leaving'
			coresume(self.routine)
			t = self.nextstep
			ns = costatus(self.routine) == 'dead' and 0 or 5
		elseif fget(mget(cell.x, cell.y), 3) then
			-- only turn at intersections and when within one pixel
			-- of the middle of the lane
			printh 'intersection'
			spew{'options after filtering', options}

			if state == 2 then
				printh 'frightened'
				-- frightened
				nd = options[flr(rnd(#options)+1)]
			else
				printh 'targeting'
				if state == 0 then
					printh 'chase'
					-- chase
					t = self:target(state, cell)
				elseif state == 1 then
					printh 'scatter'
					-- scatter
					t = self.scatterpoint
				elseif state == 3 then
					printh 'eyes'
					-- eyes
					t = self.home
					if cell == self.home then
						printh 'now home'
						self.counter = 0
						ns = 4
					end
				end
			end
		else
			-- if we somehow end up not having a target?
			t = self:target(state, cell)
		end
		if state != 2 then
			printh('target is ' .. repr(t))
			-- choose the direction with the least
			-- straight-line distance to the target
			local function kf(d)
				printh('measuring direction '..d)
				local c = cell + directions[d]
				printh('distance from ' .. repr(c) .. ' to ' .. repr(t) .. ' is ' .. (c-t):mag())
				return (c - t):mag()
			end
			nd = sort(options, kf)[1]
		end
	end
	spew {ns, nd}
	printh "done pathing"
	return ns, nd
end

function ghost:begin_leaving()
		printh 'time to go'
		-- time to leave
		self.routine = cocreate(leave_home_statemachine)
		coresume(self.routine, self)
		return self.nextstep
end

function find_valid_directions(cell, state, dir)
	-- if cell has flag 4, can't go up
	local options = fget(mget(cell.x, cell.y), 4) and {0, 1, 3} or {0, 1, 2, 3}
	-- filter out any walls
	options = filter(options, function(d)
		local c = cell + directions[d]
		local s = mget(c.x, c.y)
		local b = not fget(s, 1)
		return b and (state == 3 or state == 5 or not fget(s, 0))
		end)
		-- can't go backwards unless dead-end
		if #options > 1 then del(options, revdir(dir)) end
		return options
end

function leave_home_statemachine(self)
	-- target is just outside box
	self.nextstep = vec2(13.5, 11)
	-- target remains that until it gets there
	repeat yield() until abs(self.pos.y - 11) * 8 < 2
	-- then state is off to the left and the routine ends
	self.nextstep = vec2(9, 11)
end

blinky = class(ghost)
function blinky:init(x, y)
	ghost.init(self, x, y)
	self.name = "blinky" -- for debugging
	self.color = 8
	self.plimit = 0
	self.scatterpoint = vec2(25,-3)
	self.state = 1
	self.dir = 1
end

function blinky:target()
	-- directly for pac-man
	return pac.pos:round()
end

pinky = class(ghost)
function pinky:init(x, y)
	ghost.init(self, x, y)
	self.name = "pinky" -- for debugging
	self.color = 14
	self.plimit = 0
	self.scatterpoint = vec2(2,-3)
end

function pinky:target()
	-- four tiles ahead of pac-man
	return pac.pos:round() + directions[pac.dir] * 4
end

inky = class(ghost)
function inky:init(x, y)
	ghost.init(self, x, y)
	self.name = "inky" -- for debugging
	self.color = 12
	self.plimit = 30
	self.scatterpoint = vec2(27,32)
end

function inky:target()
	-- twice as far from blinky as the
	-- spot two in front of pac is from blinky.
	local ppos = pac.pos:round() + directions[pac.dir] * 2
	local bpos = ghosts[1].pos:round()
	-- bpos+(ppos-bpos)*2 reduces:
	return (ppos * 2) - bpos
end

clyde = class(ghost)
function clyde:init(x, y)
	ghost.init(self, x, y)
	self.name = "clyde" -- for debugging
	self.color = 9
	self.plimit = 60
	self.scatterpoint = vec2(0,32)
end

function clyde:target()
	-- meanders around eight tiles from pac
	-- unless pac gets close to his scatterpoint
	local ppos = pac.pos:round()
	if (ppos - self.pos:round()):mag() < 8 then
		return self.scatterpoint
	else
		return ppos
	end
end
__gfx__
0000000000000000000000000000000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa00000
00000000000011111111111111110000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000
0070070000011cccccccccccccc1100000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa00
000770000011cccccccccccccccc11000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaa0000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa0
00077000011cc11111111111111cc1100aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaa000aaaaaaaaaaa00000aaaaaaaaaaaaaa00aaaaaaaaaaaaaa0
0070070001cc1100000000000011cc10aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000aaaaaaaaaa000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
0000000001cc1000000000000001cc10aaaaaaaaaaaaaaaaaaaaaaaaaaa00000aaaaaaaaa0000000aaaaaaaa00000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
0000000001cc1000000000000001cc10aaaaaaaaaaaaaaaaaaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
0077770001cc1000000000000001cc10aaaaaaaaaaaaaaaaaaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaaa00aaaaaaaaaaaaaa00aaaaaaa
0777777001cc1000000000000001cc10aaaaaaaaaaaaaaaaaaaaaaaaaaa00000aaaaaaaaa0000000aaaaaaaa00000000aaaaaaa00aaaaaaaaaaaaa0000aaaaaa
7777777701cc1000000000000001cc10aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000aaaaaaaaaa000000aaaaaaa00aaaaaaaaaaaaa0000aaaaaa
7777777701cc1000000000000001cc100aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaa000aaaaaaaaaaa00000aaaaa0000aaaaa00aaaaa0000aaaaa0
7777777701cc1000777777770001cc100aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaaaa00aaaaaaaaaaaa0000aaaaa0000aaaaa00aaaa000000aaaa0
7777777701cc1000777777770001cc1000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaa0000aaaa0000aaa000000aaa00
0777777001cc1000000000000001cc10000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaa0000aaa000000a00000000a000
0077770001cc1000000000000001cc1000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000a0000a000000000000000000000
0000000001cc1000000000000001cc10000008888880000000000788887000000000088888800000000000000000000000000aaaaaa000000000000000000000
0000000001cc1000000000000001cc100008888888888000000117888871100000088888888880000000000000000000000aaaaaaaaaa0000000000000000000
0000000001cc1100000000000011cc10008888888888880000711788887117000088888888888800000000000000000000aaaaaaaaaaaa000000000000000000
00077000011cc11111111111111ccc1008888888888888800877778888777780088888888888888000000000000000000aaaaaaaaaaaaaa00000000000000000
000770000011ccccccccccccccccc11008888778888877800887788888877880088888888888888000007700007700000aaaaaaaaaaaaaa00000000000000000
0000000000011ccccccccccccccc11000888777788877770088888888888888008888888888888800007777007777000aaaaaaaaaaaaaaaa0000000000000000
000000000000111111111111111110008888771188877118888888888888888888887788887788880007117007117000aaaaaaa00aaaaaaa0000000000000000
000000000000000000000000000000008888771188877118888888888888888888877778877778880007117007117000aaaaaaa00aaaaaaa0000000000000000
000000000000000000000000000000008888877888887788888888888888888888877778877778880007777007777000aaaaaa0000aaaaaa0077770000000000
000000000000000000000000000000008888888888888888888888888888888888871178871178880000770000770000aaaaaa0000aaaaaa0777777000000000
000000000000000000000000000000008888888888888888888888888888888888881188881188880000000000000000aaaaa000000aaaaa7777777700000000
0007700000000000000000000007700088888888888888888888888888888888888888888888888800000000000000000aaaa000000aaaa07777777700000000
0007700000000000000000000007700088888888888888888888888888888888888888888888888800000000000000000aaa00000000aaa07777777700000000
00000000000000000000000000000000888088888888088888808888888808888880888888880888000000000000000000a0000000000a007777777700000000
00000000000000000000000000000000880008800880008888000880088000888800088008800088000000000000000000000000000000000777777000000000
00000000000000000000000000000000800008000080000880000800008000088000080000800008000000000000000000000000000000000077770000000000
000000000000000000000dddddd0000000000aaaaaa0000000000aaaaaa0000000000aaaaaa0000000000aa00aa0000000000000000000000000000000000000
0000000000000000000dddddddddd000000aaaaaaaaaa000000aaaaaaaaaa000000aaaaaaaaaa000000aaa0000aaa00000000000000000000000000000000000
000000000000000000dddddddddddd0000aaaaaaaaaaaa0000aaaaaaaaaaaa0000aaaaa00aaaaa0000aaa000000aaa0000000000000000000000000000000000
00000011110000000ddd66dddd66ddd00aaaaaaaaaaaaaa00aaaaaa00aaaaaa00aaaaa0000aaaaa000aa00000000aa0000000000000000000000000000000000
0000011cc11000000dd6616dd6166dd00aaaaaaaaaaaaaa00aaaaa0000aaaaa00aaaa000000aaaa00aa0000000000aa000000000000000000000000000000000
000011cccc1100000dd6166dd6616dd0aaaaaaaaaaaaaaaaaaaaa000000aaaaaaaaa00000000aaaa000000000000000000000000000000000000000000000000
00011cc11cc11000ddd6616dd6166dddaaaaaaa00aaaaaaaaaaa00000000aaaa0aa0000000000aa0000000000000000000000000000000000000000000000000
0001cc1111cc1000dddd66dddd66ddddaaaaaa0000aaaaaaaaa0000000000aaa0000000000000000000000000000000000000000000000000000000000000000
0001cc1111cc1000ddddddddddddddddaaaaa000000aaaaaaa000000000000aa0000000000000000000000000000000000000000000000000000000000000000
00011cc11cc11000ddd1ddd11ddd1dddaaaa00000000aaaaa00000000000000a0000000000000000000000000000000000000000000000000000000000000000
000011cccc110000dd1d1d1dd1d1d1ddaaa0000000000aaa00000000000000000000000000000000000000000000000000000000000000000000000000000000
0000011cc1100000ddddd1dddd1ddddd0a000000000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000001111000000dddddddddddddddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000ddd0dddddddd0ddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000dd000dd00dd000dd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000d0000d0000d0000d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
0003030300000000000000000000000004030103000000000000000000000000040303030000000000000000000002000c08181c000000000000000000000c0003030000000000000000000000000000030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0102020202020202020202020203010202020202020202020202020300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120202020203020202020202013112020202020203020202020201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120402222412040222222412013112040222222412040222241201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1110130000112013000000112013112013000000112013000011101300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120500202512050020202512050512050020202512050020251201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1130202020203020203020203020203020202020203020202020301300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120402222412040412040222222222222412040412040222241201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120500202512013112050020203010202512013112050020251201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120202020203013112020202013112020202013113020202020201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2122222222412013212222410013110040222223112040222222222300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013010202510050510050020203112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013110000003200003200000013112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013110040222212122222410013112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02020202025120505100132e2e00002e2e110050512050020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2e2e2e2e2e2e300000311300000000000011310000302e2e2e2e2e2e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
22222222224120404100132e2e2e2e2e2e110040412040222222222200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013110050020202020202510013112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013113100000000000000003113112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000112013110040222222222222410013112013000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0102020202512050510050020203010202510050512050020202020300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120202020203020203020202013112020203020203020202020201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120402222412040222222412013112040222222412040222241201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120500203112050020202512050512050020202512013010251201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1110202013113020203020203300003320203020203013112020101300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2122412013112040412040222222222222412040412013112040222300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0102512050512013112050020203010202512013112050512050020300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120203020202013112020202013112020202013112020203020201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120402222222223212222412013112040222223212222222241201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120500202020202020202512050512050020202020202020251201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1120202020202020202020203020203020202020202020202020201300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2122222222222222222222222222222222222222222222222222222300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
