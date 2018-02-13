pico-8 cartridge // http://www.pico-8.com
version 15
__lua__
--picohistory: wumpus
--by david heyman
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
-- misc
local function btnpoll()
	-- poll all buttons for p0
	for i=0,5 do
		if btn(i,0) then return i end
	end
	return false
end

-- 17 symbols for pick, plus three per call
-- 8 per call to inline
-- cheaper if used thrice
local function pick(list)
	return list[flr(rnd(#list))+1]
end

local function any(list, condition)
	for i=1,#list do
		if condition(list[i]) then
			return true
		end
	end
	return false
end

local function index_of(list, item)
	for i=1,#list do
		if item == list[i] then
			return i
		end
	end
	return false
end

-- classes
local function class(base, proto)
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

-- pair vectors
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

function vec2:__lt(other)
	return self:mag() < other
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
-->8
--=============
-- the player
--=============
player = {} -- singleton

function player:init(i)
	self.pos = vec2(60,56)
	self.i = i
	self.dir = 4 -- south (enumerated w,e,n,s)
	self.dirsprites = {96,98,66,64}
	self.update = cocreate(self.loop)
	-- first call is just to assign self inside the coroutine
	coresume(self.update, self)
end

function player:loop()
	yield() -- this just gets self assigned
	local b
	while true do
		-- input section
		b = false
		repeat -- await input
			-- poll buttons
			b = btnpoll()
			yield()
		until b
		-- now react to input
		-- 0-3: move. 4: shoot. 5: repeat audio cues.
		if b <= 3 then
			self:movement(b+1) -- one-indexed
		elseif b == 4 then
			self:arrow()
		else -- b == 5
			-- replay audio cues
			play_cues()
			yield()
		end
	end
end

-- direction to walk for each dir
local dir_vectors = {
	vec2(-1, 0), vec2(1, 0),
	vec2(0, -1), vec2(0, 1)
}
function player:movement(dir)
	self.dir = dir
	local old_index = self.i
	local new_index = world[old_index].conn[dir]
	-- move as far as wall
	local vector = dir_vectors[dir]
	local axis = dir < 3 and 'x' or 'y'
	local bound = room.bounds[dir][axis]
	repeat
		self.pos += vector
		yield()
	until self.pos[axis] == bound
	if new_index == 0 then
		self.dir = dir_reverse(dir)
		-- wall there
		-- todo play error tone
		bound = axis == 'x' and 60 or 56
		repeat
			self.pos -= vector
			yield()
		until self.pos[axis] == bound
	else
		-- move to next room
		-- change room, and walk into it
		self:enter_room(old_index, new_index)
	end
end

function dir_reverse(dir)
	return dir + (dir % 2 == 1 and 1 or -1)
end

function player:enter_room(old_index, new_index)
	self.i = new_index
	local new_room = world[new_index]
	local entry_dir = index_of(new_room.conn, old_index)
	self.dir = dir_reverse(entry_dir)
	local axis = entry_dir < 3 and 'x' or 'y'
	-- new vector is the reverse of the
	-- vector to the old room from this room
	local vector = dir_vectors[entry_dir] * -1
	-- set position based on door to previous
	self.pos = room.bounds[entry_dir]
	-- react to contents
	local bat = new_room.bat
	local pit = new_room.pit
	local wump = wumpus.i == new_index
	if pit then
		wait_for_anim(pit_animator)
		gameover("You fell into a pit.")
	else
		-- if wump or bat, walk until they run into you
		-- otherwise, walk to center
		local done
		if wump or bat then
			local a = wump and cocreate(wumpus_animator) or cocreate(bat_animator)
			add(extra_draws, a)
			done = function() return costatus(a) == 'dead' end
		else
			done = function() return self.pos.x == 60 and self.pos.y == 56 end
		end
		repeat
			self.pos += vector
			yield()
		until done()
		-- now either react to contents, or be done
		if wump then
			-- play teeth, then reset
			wait_for_anim(wumpus_teeth)
			gameover("You were eaten by a wumpus.")
		elseif bat then
			-- find a new room
			local where
			repeat
				where = flr(rnd(20))+1
			until where != new_index
			wait_for_anim(bat_pause)
			-- enter the new room
			self:enter_room(0, where)
		end
		-- nothing in the room, stop
	end
end

function wait_for_anim(co, arg)
	local c = cocreate(co)
	if arg then coresume(co,arg) end
	add(extra_draws, c)
	repeat yield() until costatus(c) == 'dead'
end

function player:arrow()
	-- todo print cue for shooting directions
	-- and feedback (from path)
	local b
	local path = {}
	for i=1,5 do
		-- take up to five inputs
		repeat
			b = btnpoll()
			yield()
		until b
		if b <= 3 then
			add(path, b+1)
		else -- nondirectional input, end path
			break
		end
	end
	if #path > 0 then -- to make sure player didn't cancel w/o dir
		local dir = path[1]
		local function firing()
			for i=1,10 do yield() end -- short pause before firing
			-- todo disable cue/feedback display
			local pos = vec2(64, 64) + (initial_vec * 8)
			-- determine horiz or vert sprite
			local s = dir < 3 and 117 or 16
			local hflip = dir == 1 or dir == 4
			local vflip = dir == 4
			-- simpler than checking position
			for i=1,64 do
				spr(s, pos.x, pos.y, 1, 1, hflip, vflip)
				pos = pos + ini
				yield()
			end
			-- then wait 1s
			for i=1,30 do yield() end
		end
		wait_for_anim(firing)
		-- check for wumpus
		if any(path, function(i) return i == wumpus.i end) then
			-- a hit
			-- todo play sound for hit
			gameover("You shot the wumpus!")
		else
			-- a miss
			-- todo play sound for miss
			wumpus:move_adj()
		end
	end
end

function player:draw()
	-- draws player
	local p = self.pos
	local s = self.dirsprites[self.dir]
	spr(s, p.x, p.y, 2, 2)
end
-->8
--=============
-- the wumpus
--=============
wumpus = {} -- singleton
function wumpus:move_adj()
	-- move to an adjacent room
	local new_room = pick(world[self.i].conn)
	self.i = new_room.i
end

function wumpus:is_near(i)
	-- tells whether the room is near the wumpus
	-- check this room
	if self.i == i then
		return true
	else
		local room = world[self.i]
		-- check rooms one away from wump
		for j in all(room.conn) do
			if j == i then
				return true
			elseif j ~= 0 then
				local one_degree = world[j]
				-- check rooms two away from wump
				for k in all(one_degree.conn) do
					if k == i then
						return true
					end
				end
			end
		end
	end
	return false
end

function wumpus_animator()
	-- runs when player enters wumpus room
	local pos = vec2(64, 64)
	repeat
		-- move 1px/frame
		pos -= (pos - player.pos):unit()
		spr(84, pos.x, pos.y, 2, 2)
		yield()
	until pos - player.pos < 4
end

function wumpus_teeth()
	-- todo wumpus teeth animation
end
-->8
--=============
-- bats and pits
--=============
function bat_animator()
	-- runs when player enters bat room
	local frame = 0
	local pos = vec2(64,64)
	repeat
		frame = (frame + 1) % 10
		local s = frame > 4 and 68 or 69
		-- move 1px/frame
		pos -= (pos - player.pos):unit()
		spr(s, pos.x, pos.y)
		yield()
	until pos - player.pos < 4
end

function bat_pause()
	-- half-a-second of black screen
	blank = true
	for i=1,15 do
		yield()
	end
	blank = false
	-- then let the player handle the new room entry
end

function pit_animator()
	-- todo runs when player enters pit room
end

function gameover(reason)
	blank = true
	-- todo set up text overlay
	extcmd('reset')
end
--=============
-- the world
--=============
room = class{
	bounds={vec2(8,56),vec2(104,56),vec2(60,16),vec2(60, 96)},
	palette={8,11,12,14}
}
function room:init(i,n,e,s,w)
	self.i = i
	self.conn = {w,e,n,s} -- in button order
	self.moss = pick(room.palette)
	self.pit = false
	self.bat = false
end

function room:status()
	-- determine what warnings to play
	local b = self.bat or any(self.conn, function(me) return me ~= 0 and world[me].bat end)
	local p = self.pit or any(self.conn, function(me) return me ~= 0 and world[me].pit end)
	return b, p, wumpus:is_near(self.i)
end

function room:draw()
	pal(11,self.moss)
	-- first draw the bit that's always the same
	map(0,0,0,0,16,15)
	-- then place the doors
	local dir_args = {
		{1,16,1,0,56,1,3},
		{2,17,1,120,56,1,3},
		{3,16,4,56,8,3,3},
		{4,16,0,56,112,3,1}
	}
	palt(0, false)
	for vals in all(dir_args) do
		if self.conn[vals[1]] ~= 0 then
			map(vals[2],vals[3],vals[4],vals[5],vals[6], vals[7])
		end
	end
	-- and the pit
	if self.pit then
		palt(14, true)
		map(19,0,40,44,6,6)
	end
	palt()
	pal()
	-- bats and wumpus move,
	-- so they get handled separately
end

world = {}
local function world_init()
	return {
		room(1,0,6,13,10),
		room(2,7,3,0,8),
		room(3,2,11,0,9),
		room(4,10,5,8,0),
		room(5,15,0,7,4),
		room(6,8,9,0,1),
		room(7,0,20,2,5),
		room(8,0,2,6,4),
		room(9,0,3,19,6),
		room(10,4,1,0,17),
		room(11,0,20,16,3),
		room(12,13,18,17,0),
		room(13,1,19,0,12),
		room(14,0,15,18,20),
		room(15,5,17,0,14),
		room(16,19,11,18,0),
		room(17,0,10,12,15),
		room(18,16,0,14,12),
		room(19,9,0,16,13),
		room(20,7,14,0,11)
	}
end

local function populate(rooms)
	-- place two bats and two pits, no overlap
	local avail = {}
	for i=1,20 do
		add(avail,i)
	end
	for key in all({'bat', 'pit'}) do
		for i=1,2 do
			local n = pick(avail)
			rooms[n][key] = true
			del(avail, n)
		end
	end
	-- return two unused locations, for player and wumpus
	local w = pick(avail)
	del(avail, w)
	return w, pick(avail)
end
-->8
--=============
-- audio
--=============
local function play_cues()
	local bat, pit, wump = world[player.i]:status()
	-- todo actually play audio
end
--=============
-- the game hooks
--=============
extra_draws = nil -- table of temp draw coroutines
text_overlay = nil
local function _init()
	-- set up the maze
	world = world_init()
	local p, w = populate(world)
	player:init(p)
	wumpus.i = w
	extra_draws = {}
	text_overlay = {}
end

local function _update()
	coresume(player.update)
end

local function _draw()
	cls()
	-- draw only if not blank
	if not blank then
		world[player.i]:draw()
		player:draw()
	end
	-- advance animations either way
	local next_draws = {}
	for f in all(extra_draws) do
		if coresume(f) then add(next_draws, f) end
	end
	extra_draws = next_draws
	if not blank then
		-- todo shading/lighting
	end	
	-- text overlays come after lighting
	if #text_overlay > 0 then
		-- todo
	end
end
__gfx__
000000002222222222204022222221042222222200111000000001100001000010000110000000000000000000000000999999992b299999999999440000011e
00000000292222222110b02222221104222922221111111111111111111111111111111109aa9aa9aa9aaa9aaa9aa9a0999999992b294444499994220000001e
0070070022422222221101111111100122442222222222222222222222222222222222220a22222222222222222222a099999944222944444494442b00000011
00077000222222221110000000100b0122222222aaaaaaaa9aaaaaaaaaaaaaa9aaaaaaaa0a21111111111111111112904999944429944444444442bb0000001e
000770002222221100044449900000b012222222111111114111111111111114111111110921101001010110110112a04499444224444222244442bb0000011e
00700700222222100944224444444400111222229999999929999999aaaa9992a99999990a21001000000000010012a04414442124222bbbb24442bb0000011e
00000000222221009422222222222110001122229aa9999929999999a99999929999aa990a21000000000000000012902222222bb2422222222222b20000111e
000000002212210442222222222221004001222299999994299a999999999999229999990921100000000000000112a0b22bb2949949994992922a22000011ee
00006000110111042222222222221004440112229999994429999999999999992b2999990a210100100100000001129029922290e11000002292aaaa00011eee
0006660010b010042222222222210044224011224999942224499999999999992b2944440a21000011100000000012a029992220e100000092922999000111ee
00004000100000442222222222210a92222401114494442b2444449999999944222944440921000022110000000012a029992490e110000042922999000011ee
00004000009109422222999222210a9222210100444442bb2444444449999444299444440a211000a92110000001129029a922901110000092912999000111ee
000040000b010442222294422221094222210092244442bb2244444444994442244442220a2110000a211000000112a0299922201100000092912999100011ee
0000400010110422222942222221094222210992b24442bbbb2442224414442124222bbb092100000a210000000012a029a42290100000004292244911111eee
0000400010000422222222222221144222210002222222b2b22222bb2222222bb22222220a21100009211000000112a02aa42490111000009292444911eeeeee
000a0a000bb04422222222222222004222210bb099922a22222aaa22b22bbb22229aaa99092110000a2100000001129029942420e1000000924244441eeeeeee
00000000000b0222222222222221001122210b0b9992aaaa9999999229922299299999990a21000000000000000012a019442490000000002242444400000000
1110000022104422222222222210000022211b009a9229999999999929999999299999990a21000000000000000112a014442490000010009242244400000000
221100002210000042222222210094400222109299922999999999992999aa9929999aaa09211000010000010000129022222240001000109221122200000000
3331100021100bb00422222210094422022210029991299999a99999299aa999299999990a21011001011100110112a0a9a92491000110014221199900000000
42211000010991bb042222211019422202210040999129999aa9999929999999129999990a211111111111111111129099992490011111009291299911000000
5511100000044400004222210944222200210444999224499999944429944999299999990922222222222222222222a094492240111111d0929299a911100100
66d5100044442244404221100422222220100422999244499944444424444444244999990aa9aa9aa9aa9aa9aa9aa9a04449229111ddd1114242449411111111
776d10002222222222001100442222222100442299424444444444442444444424444999000000000000000000000000111111112222222111111111e11ee111
882210002222222222100002222222211044422294424444444444421444224422444449000012a00a21000000001001eeeee1eeeeeeeeeeeeeeeeeee1100000
942210002222222222210bb022002110044222224442244444222442142bbb242b2224440000129aa921000000000111e11e11ee11111111eeeeeeeee1100000
a942100022222222222210b000bb0100442222222221122222bb222222222b2222211222000111222211100000001122ee111ee11001110111eeeeeee1110000
bb33100022229222222210bb000b0004422222229921199992b22999a99112999992229900000111111110000001129a11e1111110000000001eeee1ee110100
ccd510002222422222210b000990110422222222a991299992b299a999912999a992929a0000000110000000000112a0e111011000000000011eee11e1111110
dd51100022222222222100094444220042222292999299a999229999944299949992999a0000100000010000000012a0e1100000000000000011ee1eeeeeee11
ee421000222222222210b04222222210422229424442449444429444444294444442499400000000000000000001129011000000000000000000111eeeeeeee1
f9421000222222222210b0422222210422222222111111111111111111111111111111110000000000000000000012a0e1100000000000000000001eeeee1111
00000055500000000000005550000000000000001000000144444444444444444444444444444444000000000000000000000000000000000000000000000000
00000552550000000000055555000000000000001000000144444444444444444444444444444444000000000000000000000000000000000000000000000000
00005555555000000000555555500000001111001100001199999999999999999999999999999999000000000000000000000000000000000000000000000000
00000eddde00000000000eeeee000000018118101111111199999999999999999999999999999999000000000000000000000000000000000000000000000000
000000ddd0000000000000eee0000000111001111181181199999999999999999999999999999999000000000000000000000000000000000000000000000000
00000255520000000000022222000000100000010110011099999999999999999999999999999999000000000000000000000000000000000000000000000000
0000552525500000000055554a500000100000010000000099999999999999999999999999999999000000000000000000000000000000000000000000000000
04050552550500000005055444050400100000010000000099999999999999999999999999999999000000000000000000000000000000000000000000000000
04750555450500000005054455057400007000000000007099999999999999999999999999999999000000000000000000000000000000000000000000000000
0407022224050000000d0442220d0400007770000000777099999999999999999999999999999999000000000000000000000000000000000000000000000000
004d75555596900000969555557d4000000777000007770099999999999999999999999999999999000000000000000000000000000000000000000000000000
0004445559aaa90009aaa95554440000000077444447700099999999999999999999999999999999000000000000000000000000000000000000000000000000
00000550559a9000009a955055000000000044444444400099999999999999999999999999999999000000000000000000000000000000000000000000000000
00000550550900000009055055000000000444444444440099999999999999999999999999999999000000000000000000000000000000000000000000000000
0000055055000000000005505500000000044ff444ff440099999999999999999999999999999999000000000000000000000000000000000000000000000000
0000044044000000000004404400000000044ff444ff440099999999999999999999999999999999000000000000000000000000000000000000000000000000
00000055500000000000005550000000000444444444440099999999999999999999999999999999000000000000000000000000000000000000000000000000
00000255550000000000055552000000004444444444444077777777777770777777770777777777000000000000000000000000000000000000000000000000
00005555555000000000555555500000004449999999444077777777777770777777770777777777000000000000000000000000000000000000000000000000
000000dee0000000000000eed0000000004449777779444007777777777770777777770777777777000000000000000000000000000000000000000000000000
000000dde0000000000000edd0000000004449999999444000777777777700777777770777777777000000000000000000000000000000000000000000000000
00000024200000000000002240000000004444444444444000777777777700777777700777777777000000000000000000000000000000000000000000000000
0000005544000000000000a420000000004444000004444000077777777700777777700777777770000000000000000000000000000000000000000000000000
00000545540000000000044550000000004444000004444000007777777000777777700077777770000000000000000000000000000000000000000000000000
00005045500000000000045550000000000000000000000000007777770000077777000077777770000000000000000000000000000000000000000000000000
00005022200000000000005540000000000000000000000000000777700000077777000077777770000000000000000000000000000000000000000000000000
00096905500000000000002d4000000000000000a000006000000777700000077777000007777700000000000000000000000000000000000000000000000000
009aaa95500000000000007d40000000000000000444446600000777700000077777000007777700000000000000000000000000000000000000000000000000
0009a90550000000000007540000000000000000a000006000000077000000077770000007777700000000000000000000000000000000000000000000000000
00009005500000000000444500000000000000000000000000000077000000007770000007777000000000000000000000000000000000000000000000000000
00000005500000000000005500000000000000000000000000000077000000007770000000777000000000000000000000000000000000000000000000000000
00000044400000000000004440000000000000000000000000000070000000000700000000070000000000000000000000000000000000000000000000000000
__map__
3b05060708050607080506070805061a0b24093c3d3d3d3d3e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b1516171815161718151617181516192b29001d747474740f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b2526272825262728252627282526193303001d747474740f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b3536373835363738353637383536190b09001d747474740f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b0102030401020304010203040102190c0d0e1d747474740f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b1112131411121314111213141112191c741e3f2f2f2f2f1f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b2122232421222324212223242122192c2d2e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b31323334313233343132333431321900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b01020304010203040102030401021900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b11121314111213141112131411121900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b21222324212223242122232421221900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b31323334313233343132333431321900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b01020304010203040102030401021900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1b11121314111213141112131411121900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
390a0a0a0a0a0a0a0a0a0a0a0a0a0a3a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
