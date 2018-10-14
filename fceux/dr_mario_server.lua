local u = require('unix')

local PROTOCOL_VERSION = '1'

local function die(s)
	io.stderr:write(s)
	io.stderr:write('\n')
	os.exit()
end

-- fceux's print doesn't work right. It screws up numbers (often printing weird
-- floating-point numbers when given integers), doesn't accept multiple
-- arguments, and prints an extra newline. As long as we already can't handle
-- multiple arguments, we might as well fix the other two problems.
local function print(s)
	if s == nil then io.stdout:write('nil') else io.stdout:write(s) end
	io.stdout:write('\n')
end

local s2c_path = io.read("*line")
local c2s_path = io.read("*line")

-- We can't open the write end non-blocking because we don't know if the client
-- is on the other end yet and non-blocking write-only opens fail if not.
-- Instead we open it in blocking mode, then use fcntl to set it to
-- nonblocking.
print('Waiting for client to connect...')
local s2c_fd = u.open(s2c_path, u.O_WRONLY)
if not s2c_fd then
	die('Failed to open ' .. s2c_path .. ' for writing.')
end
if not u.fcntl(s2c_fd, u.F_SETFL, u.O_NONBLOCK) then
	die('Failed to set ' .. s2c_path .. ' to non-blocking mode.')
end
local c2s_fd = u.open(c2s_path, u.O_RDONLY + u.O_NONBLOCK)
if not c2s_fd then
	die('Failed to open ' .. c2s_path .. ' for reading.')
end

local c2s_coroutine = coroutine.create(function()
	-- TODO: the spec says we're supposed to truncate long messages, not
	-- discard them
	local pending = ''
	local discarding = false
	local MAX_MESSAGE_LEN = 8192

	while true do
		local this_read = u.read(c2s_fd, MAX_MESSAGE_LEN)
		if nil == this_read then coroutine.yield(nil) else
			pending = pending .. this_read
			local e = pending:find('\n')

			if nil == e then
				if discarding then
					pending = ''
				elseif pending:len() >= MAX_MESSAGE_LEN then
					discarding = true
					pending = ''
				end
			elseif discarding or e > MAX_MESSAGE_LEN then
				pending = pending:sub(e+1)
				discarding = false
			else
				while nil ~= e do
					coroutine.yield(pending:sub(1,e-1))
					pending = pending:sub(e+1)
					e = pending:find('\n')
				end
			end

			if this_read:len() < MAX_MESSAGE_LEN then coroutine.yield(nil) end
		end
	end
end)

local function o(s)
	if nil == u.write(s2c_fd, s .. '\n') then
		die("The client isn't reading its messages. I can't work in these conditions.")
	end
end

local function i()
	e, v = coroutine.resume(c2s_coroutine)
	if not e then
		die("The input coroutine died. That should be impossible. Just to be safe, I'm exploding.")
	end
	return v
end

local PLAYER_MODE =
	{ cleanup = 0
	, control = 1
	, lost = 2
	, virus_placement = 3
	, won = 4
	}

local ADDR =
	{ board_mode = 0x46
	, players = 0x727
	, speed_bonus_table = 0xa3a7
	, frames_per_row_table = 0xa7af
	, pill_sequence = 0x780
	}

local BOARD_MODE = { prep = 8, game = 4 }
local BOARD_WIDTH  = 8
local BOARD_HEIGHT = 16
local BOARD_SIZE   = BOARD_WIDTH * BOARD_HEIGHT

local PLAYER_1_ADDRS =
	{ pill_sequence_counter = 0x0327
	, pill_drop_counter = 0x0312
	, fine_speed = 0x030a
	, coarse_speed = 0x030b
	, board = 0x400
	, virus_count = 0x324
	, level = 0x734
	, pill_x = 0x305
	, pill_y = 0x306
	, pill_orientation = 0x325
	, pill_colors = 0x301
	}

local PLAYER_2_ADDRS =
	{ pill_sequence_counter = 0x03a7
	, pill_drop_counter = 0x0392
	, fine_speed = 0x038a
	, coarse_speed = 0x038b
	, board = 0x500
	, virus_count = 0x3a4
	, level = 0x735
	, pill_x = 0x385
	, pill_y = 0x386
	, pill_orientation = 0x3a5
	, pill_colors = 0x381
	}

local CELL_EMPTY = 0xff
local CELL_TWO_EMPTIES = string.char(CELL_EMPTY, CELL_EMPTY)
local CELL_BASE_OFFSET = 0x60
local CELL_BOTTOM_NIBBLE_MAP =
	{ [0] = 2
	, [1] = 1
	, [2] = 3
	}
local CELL_TOP_NIBBLE_MAP =
	{ [0x4] = 12
	, [0x5] =  8
	, [0x6] = 16
	, [0x7] = 20
	, [0x8] =  4
	, [0xd] =  0
	}
local PILL_LOOKAHEAD =
	{ [0] = 'rv'
	, [1] = 'ru'
	, [2] = 'rw'
	, [3] = 'qv'
	, [4] = 'qu'
	, [5] = 'qw'
	, [6] = 'sv'
	, [7] = 'su'
	, [8] = 'sw'
	}

local function bcd_decode(byte)
	local bot_nibble = byte % 16
	local top_nibble = (byte - bot_nibble) / 16
	return top_nibble*10 + bot_nibble
end

local function max_virus_count(level)
	return math.min(level+1,21)*4
end

local Player = {}

function Player.new(addrs, id)
	local self =
		{ addrs = addrs, id = id
		, mode = PLAYER_MODE.virus_placement
		, coarse = memory.readbyte(addrs.coarse_speed)
		, garbage = {}
		, garbage_valid = true
		, old_sequence = memory.readbyte(addrs.pill_sequence_counter)
		, old_drop = memory.readbyte(addrs.pill_drop_counter)
		, old_old_drop = memory.readbyte(addrs.pill_drop_counter) + 1 -- anything inequal to old_drop is fine
		, old_fine = memory.readbyte(addrs.fine_speed)
		, old_pill = ''
		, old_x = -1
		, old_y = -1
		, seen_nonzero_drop = false
		, seen_small_virus_count = false
		, update = Player.update
		, send_state = Player.send_state
		, send_pill = Player.send_pill
		, lookahead = Player.lookahead
		, lookcurrent = Player.lookcurrent
		, create_garbage_callback = Player.create_garbage_callback
		, create_winner_callback = Player.create_winner_callback
		, create_loser_callback = Player.create_loser_callback
		}
	-- TODO: unregister these at an appropriate time
	memory.registerwrite(addrs.board, 2*BOARD_WIDTH, self:create_garbage_callback())
	memory.registerwrite(addrs.virus_count, 1, self:create_winner_callback())
	memory.registerwrite(addrs.pill_x, 1, self:create_loser_callback())
	return self
end

function Player.update(self)
	local sequence = memory.readbyte(self.addrs.pill_sequence_counter)
	local drop = memory.readbyte(self.addrs.pill_drop_counter)
	local fine = memory.readbyte(self.addrs.fine_speed)

	if self.mode == PLAYER_MODE.cleanup then
		if self.old_sequence ~= sequence then
			self.mode = PLAYER_MODE.control
			self.seen_nonzero_drop = false
			o('mode ' .. self.id .. ' control ' .. self:lookahead())
			self:send_pill(memory.readbyte(self.addrs.pill_x)
			              ,memory.readbyte(self.addrs.pill_y)
			              ,self:lookcurrent()
			              )
		end
	elseif self.mode == PLAYER_MODE.control then
		self.seen_nonzero_drop = self.seen_nonzero_drop or (drop ~= 0)

		-- Yuck. Okay, this deserves some explanation.
		--
		-- When a pill locks (and cleanup mode begins), its drop counter isn't
		-- changed until control mode begins again. And under normal
		-- circumstances, the drop counter changes every frame, either by
		-- decrementing by one or being reset to its max if it's already zero.
		-- So the plan for detecting when cleanup mode begins is to watch for
		-- the drop counter to stay the same for two frames in a row.
		--
		-- Except there's one wrinkle, which is that the way pressing the down
		-- arrow appears to work is that it sets the drop counter to 0. This
		-- means you can see the drop counter be 0 two frames in a row if the
		-- counter naturally drops to 0 by the decrement-every-frame mechanic,
		-- gets reset to max briefly on the next frame, then the down-arrow
		-- handling gets invoked and resets it to 0 again. So the new plan is
		-- to watch for *three* frames in a row of unchanged drop counter.
		--
		-- And now you ask: can't we see three frames in a row of 0s if the
		-- down-arrow handler gets invoked two frames in a row after a natural
		-- auto-decrement-to-zero? Nope: the game only appears to check for
		-- down arrows every other frame. Yeesh.
		--
		-- Oh yeah, and the seen_nonzero_drop stuff is a workaround for the
		-- fact that we actually transition to control mode a little bit too
		-- early.
		if self.seen_nonzero_drop and self.old_old_drop == self.old_drop and self.old_drop == drop then
			self.mode = PLAYER_MODE.cleanup
			if playing then
				o('mode ' .. self.id .. ' cleanup')
			end
		elseif playing then
			local pill = self:lookcurrent()
			local x = memory.readbyte(self.addrs.pill_x)
			local y = memory.readbyte(self.addrs.pill_y)
			if pill ~= self.old_pill or x ~= self.old_x or y ~= self.old_y then
				self:send_pill(x, y, pill)
			end
		end
	elseif self.mode == PLAYER_MODE.virus_placement then
		local viruses = bcd_decode(memory.readbyte(self.addrs.virus_count))
		local max_viruses = max_virus_count(memory.readbyte(self.addrs.level))
		self.seen_small_virus_count = self.seen_small_virus_count or viruses < max_viruses
		if self.seen_small_virus_count and viruses >= max_viruses then
			self.mode = PLAYER_MODE.cleanup
			self:send_state()
		end
	end

	if self.old_fine ~= fine then
		o('speed ' .. self.id .. ' ' .. Player.compute_speed(fine, self.coarse))
	end

	-- TODO: Can the garbage writes ever span across frames? It wouldn't be
	-- nice to report half the garbage on one frame and half on the next.
	if self.garbage_valid and next(self.garbage) ~= nil then
		local columns, cells = '', ''
		for column, cell in pairs(self.garbage) do
			columns, cells = columns .. column, cells .. cell
		end
		o(table.concat({'garbage', self.id, columns, cells}, ' '))
	end

	self.garbage = {}
	self.garbage_valid = true
	self.old_sequence = sequence
	self.old_old_drop = self.old_drop
	self.old_drop = drop
	self.old_fine = fine
end

-- N.B. not part of new players, and does not take self!
function Player.compute_speed(fine, coarse)
	local speed_ix = memory.readbyte(ADDR.speed_bonus_table + coarse) + fine
	return memory.readbyte(ADDR.frames_per_row_table + speed_ix) + 1
end

local function cell_memory_byte_to_protocol(byte)
	local memory_bot_nibble = byte % 16
	local memory_top_nibble = (byte - memory_bot_nibble) / 16
	local protocol_color = CELL_BOTTOM_NIBBLE_MAP[memory_bot_nibble]
	local protocol_shape =    CELL_TOP_NIBBLE_MAP[memory_top_nibble]
	if protocol_shape and protocol_color then
		return string.char(CELL_BASE_OFFSET + protocol_color + protocol_shape)
	else
		return 'd'
	end
end

local function cell_memory_char_to_protocol(char)
	return cell_memory_byte_to_protocol(char:byte())
end

-- Where possible, we re-query emulator memory instead of using any state we've
-- stored in Lua variables. Since the state message is intended to
-- re-synchronize the state of the game with a potentially buggy client, we
-- want to be as sure as possible that we're sending it the current state of
-- the world and not what the potentially buggy server implementation thinks
-- the state of the world is.
function Player.send_state(self)
	if not playing then return end
	local fine   = memory.readbyte(self.addrs.fine_speed)
	local coarse = memory.readbyte(self.addrs.coarse_speed)
	local speed = Player.compute_speed(fine, coarse)
	local lookahead_pill = self:lookahead()
	local board = memory.readbyterange(self.addrs.board, BOARD_SIZE):gsub('.', cell_memory_char_to_protocol)
	local prefix = table.concat({'state', self.id, speed, lookahead_pill, board, ''}, ' ')
	if self.mode == PLAYER_MODE.cleanup then
		o(prefix .. 'cleanup')
	elseif self.mode == PLAYER_MODE.control then
		local drop = speed - memory.readbyte(self.addrs.pill_drop_counter)
		local current_pill = self:lookcurrent()
		local x = memory.readbyte(self.addrs.pill_x)
		local y = memory.readbyte(self.addrs.pill_y)
		o(table.concat({prefix .. 'control', drop, x, y, current_pill}, ' '))
	end
end

function Player.lookahead(self)
	local ix = (memory.readbyte(self.addrs.pill_sequence_counter)-1) % 128
	return PILL_LOOKAHEAD[memory.readbyte(ADDR.pill_sequence+ix)]
end

-- doesn't make any promises to return something sensible when the given player
-- is not in control mode
function Player.lookcurrent(self)
	local orientation = memory.readbyte(self.addrs.pill_orientation)
	local colors = memory.readbyterange(self.addrs.pill_colors, 2)
	local color1 = CELL_BOTTOM_NIBBLE_MAP[colors:byte(1)]
	local color2 = CELL_BOTTOM_NIBBLE_MAP[colors:byte(2)]
	local shape1 = 16
	local shape2 = 20
	if orientation % 2 == 1 then
		shape1, shape2 = 8, 12
	end
	if orientation >= 2 then
		color1, color2 = color2, color1
	end
	return string.char(CELL_BASE_OFFSET + shape1 + color1, CELL_BASE_OFFSET + shape2 + color2)
end

function Player.send_pill(self, x, y, pill)
	o(table.concat({'pill', self.id, x, y, pill}, ' '))
	self.old_x = x
	self.old_y = y
	self.old_pill = pill
end

-- There are a couple interesting ways to see writes to the top row.
-- One is garbage. Another is a pill landing there. Another is a clear
-- happening there.
--
-- So one heuristic for finding garbage is:
-- 1. There was a write of a disconnected cell in the top row.
-- 2. There was no write indicating a clear in the top row (hence that
-- the disconnected cell write was the result of disconnecting a
-- horizontal pill).
-- 3. There was no write indicating a clear in the second row (hence
-- that the disconnected cell write was the result of disconnecting a
-- vertical pill).
--
-- That heuristic should be sufficient, but just for fun, we strengthen (3) to
-- be simply "There was no write in the second row.".
--
-- TODO: Double-check that this heuristic works correctly when placing a
-- vertical pill in the top row (hence disconnecting and discarding the
-- off-the-screen half of the pill).
function Player.create_garbage_callback(self)
	return function(addr, size)
		if size ~= 1 then
			print('WARNING: Saw unexpected write size of ' .. size .. ' for address ' .. addr)
			print('         in garbage callback. Weird! The code probably needs to be adapted')
			print('         to handle this possibility.')
		end
		local byte = memory.readbyte(addr)
		local shape_nibble = (byte - byte % 16) / 16
		local column = addr - self.addrs.board
		self.garbage_valid = self.garbage_valid
		                 and shape_nibble == 8
		                 and column < BOARD_WIDTH
		if byte ~= CELL_EMPTY then
			self.garbage[column] = cell_memory_byte_to_protocol(byte)
		end
	end
end

function Player.create_winner_callback(self)
	return function(addr, size)
		if memory.readbyte(addr) == 0 and self.mode == PLAYER_MODE.cleanup and playing then
			o('winner ' .. self.id)
			self.mode = PLAYER_MODE.won
			playing = false
		end
	end
end

-- TODO: This is totally incorrect. The pill x and y coordinates get reset to
-- 3,15 before cleanup is finished, so this will frequently incorrectly declare
-- a loser: whenever 3,15 or 4,15 are occupied at pill lock time but a match
-- causes them to be emptied.
function Player.create_loser_callback(self)
	return function(addr, size)
		if self.mode == PLAYER_MODE.cleanup and playing and
		   memory.readbyterange(self.addrs.board + 3, 2) ~= CELL_TWO_EMPTIES then
			o('loser ' .. self.id)
			self.mode = PLAYER_MODE.lost
			playing = false
		end
	end
end

local old_board_mode = 0
local player_count = 0
playing = false
local players = {}

local function send_messages(before)
	local board_mode = memory.readbyte(ADDR.board_mode)
	if board_mode ~= old_board_mode then
		old_board_mode = board_mode
		if board_mode == BOARD_MODE.prep then
			player_count = memory.readbyte(ADDR.players)
			playing = true
			if player_count == 1 then
				players = { Player.new(PLAYER_1_ADDRS, 'you') }
			elseif player_count == 2 then
				players = { Player.new(PLAYER_1_ADDRS, 'opponent')
				          , Player.new(PLAYER_2_ADDRS, 'you')
				          }
			else
				print('Wow! Player count is not 1 or 2, but ' .. player_count .. '. Everything will probably break shortly.')
			end
		end
	end

	if before and (board_mode == BOARD_MODE.game or board_mode == BOARD_MODE.prep) then
		local placing_viruses = false
		for _, player in ipairs(players) do
			placing_viruses = placing_viruses or player.mode == PLAYER_MODE.virus_placement
		end
		if playing and not placing_viruses then
			o('frame ' .. emu.framecount())
		end

		for _, player in ipairs(players) do
			player:update()
		end
	end
end

local function receive_messages(before)
	local message = i()
	while message ~= nil do
		if message == 'request-state' then
			for _, player in ipairs(players) do
				player:send_state()
			end
		else
			print('WARNING: Ignoring an unimplemented message type.')
			print('         ' .. message)
		end
		message = i()
	end
end

local function handle_frame(before)
	send_messages(before)
	receive_messages(before)
end

print('Negotiating version...')
o('propose-version ' .. PROTOCOL_VERSION)
o('request-version')
local reply = i()
-- Polling isn't the greatest. Perhaps we can temporarily turn the pipe into a
-- blocking one again or something?
while nil == reply do reply = i() end
if 'version ' .. PROTOCOL_VERSION ~= reply then
	print('WARNING: The client is non-conforming. The protocol requires that it reply with')
	print('         "version ' .. PROTOCOL_VERSION .. '" at this point, but instead it said:')
	print('         ' .. reply)
	print("         (I'll do my best to continue anyway, but expect some oddities.)")
else
	print('Negotiated version ' .. PROTOCOL_VERSION .. '.')
end

emu.registerbefore(function() handle_frame(true ) end)
emu.registerafter (function() handle_frame(false) end)
