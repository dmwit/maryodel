local u = require('unix')

local PROTOCOL_VERSION = '0-statefix'

local function die(s)
	io.stderr:write(s)
	io.stderr:write('\n')
	os.exit()
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

local LMODE = { cleanup = 0, control = 1, lost = 2 }
local ADDR = { mode = 0x46, players = 0x727 }
local MODE = { prep = 8, game = 4 }
local PLAYER_1_ADDRS = { pill_sequence_counter = 0x0327, pill_drop_counter = 0x0312 }
local PLAYER_2_ADDRS = { pill_sequence_counter = 0x03A7, pill_drop_counter = 0x0392 }

local Player = {}

function Player.new(addrs, id)
	return { addrs = addrs, id = id
	       , logical_mode = LMODE.cleanup
	       , old_mode = memory.readbyte(ADDR.mode)
	       , old_sequence = memory.readbyte(addrs.pill_sequence_counter)
	       , old_drop = memory.readbyte(addrs.pill_drop_counter)
	       , seen_nonzero_drop = false
	       , update_logical_mode = Player.update_logical_mode
	       , send_state = Player.send_state
	       }
end

function Player.update_logical_mode(self)
	local sequence = memory.readbyte(self.addrs.pill_sequence_counter)
	local drop = memory.readbyte(self.addrs.pill_drop_counter)
	if self.logical_mode == LMODE.cleanup then
		local mode = memory.readbyte(ADDR.mode)
		if self.old_sequence ~= sequence then
			self.logical_mode = LMODE.control
			self.seen_nonzero_drop = false
			-- TODO: which pill are they getting?
			o('mode ' .. self.id .. ' control')
		end
		self.old_mode = mode
	elseif self.logical_mode == LMODE.control then
		self.seen_nonzero_drop = self.seen_nonzero_drop or (drop ~= 0)
		if self.seen_nonzero_drop and self.old_drop == drop then
			self.logical_mode = LMODE.cleanup
			o('mode ' .. self.id .. ' cleanup')
		end
	end
	self.old_sequence = sequence
	self.old_drop = drop
end

function Player.send_state(self)
	-- TODO
	o('state ' .. self.id)
end

local old_mode = 0
local player_count = 0
local players = {}

local function send_messages(before)
	local mode = memory.readbyte(ADDR.mode)
	if mode ~= old_mode then
		old_mode = mode
		if     mode == MODE.prep then
			player_count = memory.readbyte(ADDR.players)
			o('players ' .. player_count)
			if player_count == 1 then
				players = { Player.new(PLAYER_1_ADDRS, 'you') }
			elseif player_count == 2 then
				players = { Player.new(PLAYER_1_ADDRS, 'opponent')
				          , Player.new(PLAYER_2_ADDRS, 'you')
				          }
			else
				print('Wow! Player count is not 1 or 2, but ' .. player_count .. '. Everything will probably break shortly.')
			end
			for _, player in ipairs(players) do
				player:send_state()
			end
		end
	end

	if before and (mode == MODE.game or mode == MODE.prep) then
		for _, player in ipairs(players) do
			player:update_logical_mode()
		end
	end
end

local function send_messages_before() send_messages(true ) end
local function send_messages_after()  send_messages(false) end

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

emu.registerbefore(send_messages_before)
emu.registerafter(send_messages_after)
