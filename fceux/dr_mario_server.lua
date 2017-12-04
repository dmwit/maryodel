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
