#!/usr/local/bin/relatively fishier
set PID /tmp/hypercorn.veritates.love.pid
if test -f $PID
  echo 'Killing existing ...'
  kill -- -(string trim (ps -o pgid= (cat $PID)))
  kill (cat $PID)
  echo $status
end

function remove_pid --on-signal SIGINT --on-event fish_exit
  rm -f $PID
end

hypercorn \
  --reload \
  --bind 0.0.0.0:7357 \
  --certfile cert/localhost.crt \
  --keyfile cert/localhost.key \
  --pid $PID \
  --websocket-ping-interval 20 \
  --graceful-timeout 5 \
  --worker-class uvloop \
  app
