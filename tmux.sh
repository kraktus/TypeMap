tmux new-session -s typemap -d
tmux split-window -d
tmux split-window -d
# tmux split-window -d
tmux send-keys -t typemap.1 "sbt" C-m
#tmux send-keys -t typemap.2 "pnpm run watch-format" C-m
# tmux send-keys -t lila.4 "cd ../lila-ws && gomph && sbt run" C-m
tmux attach-session -t typemap