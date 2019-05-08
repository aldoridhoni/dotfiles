function rec-ascii
    set -l now (date +%Y%m%d-%H%M)
    command asciinema rec -w 1 "asciinema-$now.json"
end
