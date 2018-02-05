function mkdir --description 'Make dir and change to it'
    command mkdir -pv $argv
    cd (echo $argv |command rev|cut -d' ' -f1|command rev)
end
