function curly --description 'curl with custom useragent and resume download'
    set -l ua "Mozilla/5.0 (Macintosh; Intel Mac OS X 20_11) \
    AppleWebKit/638.34.48 (KHTML, like Gecko) \Version/12.0 Safari/638.35.8"
    curl -C - -A $ua -v $argv
end
