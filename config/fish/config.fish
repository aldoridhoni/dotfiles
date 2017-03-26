# For fish before 2.3.0
switch "$FISH_VERSION"
    case 2.2.0 2.1.2 2.1.1 2.1.0 2.0.0
        for file in ~/.config/fish/conf.d/*
            [ -f $file -a -r $file ]; and source $file
        end
end
